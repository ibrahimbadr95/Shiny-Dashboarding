# ============================================================
# Data-inleesproces
#
# Leest de brondata in vanuit een CSV-bestand en voert een
# eerste reeks controles uit: verplichte kolommen, datatypes,
# omvang van de dataset, ontbrekende waarden, dubbele records
# en ongeldige waarden. Dit gebeurt bewust vóór de opschoning
# (zie R/data_opschonen.R), zodat problemen in de brondata
# zichtbaar blijven in plaats van stilzwijgend te worden
# opgelost.
# ============================================================

library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(janitor)

# ============================================================
# Instellingen
# ============================================================

VERPLICHTE_KOLOMMEN <- c(
  "transactie_id", "datum", "ministerie", "categorie", "leverancier",
  "begroot_bedrag", "werkelijk_bedrag", "betaalstatus", "inkoopmethode",
  "factuurnummer", "omschrijving"
)

# Verwacht datatype per kolom. Wordt gebruikt door controleer_datatypes().
VERWACHTE_TYPES <- c(
  transactie_id    = "tekst",
  datum            = "datum",
  ministerie       = "tekst",
  categorie        = "tekst",
  leverancier      = "tekst",
  begroot_bedrag   = "numeriek",
  werkelijk_bedrag = "numeriek",
  betaalstatus     = "tekst",
  inkoopmethode    = "tekst",
  factuurnummer    = "tekst",
  omschrijving     = "tekst"
)

# ============================================================
# Hulpfuncties
# ============================================================

# Alle brondata wordt eerst als tekst ingelezen (zie lees_brondata()).
# Deze functies bepalen per waarde of die, ondanks dat, alsnog geldig
# is voor het verwachte datatype.

is_geldig_numeriek <- function(waarden) {
  !is.na(suppressWarnings(as.numeric(waarden)))
}

is_geldige_datum <- function(waarden) {
  !is.na(suppressWarnings(lubridate::ymd(waarden, quiet = TRUE)))
}

is_ontbrekend <- function(waarden) {
  is.na(waarden) | stringr::str_trim(waarden) == ""
}

# ============================================================
# 1. Brondata inlezen vanuit een CSV-bestand
# ============================================================

#' Leest het brondatabestand in als tekst.
#'
#' Alle kolommen worden als tekst ingelezen, zodat de controles in dit
#' bestand zelf kunnen bepalen welke waarden wel of niet geldig zijn
#' voor het verwachte datatype. Automatische typeconversie door readr
#' zou dit soort problemen (bv. "geen datum" of "31-02-2026") stil
#' laten verdwijnen in NA's, nog vóór ze gesignaleerd kunnen worden.
lees_brondata <- function(bestandspad) {
  if (!file.exists(bestandspad)) {
    stop(sprintf("Brondatabestand niet gevonden: %s", bestandspad))
  }

  gegevens <- readr::read_csv(
    bestandspad,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )

  janitor::clean_names(gegevens)
}

# ============================================================
# 2. Controleren of alle verplichte kolommen aanwezig zijn
# ============================================================

controleer_verplichte_kolommen <- function(gegevens, verplichte_kolommen = VERPLICHTE_KOLOMMEN) {
  aanwezige_kolommen <- names(gegevens)
  ontbrekende_kolommen <- setdiff(verplichte_kolommen, aanwezige_kolommen)
  onverwachte_kolommen <- setdiff(aanwezige_kolommen, verplichte_kolommen)

  list(
    alle_verplichte_kolommen_aanwezig = length(ontbrekende_kolommen) == 0,
    ontbrekende_kolommen = ontbrekende_kolommen,
    onverwachte_kolommen = onverwachte_kolommen
  )
}

# ============================================================
# 3. Datatypes controleren
# ============================================================

#' Controleert per verplichte kolom hoeveel waarden niet overeenkomen
#' met het verwachte datatype (zie VERWACHTE_TYPES).
#'
#' Voor tekstkolommen wordt alleen gecontroleerd of de kolom bestaat
#' (elke ingelezen waarde is per definitie tekst); ontbrekende tekst
#' wordt gesignaleerd door signaleer_ontbrekende_waarden().
controleer_datatypes <- function(gegevens, verwachte_types = VERWACHTE_TYPES) {
  gedeelde_kolommen <- intersect(names(verwachte_types), names(gegevens))

  resultaten <- lapply(gedeelde_kolommen, function(kolom) {
    verwacht_type <- verwachte_types[[kolom]]
    waarden <- gegevens[[kolom]]
    aanwezig <- !is_ontbrekend(waarden)

    aantal_ongeldig <- switch(verwacht_type,
      numeriek = sum(aanwezig & !is_geldig_numeriek(waarden)),
      datum    = sum(aanwezig & !is_geldige_datum(waarden)),
      0L
    )

    dplyr::tibble(
      kolom = kolom,
      verwacht_type = verwacht_type,
      aantal_ongeldig = aantal_ongeldig
    )
  })

  dplyr::bind_rows(resultaten)
}

# ============================================================
# 4. Aantal rijen en kolommen bepalen
# ============================================================

bepaal_dimensies <- function(gegevens) {
  list(aantal_rijen = nrow(gegevens), aantal_kolommen = ncol(gegevens))
}

# ============================================================
# 5. Ontbrekende waarden signaleren
# ============================================================

signaleer_ontbrekende_waarden <- function(gegevens) {
  aantallen <- vapply(gegevens, function(kolom) sum(is_ontbrekend(kolom)), integer(1))

  dplyr::tibble(
    kolom = names(aantallen),
    aantal_ontbrekend = as.integer(aantallen)
  ) |>
    dplyr::arrange(dplyr::desc(aantal_ontbrekend))
}

# ============================================================
# 6. Dubbele records signaleren
# ============================================================

#' Signaleert volledig identieke rijen in de ingelezen brondata.
#'
#' Dit betreft duplicatie op recordniveau (alle kolommen identiek),
#' zoals die kan ontstaan bij een fout in de brondata-aanlevering.
#' Specifiekere signalen zoals dubbele factuurnummers horen bij de
#' risicodetectie (zie R/risico_detectie.R), niet bij het inlezen.
signaleer_dubbele_records <- function(gegevens) {
  is_dubbel <- duplicated(gegevens) | duplicated(gegevens, fromLast = TRUE)

  list(
    aantal_dubbele_records = sum(duplicated(gegevens)),
    dubbele_records = gegevens[is_dubbel, ]
  )
}

# ============================================================
# 7. Ongeldige waarden signaleren
# ============================================================

#' Signaleert waarden die aanwezig zijn, maar ongeldig voor hun kolom:
#' niet-numerieke of negatieve bedragen, en niet-herkenbare datums.
#' Ontbrekende waarden worden hier bewust niet meegeteld; die worden
#' al gerapporteerd door signaleer_ontbrekende_waarden().
signaleer_ongeldige_waarden <- function(gegevens) {
  datum_aanwezig <- !is_ontbrekend(gegevens$datum)
  begroot_numeriek <- suppressWarnings(as.numeric(gegevens$begroot_bedrag))
  werkelijk_numeriek <- suppressWarnings(as.numeric(gegevens$werkelijk_bedrag))

  dplyr::tibble(
    type_signaal = c(
      "ongeldige datum",
      "niet-numeriek begroot_bedrag",
      "niet-numeriek werkelijk_bedrag",
      "negatief begroot_bedrag",
      "negatief werkelijk_bedrag"
    ),
    aantal = c(
      sum(datum_aanwezig & !is_geldige_datum(gegevens$datum)),
      sum(!is_ontbrekend(gegevens$begroot_bedrag) & is.na(begroot_numeriek)),
      sum(!is_ontbrekend(gegevens$werkelijk_bedrag) & is.na(werkelijk_numeriek)),
      sum(!is.na(begroot_numeriek) & begroot_numeriek < 0),
      sum(!is.na(werkelijk_numeriek) & werkelijk_numeriek < 0)
    )
  )
}

# ============================================================
# Samenvattend: volledig data-inleesproces
# ============================================================

#' Voert het volledige data-inleesproces uit (stappen 1 t/m 7) en geeft
#' zowel de ingelezen data als alle controleresultaten terug.
voer_data_inleesproces_uit <- function(bestandspad) {
  gegevens <- lees_brondata(bestandspad)

  list(
    gegevens = gegevens,
    kolomcontrole = controleer_verplichte_kolommen(gegevens),
    dimensies = bepaal_dimensies(gegevens),
    datatypecontrole = controleer_datatypes(gegevens),
    ontbrekende_waarden = signaleer_ontbrekende_waarden(gegevens),
    dubbele_records = signaleer_dubbele_records(gegevens),
    ongeldige_waarden = signaleer_ongeldige_waarden(gegevens)
  )
}

#' Drukt een leesbare samenvatting van voer_data_inleesproces_uit() af.
rapporteer_data_inleesproces <- function(rapport) {
  cat("=== Data-inleesproces ===\n")
  cat("Rijen:", rapport$dimensies$aantal_rijen, "\n")
  cat("Kolommen:", rapport$dimensies$aantal_kolommen, "\n\n")

  cat("Verplichte kolommen aanwezig:", rapport$kolomcontrole$alle_verplichte_kolommen_aanwezig, "\n")
  if (length(rapport$kolomcontrole$ontbrekende_kolommen) > 0) {
    cat("Ontbrekende kolommen:", paste(rapport$kolomcontrole$ontbrekende_kolommen, collapse = ", "), "\n")
  }
  cat("\n")

  cat("Datatypecontrole (aantal ongeldige waarden per kolom):\n")
  print(rapport$datatypecontrole)
  cat("\n")

  cat("Ontbrekende waarden per kolom:\n")
  print(rapport$ontbrekende_waarden)
  cat("\n")

  cat("Dubbele records:", rapport$dubbele_records$aantal_dubbele_records, "\n\n")

  cat("Ongeldige waarden:\n")
  print(rapport$ongeldige_waarden)
}

# ============================================================
# Handmatig testen: Rscript R/data_inladen.R
# ============================================================

if (!interactive() && sys.nframe() == 0) {
  standaardpad <- file.path("data", "raw", "transacties.csv")
  rapport <- voer_data_inleesproces_uit(standaardpad)
  rapporteer_data_inleesproces(rapport)
}
