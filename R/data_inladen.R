library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(janitor)


VERPLICHTE_KOLOMMEN <- c(
  "transactie_id", "datum", "ministerie", "categorie", "leverancier",
  "begroot_bedrag", "werkelijk_bedrag", "betaalstatus", "inkoopmethode",
  "factuurnummer", "omschrijving"
)

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


is_geldig_numeriek <- function(waarden) {
  !is.na(suppressWarnings(as.numeric(waarden)))
}

is_geldige_datum <- function(waarden) {
  !is.na(suppressWarnings(lubridate::ymd(waarden, quiet = TRUE)))
}

is_ontbrekend <- function(waarden) {
  is.na(waarden) | stringr::str_trim(waarden) == ""
}



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


bepaal_dimensies <- function(gegevens) {
  list(aantal_rijen = nrow(gegevens), aantal_kolommen = ncol(gegevens))
}


signaleer_ontbrekende_waarden <- function(gegevens) {
  aantallen <- vapply(gegevens, function(kolom) sum(is_ontbrekend(kolom)), integer(1))

  dplyr::tibble(
    kolom = names(aantallen),
    aantal_ontbrekend = as.integer(aantallen)
  ) |>
    dplyr::arrange(dplyr::desc(aantal_ontbrekend))
}


signaleer_dubbele_records <- function(gegevens) {
  is_dubbel <- duplicated(gegevens) | duplicated(gegevens, fromLast = TRUE)

  list(
    aantal_dubbele_records = sum(duplicated(gegevens)),
    dubbele_records = gegevens[is_dubbel, ]
  )
}


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


standaardpad <- file.path("data", "raw", "transacties.csv")
rapport <- voer_data_inleesproces_uit(standaardpad)
rapporteer_data_inleesproces(rapport)
