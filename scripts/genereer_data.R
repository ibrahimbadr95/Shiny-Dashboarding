# ============================================================
# Genereert een synthetische, bewust "vervuilde" dataset van
# fictieve rijksuitgaven voor het audit-dashboard.
# ============================================================

library(dplyr)
library(readr)
library(stringr)
library(lubridate)

# ============================================================
# Instellingen
# ============================================================

AANTAL_TRANSACTIES <- 7500
ZAAD <- 42

MINISTERIES <- c(
  "Ministerie van Financiën",
  "Ministerie van Binnenlandse Zaken en Koninkrijksrelaties",
  "Ministerie van Economische Zaken",
  "Ministerie van Sociale Zaken en Werkgelegenheid",
  "Ministerie van Infrastructuur en Waterstaat"
)
MINISTERIE_KANSEN <- c(0.20, 0.20, 0.18, 0.22, 0.20)

CATEGORIEEN <- c(
  "ICT", "Personeel", "Huisvesting", "Advies",
  "Inkoop", "Onderzoek", "Reizen"
)
CATEGORIE_KANSEN <- c(0.23, 0.18, 0.12, 0.15, 0.14, 0.10, 0.08)

LEVERANCIERS <- c(
  "DataSoft BV", "GovTech Nederland", "Delta Consulting",
  "Publieke Diensten BV", "Infra Solutions", "Insight Partners",
  "Nationale Services", "CloudWorks BV", "SecureIT Nederland",
  "Onderzoek & Advies BV"
)

BETAALSTATUSSEN <- c("Betaald", "Openstaand", "Geannuleerd")
BETAALSTATUS_KANSEN <- c(0.82, 0.15, 0.03)

INKOOPMETHODEN <- c(
  "Aanbesteding", "Meervoudig onderhands",
  "Enkelvoudig onderhands", "Raamovereenkomst"
)
INKOOPMETHODE_KANSEN <- c(0.35, 0.25, 0.20, 0.20)

OMSCHRIJVINGEN <- list(
  ICT = c(
    "Softwarelicenties", "Cloudhosting", "Applicatiebeheer",
    "Hardware en werkplekken", "Cybersecuritydiensten"
  ),
  Personeel = c(
    "Externe inhuur", "Opleiding en training",
    "Tijdelijke ondersteuning", "Personeelsadvies"
  ),
  Huisvesting = c(
    "Kantoorhuur", "Onderhoud gebouw",
    "Facilitaire diensten", "Energievoorziening"
  ),
  Advies = c(
    "Strategisch advies", "Juridisch advies",
    "Procesadvies", "Organisatieadvies"
  ),
  Inkoop = c(
    "Kantoorartikelen", "Materiaalinkoop",
    "Dienstverlening", "Algemene inkoop"
  ),
  Onderzoek = c(
    "Dataonderzoek", "Beleidsonderzoek",
    "Evaluatieonderzoek", "Marktonderzoek"
  ),
  Reizen = c(
    "Dienstreis", "Treinkosten",
    "Hotelkosten", "Internationale reis"
  )
)

# ============================================================
# Hulpfuncties
# ============================================================

maak_factuurnummer <- function(index) {
  sprintf("INV-2026-%06d", index)
}

kies_omschrijving <- function(categorie) {
  vapply(categorie, function(cat) sample(OMSCHRIJVINGEN[[cat]], 1), character(1))
}

voeg_ontbrekende_waarden_toe <- function(gegevens, kolom, aandeel) {
  aantal <- max(1, round(nrow(gegevens) * aandeel))
  indexen <- sample(nrow(gegevens), aantal)
  gegevens[[kolom]][indexen] <- NA
  gegevens
}

voeg_inconsistente_ministeries_toe <- function(gegevens) {
  aantal <- max(1, round(nrow(gegevens) * 0.01))
  indexen <- sample(nrow(gegevens), aantal)

  varianten <- c(
    "ministerie van financiën",
    "Ministerie van Financien ",
    " MINISTERIE VAN FINANCIËN",
    "Ministerie van Economische zaken",
    "Ministerie van Sociale Zaken en werkgelegenheid "
  )

  gegevens$ministerie[indexen] <- sample(varianten, aantal, replace = TRUE)
  gegevens
}

voeg_negatieve_bedragen_toe <- function(gegevens) {
  aantal <- max(1, round(nrow(gegevens) * 0.005))
  indexen <- sample(nrow(gegevens), aantal)

  gegevens$werkelijk_bedrag[indexen] <- -abs(gegevens$werkelijk_bedrag[indexen])
  gegevens
}

voeg_extreme_bedragen_toe <- function(gegevens) {
  aantal <- max(5, round(nrow(gegevens) * 0.003))
  indexen <- sample(nrow(gegevens), aantal)

  factoren <- runif(aantal, min = 8, max = 25)
  gegevens$werkelijk_bedrag[indexen] <- round(
    gegevens$werkelijk_bedrag[indexen] * factoren, 2
  )
  gegevens
}

voeg_dubbele_facturen_toe <- function(gegevens) {
  aantal <- max(10, round(nrow(gegevens) * 0.006))
  bron_indexen <- sample(nrow(gegevens), aantal)
  overige <- setdiff(seq_len(nrow(gegevens)), bron_indexen)
  doel_indexen <- sample(overige, aantal)

  gegevens$factuurnummer[doel_indexen] <- gegevens$factuurnummer[bron_indexen]
  gegevens
}

voeg_dubbele_transacties_toe <- function(gegevens) {
  aantal <- max(5, round(nrow(gegevens) * 0.003))
  indexen <- sample(nrow(gegevens), aantal)

  bind_rows(gegevens, gegevens[indexen, ])
}

voeg_ongeldige_datums_toe <- function(gegevens) {
  aantal <- max(5, round(nrow(gegevens) * 0.003))
  indexen <- sample(nrow(gegevens), aantal)

  ongeldige_datums <- c("31-02-2026", "2026-13-01", "geen datum", "")

  gegevens$datum[indexen] <- sample(ongeldige_datums, aantal, replace = TRUE)
  gegevens
}

# ============================================================
# Dataset genereren
# ============================================================

genereer_dataset <- function(aantal_transacties = AANTAL_TRANSACTIES, zaad = ZAAD) {
  set.seed(zaad)

  startdatum <- as.Date("2026-01-01")
  einddatum <- as.Date("2026-12-31")
  aantal_dagen <- as.integer(einddatum - startdatum)

  datums <- startdatum + sample(0:aantal_dagen, aantal_transacties, replace = TRUE)

  ministeries <- sample(
    MINISTERIES, aantal_transacties,
    replace = TRUE, prob = MINISTERIE_KANSEN
  )

  categorieen <- sample(
    CATEGORIEEN, aantal_transacties,
    replace = TRUE, prob = CATEGORIE_KANSEN
  )

  leveranciers <- sample(LEVERANCIERS, aantal_transacties, replace = TRUE)

  # Lognormale verdeling geeft veel normale bedragen en enkele grotere bedragen.
  werkelijk_bedrag <- rlnorm(aantal_transacties, meanlog = 8.3, sdlog = 0.9)
  werkelijk_bedrag <- round(pmin(pmax(werkelijk_bedrag, 100), 350000), 2)

  # Begroting ligt meestal redelijk dicht bij het werkelijke bedrag.
  afwijkingsfactor <- rnorm(aantal_transacties, mean = 1.0, sd = 0.15)
  afwijkingsfactor <- pmin(pmax(afwijkingsfactor, 0.6), 1.5)
  begroot_bedrag <- round(werkelijk_bedrag / afwijkingsfactor, 2)

  betaalstatussen <- sample(
    BETAALSTATUSSEN, aantal_transacties,
    replace = TRUE, prob = BETAALSTATUS_KANSEN
  )

  inkoopmethoden <- sample(
    INKOOPMETHODEN, aantal_transacties,
    replace = TRUE, prob = INKOOPMETHODE_KANSEN
  )

  omschrijvingen <- kies_omschrijving(categorieen)

  gegevens <- tibble(
    transactie_id = sprintf("TRX-%06d", seq_len(aantal_transacties)),
    datum = format(datums, "%Y-%m-%d"),
    ministerie = ministeries,
    categorie = categorieen,
    leverancier = leveranciers,
    begroot_bedrag = begroot_bedrag,
    werkelijk_bedrag = werkelijk_bedrag,
    betaalstatus = betaalstatussen,
    inkoopmethode = inkoopmethoden,
    factuurnummer = maak_factuurnummer(seq_len(aantal_transacties)),
    omschrijving = omschrijvingen
  )

  # --------------------------------------------------------
  # Bewust datakwaliteitsproblemen toevoegen
  # --------------------------------------------------------

  gegevens <- voeg_ontbrekende_waarden_toe(gegevens, "leverancier", aandeel = 0.008)
  gegevens <- voeg_ontbrekende_waarden_toe(gegevens, "ministerie", aandeel = 0.004)
  gegevens <- voeg_ontbrekende_waarden_toe(gegevens, "categorie", aandeel = 0.003)

  gegevens <- voeg_inconsistente_ministeries_toe(gegevens)
  gegevens <- voeg_negatieve_bedragen_toe(gegevens)
  gegevens <- voeg_extreme_bedragen_toe(gegevens)
  gegevens <- voeg_dubbele_facturen_toe(gegevens)
  gegevens <- voeg_ongeldige_datums_toe(gegevens)

  gegevens <- voeg_dubbele_transacties_toe(gegevens)

  # Willekeurige volgorde zodat duplicaten niet direct naast elkaar staan.
  gegevens[sample(nrow(gegevens)), ]
}

# ============================================================
# Opslaan
# ============================================================

vind_projectmap <- function() {
  bestandsargument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(bestandsargument) > 0) {
    scriptpad <- normalizePath(sub("^--file=", "", bestandsargument[1]))
    return(dirname(dirname(scriptpad)))
  }
  normalizePath(getwd())
}

sla_dataset_op <- function(gegevens) {
  projectmap <- vind_projectmap()
  uitvoermap <- file.path(projectmap, "data", "raw")
  dir.create(uitvoermap, recursive = TRUE, showWarnings = FALSE)

  uitvoerbestand <- file.path(uitvoermap, "transacties.csv")
  write_csv(gegevens, uitvoerbestand)

  uitvoerbestand
}

# ============================================================
# Uitvoeren
# ============================================================

if (!interactive()) {
  gegevens <- genereer_dataset()
  uitvoerbestand <- sla_dataset_op(gegevens)

  cat("Dataset succesvol aangemaakt.\n")
  cat("Bestand:", uitvoerbestand, "\n")
  cat("Aantal rijen:", format(nrow(gegevens), big.mark = ","), "\n")
  cat("Aantal kolommen:", ncol(gegevens), "\n")
}
