# Requirements – Rijksuitgaven Audit Dashboard

## 1. Doel

Ontwikkel een interactieve R Shiny-dashboardapplicatie waarmee fictieve overheidsuitgaven kunnen worden geanalyseerd op:

- totale uitgaven;
- begrotingsafwijkingen;
- uitgaven per ministerie;
- uitgaven per categorie;
- uitgaven per leverancier;
- mogelijke afwijkende transacties;
- dubbele facturen;
- ontbrekende of ongeldige gegevens;
- datakwaliteit.

De applicatie moet volledig werken met synthetische data.

------------------------------------------------------------------------

## 2. Technische eisen

De applicatie moet worden ontwikkeld met:

- R;
- RStudio;
- Shiny;
- dplyr;
- tidyr;
- readr;
- stringr;
- ggplot2;
- lubridate;
- janitor;
- DT.

Versiebeheer moet plaatsvinden met Git.

------------------------------------------------------------------------

## 3. Dataset

Gebruik een synthetische dataset met minimaal 5.000 transacties.

De dataset moet minimaal de volgende kolommen bevatten:

- `transactie_id`;
- `datum`;
- `ministerie`;
- `categorie`;
- `leverancier`;
- `begroot_bedrag`;
- `werkelijk_bedrag`;
- `betaalstatus`;
- `inkoopmethode`;
- `factuurnummer`;
- `omschrijving`.

De dataset moet bewust enkele datakwaliteitsproblemen bevatten, waaronder:

- ontbrekende waarden;
- dubbele factuurnummers;
- dubbele transacties;
- negatieve bedragen;
- uitzonderlijk hoge bedragen;
- inconsistente schrijfwijzen;
- ongeldige of ontbrekende datums;
- ontbrekende leveranciers;
- ontbrekende ministeries.

### 3.1 Datagenerator (`scripts/genereer_data.R`)

Dit script genereert de synthetische dataset uit dit hoofdstuk. Het maakt ±7.500 transacties aan en voegt bewust de datakwaliteitsproblemen uit dit hoofdstuk toe (ontbrekende waarden, dubbele facturen, dubbele transacties, negatieve en uitzonderlijk hoge bedragen, inconsistente schrijfwijzen van ministeries en ongeldige datums).

Uitvoeren vanaf de projectroot:

``` bash
Rscript scripts/genereer_data.R
```

Het resultaat wordt weggeschreven naar `data/raw/transacties.csv` (niet meegenomen in Git, omdat het reproduceerbaar is via dit script).

**Let op:** dit script is door AI gegenereerd als hulpmiddel om snel een bruikbare startdataset te hebben. Het is bedoeld als vertrekpunt, niet als eindresultaat — controleer en pas het gerust aan waar nodig.

------------------------------------------------------------------------

## 4. Data-inleesproces

De applicatie moet:

1.  de brondata inlezen vanuit een CSV-bestand;
2.  controleren of alle verplichte kolommen aanwezig zijn;
3.  de datatypes controleren;
4.  het aantal rijen en kolommen bepalen;
5.  ontbrekende waarden signaleren;
6.  dubbele records signaleren;
7.  ongeldige waarden signaleren.

------------------------------------------------------------------------

## 5. Data-opschoning

De data moet vóór analyse worden opgeschoond.

De opschoning moet minimaal bevatten:

- verwijderen van onnodige spaties;
- standaardiseren van tekstwaarden;
- converteren van datums naar een geldig datumformaat;
- converteren van bedragen naar numerieke waarden;
- herkennen van ontbrekende waarden;
- herkennen van dubbele transacties;
- herkennen van dubbele factuurnummers;
- controleren van negatieve bedragen;
- controleren van onbekende ministeries;
- controleren van onbekende categorieën.

De opgeschoonde data moet apart kunnen worden opgeslagen.

------------------------------------------------------------------------

## 6. Analysefuncties

Maak herbruikbare R-functies voor minimaal:

- totale uitgaven;
- totale begroting;
- budgetafwijking;
- budgetafwijking in procenten;
- uitgaven per ministerie;
- uitgaven per categorie;
- uitgaven per leverancier;
- maandelijkse uitgaven;
- aantal transacties;
- aantal risicosignalen;
- dubbele facturen;
- ontbrekende waarden;
- afwijkende transacties.

------------------------------------------------------------------------

## 7. Risicosignalen

De applicatie moet transacties kunnen markeren als risicosignaal.

Ondersteun minimaal de volgende risicotypen:

- uitzonderlijk hoge transactie;
- dubbele factuur;
- ontbrekende leverancier;
- ontbrekend ministerie;
- negatief bedrag;
- statistische afwijking.

Voor statistische afwijkingen moet minimaal één eenvoudige detectiemethode worden toegepast, bijvoorbeeld:

- IQR-methode; of
- gemiddelde plus/min drie standaarddeviaties.

Een risicosignaal mag alleen worden gepresenteerd als indicatie voor nader onderzoek, niet als bewijs van een fout.

------------------------------------------------------------------------

## 8. Shiny-dashboard

Het dashboard moet minimaal vijf onderdelen bevatten.

### 8.1 Overzicht

Toon minimaal:

- totale uitgaven;
- totale begroting;
- budgetafwijking;
- budgetafwijking in procenten;
- aantal transacties;
- aantal risicosignalen.

Toon daarnaast minimaal:

- staafdiagram met uitgaven per ministerie;
- lijndiagram met uitgaven per maand.

### 8.2 Begrotingsanalyse

Toon per ministerie minimaal:

- begroting;
- werkelijke uitgaven;
- absolute afwijking;
- procentuele afwijking.

Toon deze informatie zowel in tabelvorm als grafisch.

### 8.3 Risicosignalen

Toon een interactieve tabel met minimaal:

- transactie-id;
- datum;
- ministerie;
- leverancier;
- bedrag;
- risicotype;
- omschrijving.

De tabel moet filterbaar zijn.

### 8.4 Leveranciersanalyse

Toon minimaal:

- totale uitgaven per leverancier;
- top 10 leveranciers;
- aandeel van de grootste leveranciers in de totale uitgaven.

### 8.5 Datakwaliteit

Toon minimaal:

- aantal ontbrekende waarden;
- aantal dubbele facturen;
- aantal dubbele transacties;
- aantal negatieve bedragen;
- aantal ongeldige datums;
- aantal onbekende ministeries.

------------------------------------------------------------------------

## 9. Interactiviteit

Het dashboard moet minimaal de volgende filters bevatten:

- ministerie;
- categorie;
- leverancier;
- periode;
- minimum- en/of maximumbedrag;
- risicotype.

Grafieken, tabellen en KPI's moeten automatisch worden bijgewerkt wanneer de filters wijzigen.

------------------------------------------------------------------------

## 10. Visualisaties

Gebruik `ggplot2` voor de belangrijkste grafieken.

De applicatie moet minimaal bevatten:

- staafdiagram;
- lijndiagram;
- scatterplot of boxplot;
- visualisatie van begroting tegenover werkelijke uitgaven.

Grafieken moeten duidelijke:

- titels;
- aslabels;
- legenda's;
- eenheden bevatten.

------------------------------------------------------------------------

## 11. Tabellen

Gebruik een interactieve tabel voor transactiedetails.

De tabel moet minimaal ondersteunen:

- sorteren;
- zoeken;
- filteren;
- paginering.

------------------------------------------------------------------------

## 12. Codekwaliteit

De code moet worden opgesplitst in logische onderdelen.

Gebruik minimaal de volgende structuur:

``` text
rijksuitgaven-audit-dashboard/
├── app.R
├── R/
│   ├── data_inladen.R
│   ├── data_opschonen.R
│   ├── analyses.R
│   ├── risico_detectie.R
│   └── visualisaties.R
├── data/
│   └── raw/
├── data_processed/
├── scripts/
│   └── genereer_data.R
└── README.md
```

Vermijd waar mogelijk:

- dubbele code;
- hardcoded waarden;
- onnodig grote functies;
- analysecode rechtstreeks in de UI.

Gebruik duidelijke Nederlandse namen voor functies en variabelen.

------------------------------------------------------------------------

## 13. README

De repository moet een `README.md` bevatten met minimaal:

- projectdoel;
- businesscontext;
- gebruikte technologieën;
- beschrijving van de dataset;
- uitleg van de analyses;
- uitleg van de risicosignalen;
- projectstructuur;
- instructies om de applicatie lokaal te starten;
- screenshots van het dashboard;
- beperkingen;
- mogelijke toekomstige uitbreidingen.

------------------------------------------------------------------------

## 14. Git

Gebruik Git tijdens de ontwikkeling.

De repository moet meerdere betekenisvolle commits bevatten, bijvoorbeeld:

- `synthetische dataset toegevoegd`;
- `data opschoning geïmplementeerd`;
- `analysefuncties toegevoegd`;
- `ggplot visualisaties toegevoegd`;
- `shiny dashboard basis toegevoegd`;
- `risicosignalen toegevoegd`;
- `dashboard filters toegevoegd`.

------------------------------------------------------------------------

## 15. Eindresultaat

Het eindproduct moet bestaan uit:

1.  een werkende R Shiny-applicatie;
2.  een synthetische dataset;
3.  reproduceerbare data-opschoning;
4.  herbruikbare analysefuncties;
5.  interactieve visualisaties;
6.  risicosignalering;
7.  datakwaliteitsanalyse;
8.  een goed gestructureerde Git-repository;
9.  een duidelijke README.
