# staat1cho

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/staat1cho)](https://CRAN.R-project.org/package=staat1cho)
[![R-CMD-check](https://github.com/cedanl/staat-van-onderwijsinstelling/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cedanl/staat-van-onderwijsinstelling/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

R-package voor het berekenen van studie-indicatoren op basis van 1CHO-data van DUO. Oorspronkelijk ontwikkeld voor Avans Hogeschool door Veerle van Son en Damiëtte Bakx-van den Brink, doorontwikkeld door CEDA/Npuls.

Het package berekent vier indicatoren per instroomcohort: **instroom**, **rendement**, **uitval** en **studiewissel**. De resultaten zijn te bekijken via een interactief Shiny-dashboard of te verwerken via een R-pipeline.

---

## Installeren

Installeer de stabiele versie van CRAN:

```r
install.packages("staat1cho")
```

Of installeer de ontwikkelversie direct van GitHub:

```r
# install.packages("pak")
pak::pak("cedanl/staat-van-onderwijsinstelling")
```

---

## Gebruik

### Dashboard

Upload een 1CHO CSV-bestand en verken de indicatoren interactief:

```r
library(staat1cho)
start_dashboard()
```

Het dashboard opent in je browser. Je kunt:

- Een 1CHO-bestand uploaden en verwerken
- Filteren op jaar, locatie, sector, opleiding, opleidingsvorm en geslacht
- Trends bekijken voor instroom, rendement, uitval en studiewissel
- De verwerkte data downloaden als CSV

### Pipeline

Voor batch-verwerking is er een `pipeline.R` in de projectroot. Stel bovenaan het jaar en het type instelling in:

```r
jaar     <- 2025
soort_ho <- c("wetenschappelijk onderwijs", "wo")  # of "hoger beroepsonderwijs", "hbo"
```

De pipeline verwerkt instroom, rendement, uitval, studiewissel en combinatie in volgorde en slaat de tussenbestanden op in `Output/<jaar>/`.

### Losse functies

Je kunt de functies ook zelf samenstellen:

```r
library(staat1cho)

basis      <- maak_basisbestand("pad/naar/bestand.csv")
cohort     <- maak_instroom_cohort(basis, soort_ho = c("wetenschappelijk onderwijs", "wo"))
diploma    <- maak_diploma_behaald(basis)
rendement  <- bereken_rendement(cohort, diploma)
uitval     <- bereken_uitval(basis, diploma, cohort, jaar = 2025)
wissel     <- bereken_studiewissel(basis, cohort, diploma, uitval)
resultaat  <- combineer_indicatoren(cohort, rendement, uitval, wissel)
```

---

## Invoerdata

Het package verwacht de **enriched** output van de [1cijferho tool](https://github.com/cedanl/1cijferho): het CSV-bestand met `_enriched` in de naam, waarbij codes al zijn omgezet naar leesbare labels.

Het bestand moet onder andere deze kolommen bevatten:

| Kolom | Omschrijving |
|---|---|
| `persoonsgebonden_nummer` | Pseudonummer student |
| `inschrijvingsjaar` | Startjaar academisch jaar |
| `verblijfsjaar_actuele_instelling` | Jaar aan de instelling |
| `verblijfsjaar_actuele_opleiding_instelling` | Jaar in deze opleiding aan de instelling |
| `diplomajaar` | Academisch jaar van diploma |
| `soort_hoger_onderwijs` | Bijv. `"wetenschappelijk onderwijs"` |
| `geslacht`, `opleidingsvorm`, `opleiding_actueel_equivalent` | Kenmerken |
| `vestigingsnummer_gemeentenaam_volgens_rio` | Locatienaam |

Als een verplichte kolom ontbreekt, meldt het dashboard dit direct na het uploaden.

---

## Uitvoer

Per student worden de volgende indicatoren berekend:

| Categorie | Indicatoren |
|---|---|
| Studentkenmerken | instroomjaar, geslacht, locatie, sector, opleidingsvorm, leeftijd bij instroom |
| Status | status na observatieperiode, soort diploma |
| Rendement | diploma binnen 3, 5 en 8 jaar |
| Uitval | uitval binnen 1 en 3 jaar |
| Studiewissel | gewisseld binnen 1 en 3 jaar, opleiding/sector na wissel |

---

## Functies

| Functie | Wat het doet |
|---|---|
| `start_dashboard()` | Start het interactieve Shiny-dashboard |
| `maak_basisbestand()` | Laadt het 1CHO-bestand en voegt labelkolommen toe |
| `maak_instroom_cohort()` | Maakt cohortbestand aan (nieuwe instromers) |
| `maak_diploma_behaald()` | Bepaalt diplomaresultaten per student |
| `bereken_rendement()` | Rendement binnen 3, 5 en 8 jaar |
| `bereken_uitval()` | Uitvalstatus binnen 1 en 3 jaar |
| `bereken_studiewissel()` | Studiewissel binnen 1 en 3 jaar |
| `combineer_indicatoren()` | Voegt alle indicatoren samen tot analysebestand |

---

## Vereisten

- R >= 4.1.0
- Tidyverse-packages (`dplyr`, `ggplot2`, `readr`, `tidyr`, `forcats`, `scales`)
- Shiny-packages (`shiny`, `bslib`, `DT`, `plotly`)
