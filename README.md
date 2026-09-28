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

Installeer de ontwikkelversie direct van GitHub:

```r
# install.packages("pak")
pak::pak("cedanl/staat-van-onderwijsinstelling")
```

> **Let op:** CRAN heeft nog versie 0.1.0 (april 2026), zonder VAKHAVW, bekostiging, benchmarkrapport en de correctie voor onvolledige cohorten. Gebruik tot versie 0.2.0 op CRAN staat de GitHub-versie hierboven.

Voor het dashboard zijn extra packages nodig (`bslib`, `DT`, `ggplot2`, `plotly`, `scales`, `tidyr`, `writexl`). `start_dashboard()` vraagt erom als ze ontbreken.

---

## Gebruik

### Dashboard

Upload een 1CHO CSV-bestand en verken de indicatoren interactief:

```r
library(staat1cho)
start_dashboard()
```

Het dashboard opent in je browser. Je kunt:

- Een 1CHO-bestand uploaden, optioneel met VAKHAVW- en VLPBEK-bestand
- Kiezen tussen studentniveau en inschrijvingsniveau
- Filteren op jaar, locatie, sector, opleiding, opleidingsniveau, opleidingsvorm en geslacht
- Trends bekijken voor instroom, rendement, uitval, studiewissel, vooropleiding en bekostiging
- De verwerkte data downloaden als CSV en een geanonimiseerd benchmarkrapport als Excel

Geen eigen data bij de hand? Maak een synthetisch voorbeeldbestand en upload dat:

```r
readr::write_delim(maak_synthetische_1cho(), "demo_1cho.csv", delim = ";", na = "")
```

### Pipeline

Voor batch-verwerking is er een `pipeline.R` in de projectroot. Stel bovenaan het pad naar je 1CHO-bestand, het analyseniveau en eventueel VAKHAVW/VLPBEK in:

```r
pad_1cho    <- "pad/naar/EV..._enriched.csv"  # leeg = synthetische demodata
niveau      <- "student"                       # of "inschrijving"
vakhawv_pad <- ""
vlpbek_pad  <- ""
```

Het peiljaar en het soort hoger onderwijs worden uit de data afgeleid.

De pipeline slaat de tussenbestanden en het benchmarkrapport op in `Output/<jaar>/`.

### Welke 1cijferho-uitvoer gebruik je?

1cijferho kan de persoonsnummers uitleveren met het **BSN behouden**, **omgezet naar studentnummer** of **gepseudonimiseerd**. Wat je kiest bepaalt wat je kunt koppelen:

| 1cijferho-uitvoer | Indicatoren | VAKHAVW | VLPBEK (bekostiging) |
|---|---|---|---|
| BSN behouden | ja | ja | **ja** |
| Studentnummer | ja | ja, als EV en VAKHAVW dezelfde uitvoer zijn | nee |
| Gepseudonimiseerd | ja | ja, als EV en VAKHAVW dezelfde uitvoer zijn | nee |

Het VLPBEK-bestand komt rechtstreeks van DUO en bevat altijd het echte BSN of onderwijsnummer. **Wil je bekostiging koppelen, gebruik dan de uitvoer met het BSN behouden.** staat1cho pseudonimiseert of vertaalt zelf geen nummers. Koppel je toch een gepseudonimiseerd bestand, dan stopt staat1cho met een foutmelding; bij studentnummers volgt een waarschuwing over het lage koppelpercentage.

Het benchmarkrapport bevat nooit persoonsnummers en is geschikt om te delen. De volledige CSV-download uit het dashboard bevat ze wel: die blijft binnen de instelling.

### Losse functies

Je kunt de functies ook zelf samenstellen:

```r
library(staat1cho)

basis      <- maak_basisbestand("pad/naar/bestand.csv")
cohort     <- maak_instroom_cohort(basis, soort_ho = c("wetenschappelijk onderwijs", "wo"))
diploma    <- maak_diploma_behaald(basis)
rendement  <- bereken_rendement(cohort, diploma)
uitval     <- bereken_uitval(basis, diploma, cohort)  # peiljaar uit de data
wissel     <- bereken_studiewissel(basis, cohort, diploma, uitval)
resultaat  <- combineer_indicatoren(cohort, rendement, uitval, wissel)
rapport    <- maak_benchmarkrapport(resultaat)
schrijf_benchmarkrapport(rapport, "benchmark.xlsx")
```

Zie `vignette("staat1cho")` voor een uitgewerkt voorbeeld.

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

Per student (of per inschrijving) worden de volgende indicatoren berekend:

| Categorie | Indicatoren |
|---|---|
| Studentkenmerken | instroomjaar, geslacht, locatie, sector, opleidingsvorm, leeftijd bij instroom |
| Status | status na observatieperiode, soort diploma |
| Rendement | diploma binnen 3, 5 en 8 jaar |
| Uitval | uitval binnen 1 en 3 jaar |
| Studiewissel | gewisseld binnen 1 en 3 jaar, opleiding/sector na wissel (alleen studentniveau) |
| Vooropleiding (optioneel) | eindcijfer, wiskundecijfer en aantal vakken uit VAKHAVW |
| Bekostiging (optioneel) | bekostigd, reden niet bekostigd, herstelbaar uit VLPBEK |

### Onvolledige cohorten

Rendement binnen 5 jaar is voor een cohort dat pas 2 jaar in de data zit nog niet te meten. Zulke studenten krijgen `"Nog niet waarneembaar"` en tellen niet mee in percentages. Recente cohorten hebben daardoor lege waarden voor de langere termijnen; dat is verwacht.

### Studentniveau of inschrijvingsniveau

Op **studentniveau** telt elke student één keer, bij de opleiding waarin die instroomt, en gelden uitkomsten voor de hele instelling: een wisselaar die elders een diploma haalt, telt bij de instroomopleiding als geslaagd. Op **inschrijvingsniveau** is elke opleiding een eigen cohort en telt een wissel als uitval uit de oude opleiding. Gebruik inschrijvingsniveau om opleidingen te vergelijken.

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
| `lees_vakhawv()` / `verrijk_met_vakhawv()` | Leest VAKHAVW-vakcijfers en koppelt ze per student |
| `lees_bekostiging()` / `verrijk_met_bekostiging()` | Leest een VLPBEK-bestand en koppelt de bekostigingsstatus (vereist 1cijferho-uitvoer met BSN) |
| `is_gepseudonimiseerd()` | Herkent door 1cijferho gepseudonimiseerde persoonsnummers |
| `maak_benchmarkrapport()` | Geaggregeerd rapport met privacyonderdrukking |
| `schrijf_benchmarkrapport()` | Slaat het benchmarkrapport op als Excel met toelichting en metadata |
| `maak_synthetische_1cho()` | Synthetisch 1CHO-bestand met bekende uitkomsten voor demo en validatie |
| `DEFINITIES`, `BEKOSTIGINGSTATUS_CODES` | Definities van indicatoren en DUO-redencodes |

---

## Vereisten

- R >= 4.1.0
- Tidyverse-packages (`dplyr`, `ggplot2`, `readr`, `tidyr`, `forcats`, `scales`)
- Shiny-packages (`shiny`, `bslib`, `DT`, `plotly`)
