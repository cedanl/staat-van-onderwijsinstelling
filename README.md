# staat1cho

R-package voor het berekenen van studie-indicatoren op basis van 1CHO-data. Oorspronkelijk ontwikkeld voor Avans Hogeschool door Veerle van Son en Damiëtte Bakx-van den Brink, doorontwikkeld door CEDA/Npuls.

Het package berekent vier indicatoren per instroomcohort: instroom, rendement, uitval en studiewissel. De resultaten zijn te bekijken via een interactief Shiny-dashboard of te exporteren als CSV.

## Installeren

```r
# install.packages("pak")
pak::pak("cedanl/staat-van-onderwijsinstelling")
```

## Gebruik

### Dashboard

Upload een 1CHO CSV-bestand en verken de indicatoren interactief:

```r
library(staat1cho)
start_dashboard()
```

Het dashboard opent in je browser. Daar kun je:

- Een 1CHO-bestand uploaden en verwerken
- Filteren op jaar, locatie, sector, opleiding, opleidingsvorm en geslacht
- Trends bekijken voor instroom, rendement, uitval en studiewissel
- De verwerkte data downloaden als CSV

### Losse scripts

De scripts in de projectroot kunnen ook los worden gedraaid, zonder het package te installeren:

```
01_Instroom.R
02_Rendement.R
03_Uitval.R
04_Studiewissel.R
05_Combineer alle data.R
```

Stel bovenaan `01_Instroom.R` het cohortjaar en het type instelling in:

```r
jaar     = 2025
soort_ho = c("hoger beroepsonderwijs", "hbo")  # gebruik c("wetenschappelijk onderwijs", "wo") voor WO
```

Draai de scripts daarna in volgorde. De tussenbestanden worden opgeslagen in `Output/<jaar>/`.

## Invoerdata

Het package verwacht de **enriched** output van de [1cijferho tool](https://github.com/cedanl/1cijferho): het CSV-bestand met `_enriched` in de naam, waarbij codes al zijn omgezet naar leesbare labels (zoals "man"/"vrouw", "voltijd"/"deeltijd").

Het bestand moet onder andere deze kolommen bevatten:

- `persoonsgebonden_nummer`
- `inschrijvingsjaar`
- `verblijfsjaar_actuele_instelling`
- `diplomajaar`
- `soort_hoger_onderwijs`
- `geslacht`, `opleidingsvorm`, `opleidingscode_naam_opleiding`
- `vestigingsnummer_gemeentenaam_volgens_rio`

Als een verplichte kolom ontbreekt, meldt het dashboard dit direct na het uploaden.

## Uitvoer

Per student worden de volgende indicatoren berekend:

| Categorie | Indicatoren |
|---|---|
| Studentkenmerken | instroomjaar, geslacht, locatie, sector, opleidingsvorm, leeftijd bij instroom |
| Status | status na observatieperiode, soort diploma |
| Rendement | diploma binnen 3, 5 en 8 jaar |
| Uitval | uitval binnen 1 en 3 jaar |
| Studiewissel | gewisseld binnen 1 en 3 jaar, opleiding/sector na wissel |

## Functies

| Functie | Wat het doet |
|---|---|
| `start_dashboard()` | Start het interactieve Shiny-dashboard |
| `maak_basisbestand()` | Laadt en filtert het 1CHO-bestand |
| `maak_instroom_cohort()` | Maakt cohortbestand aan (nieuwe instromers) |
| `maak_diploma_behaald()` | Bepaalt diplomaresultaten per student |
| `bereken_rendement()` | Rendement binnen 3, 5 en 8 jaar |
| `bereken_uitval()` | Uitvalstatus binnen 1 en 3 jaar |
| `bereken_studiewissel()` | Studiewissel binnen 1 en 3 jaar |
| `combineer_indicatoren()` | Voegt alle indicatoren samen |

## Vereisten

- R >= 4.1.0
- Tidyverse-packages (dplyr, ggplot2, readr, tidyr, forcats, scales)
- Shiny-packages (shiny, bslib, DT, plotly)
