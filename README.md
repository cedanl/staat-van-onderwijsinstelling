# Staat van de Onderwijsinstelling

Scripts voor het berekenen van studie-indicatoren op basis van 1CHO-data. Oorspronkelijk ontwikkeld voor Avans Hogeschool door Veerle van Son en Damiëtte Bakx-van den Brink.

De scripts berekenen vier indicatoren per instroomcohort: instroom, rendement, uitval en studiewissel.

## Vereisten

- R met tidyverse

## Gebruik

Stel bovenaan `01_Instroom.R` het jaartal en het type instelling in:

```r
jaar     = 2025
soort_ho = c("hoger beroepsonderwijs", "hbo")  # gebruik c("wetenschappelijk onderwijs", "wo") voor WO
```

Draai de scripts daarna in volgorde:

```
01_Instroom.R
02_Rendement.R
03_Uitval.R
04_Studiewissel.R
05_Combineer alle data.R
```

## Invoerdata

Script 01 leest een verrijkt 1CHO-bestand uit de `data/` map:

```
data/<bestandsnaam>.csv
```

Het invoerbestand moet de **enriched** output zijn van de [1cijferho tool](https://github.com/cedanl/1cijferho). Dat is het bestand met `_enriched` in de naam, waarbij codes al zijn omgezet naar leesbare labels (zoals "man"/"vrouw", "voltijd"/"deeltijd", "hoofdinschrijving...").

Pas de bestandsnaam aan in `01_Instroom.R` als je een ander bestand gebruikt.

De overige scripts lezen tussenbestanden uit de `Output/` map die door de vorige scripts zijn aangemaakt.

## Uitvoer

Alle tussenbestanden en het eindresultaat worden opgeslagen in:

```
Output/<jaar>/
```

Het eindbestand `Indicatoren_1cHO_<jaar>.RDS` bevat per student onder andere: locatie, opleiding, opleidingsvorm, geslacht, leeftijd bij instroom, rendement (3-8 jaar), uitval en studiewissel.

## Opzet

| Script | Wat het doet |
|---|---|
| 01_Instroom | Maakt cohortbestand aan (één regel per student, nieuwe instromers) |
| 02_Rendement | Berekent diplomaresultaten per cohort |
| 03_Uitval | Berekent uitval per jaar na instroom |
| 04_Studiewissel | Berekent van opleiding gewisseld binnen 1 en 3 jaar |
| 05_Combineer | Voegt alle indicatoren samen tot één analyseklaar bestand |
