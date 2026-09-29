# Staat van de Onderwijsinstelling (staat1cho)

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/staat1cho)](https://CRAN.R-project.org/package=staat1cho)
[![R-CMD-check](https://github.com/cedanl/staat-van-onderwijsinstelling/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cedanl/staat-van-onderwijsinstelling/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

Met staat1cho maak je van de 1cijferHO-levering van DUO een overzicht van studiesucces aan je eigen instelling: instroom, rendement (diploma binnen 3, 5 en 8 jaar), uitval (binnen 1 en 3 jaar) en studiewissel. Je bekijkt de cijfers in een dashboard in je browser en downloadt een benchmarkrapport in Excel dat je met andere instellingen kunt delen.

staat1cho is oorspronkelijk ontwikkeld voor Avans Hogeschool door Veerle van Son en Damiëtte Bakx-van den Brink en wordt doorontwikkeld door CEDA/Npuls.

## De route in het kort

| Stap | Wat je doet | Hoe vaak |
|---|---|---|
| 1 | De DUO-bestanden verzamelen | Elke levering |
| 2 | De bestanden omzetten met 1cijferho | Elke levering |
| 3 | R, RStudio en staat1cho installeren | Eenmalig |
| 4 | Het dashboard starten en je bestand laden | Elke keer |
| 5 | De cijfers lezen en het rapport delen | Elke levering |

Reken de eerste keer op een ochtend, vooral voor het installeren. Daarna kost een nieuwe levering een half uur tot een uur.

**Privacy.** De 1cijferHO-bestanden bevatten persoonsgegevens. Werk op een beveiligde werkplek van je instelling en stem met je privacy officer of FG af dat je deze analyse mag doen. Laat hen ook eenmalig naar het benchmarkrapport kijken voordat je het deelt.

---

## Stap 1: Verzamel de DUO-bestanden

Vraag de meest recente 1cijferHO-levering op bij de afdeling die hem ontvangt (meestal BI, informatiemanagement of studentenadministratie). Een levering bevat de hele geschiedenis van je studenten, dus alleen de nieuwste is nodig. Pak eventuele `.zip`-bestanden uit.

| Bestand | Herken je aan | Nodig? |
|---|---|---|
| Inschrijvingsbestand | Begint met `EV`, eindigt op `.asc` | **Ja** |
| Bestandsbeschrijving | `Bestandsbeschrijving_1cyferho_...txt` | **Ja** |
| Decodeertabellen | `Dec_...asc` en `Bestandsbeschrijving_Dec-bestanden.txt` | **Ja** |
| Havo/vwo-cijfers | Begint met `VAKHAVW`, plus `Bestandsbeschrijving_Vakkenbestanden.txt` | Optioneel, [zie VAKHAVW](#vakhavw-havo--en-vwo-cijfers) |

---

## Stap 2: Zet de bestanden om met 1cijferho

De DUO-bestanden hebben vaste kolombreedtes en codes. De gratis tool [1cijferho](https://github.com/cedanl/1cijferho) (ook van CEDA) zet ze om naar CSV met leesbare omschrijvingen. De opdrachten hieronder zijn voor Windows (PowerShell); op een Mac werk je in Terminal en gebruik je `/` in plaats van `\` in paden.

**Eenmalig installeren**

1. Installeer uv, het hulpprogramma dat Python voor 1cijferho regelt. Open PowerShell en plak:

   ```powershell
   powershell -ExecutionPolicy ByPass -c "irm https://astral.sh/uv/install.ps1 | iex"
   ```

   Op een Mac: `curl -LsSf https://astral.sh/uv/install.sh | sh`. Sluit daarna het venster en open het opnieuw.

2. Download [1cijferho als ZIP](https://github.com/cedanl/1cijferho/archive/refs/heads/main.zip) en pak het uit, bijvoorbeeld naar `C:\Tools\1cijferho`.

**Elke levering**

1. Kopieer de bestanden uit stap 1 direct (niet in een submap) naar `C:\Tools\1cijferho\data\01-input`.
2. Open de map `C:\Tools\1cijferho` in de Verkenner, typ `powershell` in de adresbalk en druk op Enter. Start de tool (de eerste keer duurt `uv sync` enkele minuten):

   ```powershell
   uv sync --extra frontend
   uv run streamlit run src/main.py
   ```

3. De tool opent in je browser. Klik op **Eigen data uploaden** en doorloop de pagina's in het menu: *Bestanden controleren*, **Stap 1 · Metadata extraheren**, **Stap 2 · Metadata valideren** en **Stap 3 · Turbo Conversie**. Kies bij Turbo Conversie deze instellingen en klik op *Start Turbo Convert*:

| Instelling | Kies | Waarom |
|---|---|---|
| Instelmodus | **Eigen instellingen** | De voorinstellingen (NFWA, Evaluatietool) zetten te weinig kolommen om. |
| EV-bestanden | **Aan** | Het hoofdbestand. |
| VAKHAVW-bestanden | Aan als je een VAKHAVW-bestand hebt | |
| Gedecodeerde variant | **Aan** | Nodig voor de verrijkte variant. |
| Verrijkte variant | **Aan** | staat1cho leest de verrijkte variant. |
| Kolomselectie | **Alles aan laten** | staat1cho gebruikt onder meer opleidingsnaam en vooropleiding. |
| snake_case | **Aan** | staat1cho verwacht kolomnamen als `persoonsgebonden_nummer`. |
| Studentnummer koppeling | **Leeg laten** | Niet nodig. |

Parquet maakt niet uit; stap 4 (*Output valideren*) is een optionele extra controle. Laat het PowerShell-venster open zolang je met de tool werkt.

**Het resultaat** staat in `data\02-output`: het bestand dat begint met `EV` en eindigt op **`_enriched.csv`** (bijvoorbeeld `EV21PL24_enriched.csv`). Heb je VAKHAVW omgezet, dan staat er ook een `VAKHAVW..._decoded.csv`.

---

## Stap 3: Installeer R, RStudio en staat1cho (eenmalig)

1. Installeer [R](https://cran.r-project.org/) en [RStudio Desktop](https://posit.co/download/rstudio-desktop/). Mag je zelf geen programma's installeren, vraag dan je IT-afdeling om R, RStudio en uv.
2. Open RStudio en plak in de Console (linksonder):

   ```r
   install.packages("pak")
   pak::pak("cedanl/staat-van-onderwijsinstelling")
   install.packages(c("bslib", "DT", "ggplot2", "plotly", "scales", "tidyr", "writexl"))
   ```

Er verschijnt veel tekst, soms in rood. Het is gelukt als de Console weer `>` toont zonder `Error`.

> **Let op:** installeer staat1cho niet met `install.packages("staat1cho")`. Die CRAN-versie (0.1.0) rekent voor recente cohorten een onterecht rendement van 0%. Gebruik de opdracht hierboven tot versie 0.2.0 op CRAN staat.

---

## Stap 4: Start het dashboard en laad je bestand

```r
library(staat1cho)
start_dashboard()
```

Het dashboard opent in je browser.

1. Kies bij **1CHO CSV-bestand** het `_enriched.csv`-bestand uit stap 2.
2. Kies het **Analyseniveau**:
   - **Studentniveau**: elke student telt één keer, bij de opleiding waarin die begon. Voor cijfers over de instelling als geheel.
   - **Inschrijvingsniveau**: elke opleiding is een eigen cohort. Om opleidingen met elkaar te vergelijken.
3. Optioneel: kies bij **VAKHAVW-bestand** het `VAKHAVW..._decoded.csv`-bestand. Laat **VLPBEK-bestand** leeg, [zie VLPBEK](#vlpbek-bekostiging).
4. Klik op **Data verwerken**.

Controleer daarna of de **peildatum** bovenin past bij je levering, en of het aantal studenten in de linkerkolom ongeveer klopt met wat je verwacht.

**Werkgeheugen.** Het dashboard accepteert bestanden tot 10 GB, maar je hebt vrij werkgeheugen nodig van ongeveer **drie keer de bestandsgrootte** (een EV-bestand van 2 GB vraagt zo'n 6 GB). Sluit andere zware programma's of gebruik een computer of virtuele werkplek met meer geheugen.

---

## Stap 5: De cijfers lezen en delen

Houd je muis boven het **?**-icoon bij een getal voor de precieze definitie. De belangrijkste keuzes:

- **Instroom**: eerste jaar aan de instelling (studentniveau) of in de opleiding (inschrijvingsniveau). Alleen hoofdinschrijvingen.
- **Rendement**: diploma binnen 3, 5 of 8 jaar aan de instelling. Propedeuses tellen niet mee.
- **Uitval**: niet meer ingeschreven en geen diploma. Een overstap naar een andere instelling telt ook als uitval.
- **Studiewissel**: overstap naar een andere opleiding binnen de instelling. Alleen op studentniveau.
- **Nog niet waarneembaar**: recente cohorten zijn bijvoorbeeld nog geen 5 jaar te volgen en tellen dan niet mee. Lege waarden bij recente jaren zijn dus normaal.
- **Vooropleiding**: havo, vwo, mbo, ho, buitenlands, overig of onbekend.
- **Eerstejaars HO**: het eerste jaar aan de instelling is ook het eerste jaar in het hoger onderwijs. Gebruik dit filter om te vergelijken met landelijke cijfers over eerstejaars.
- **Peildatum**: 1 oktober van het laatste jaar in de levering. Een nieuwe levering kan cijfers van eerdere cohorten iets bijwerken; vergelijk dus alleen cijfers met dezelfde peildatum.

Onderaan de linkerkolom staan twee downloads:

| Knop | Wat je krijgt | Delen? |
|---|---|---|
| **Download benchmarkrapport** | Excel met percentages per sector, opleidingsvorm, niveau en instroomjaar, met toelichting en peildatum. Geen persoonsnummers. Groepen van minder dan 30 studenten blijven leeg, net als percentages over minder dan 5 studenten of waarbij op minder dan 5 na iedereen het betreft. | Ja, met andere instellingen. |
| **Download als CSV** | Het volledige analysebestand, één regel per student, met persoonsnummers. | **Nee**, alleen binnen de instelling. |

---

## Optionele bestanden

### VAKHAVW: havo- en vwo-cijfers

VAKHAVW voegt het gemiddelde eindcijfer, het wiskundecijfer en het aantal vakken toe, in een extra tabblad *Vooropleiding*. Zet het samen met het EV-bestand om in dezelfde 1cijferho-run. Na het verwerken meldt het dashboard hoeveel VAKHAVW-studenten het terugvond. Dat is nooit 100%: VAKHAVW bevat ook studenten van vóór je eerste cohort, en mbo-instromers en buitenlandse studenten hebben geen havo/vwo-cijfers.

### VLPBEK: bekostiging

**Nog niet gebruiken.** Het VLPBEK-bestand gebruikt het BSN, het EV-bestand een eigen persoonsnummer van DUO. Daardoor vindt de koppeling (vrijwel) niets terug. Dit wordt nog aangepast; de rest van staat1cho werkt zonder.

---

## Als het niet lukt

| Wat je ziet | Wat je kunt doen |
|---|---|
| PowerShell: *uv wordt niet herkend* | Sluit PowerShell en open het opnieuw, of herstart de computer. |
| 1cijferho: *Geen bestanden gevonden* | Zet de bestanden direct in `data\01-input`, niet in een submap, en klik opnieuw op *Bestanden controleren*. |
| Dashboard: *Ontbrekende kolom(men)* | Kies het `_enriched.csv`-bestand, en zet opnieuw om met de instellingen uit stap 2 (eigen instellingen, verrijkte variant en snake_case aan). |
| Verwerken stopt of R loopt vast | Te weinig werkgeheugen, zie *Werkgeheugen* bij stap 4. |
| VAKHAVW: bijna niemand teruggevonden | Gebruik EV en VAKHAVW uit dezelfde levering en dezelfde 1cijferho-run. |
| RStudio: *there is no package called 'pak'* | Voer eerst `install.packages("pak")` uit. |
| Rendement is 0% voor recente cohorten | Je hebt de CRAN-versie 0.1.0. Installeer opnieuw zoals in stap 3. |

Kom je er niet uit, meld het via [GitHub Issues](https://github.com/cedanl/staat-van-onderwijsinstelling/issues). Stuur nooit echte studentgegevens mee.

## Oefenen zonder echte data

In 1cijferho kun je op de startpagina kiezen voor **Probeer met demo**. Of maak in RStudio een verzonnen bestand dat je in het dashboard uploadt:

```r
library(staat1cho)
readr::write_delim(maak_synthetische_1cho(), "oefenbestand_1cho.csv", delim = ";", na = "")
getwd()  # de map waarin het bestand staat
```

## Voor R-gebruikers

Wil je zonder dashboard werken, met een vast script (`pipeline.R`) of met de losse functies? Zie `vignette("staat1cho")`.
