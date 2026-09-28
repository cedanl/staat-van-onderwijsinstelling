# Staat van de Onderwijsinstelling (staat1cho)

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/staat1cho)](https://CRAN.R-project.org/package=staat1cho)
[![R-CMD-check](https://github.com/cedanl/staat-van-onderwijsinstelling/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cedanl/staat-van-onderwijsinstelling/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

Met staat1cho maak je van de 1cijferHO-levering van DUO een overzicht van studiesucces aan je eigen instelling: hoeveel studenten instromen, hoeveel een diploma halen, hoeveel uitvallen en hoeveel van opleiding wisselen. Je bekijkt de cijfers in een dashboard in je browser en maakt met één klik een benchmarkrapport in Excel dat je kunt delen.

staat1cho is oorspronkelijk ontwikkeld voor Avans Hogeschool door Veerle van Son en Damiëtte Bakx-van den Brink en wordt doorontwikkeld door CEDA/Npuls.

Deze handleiding neemt je mee van de ruwe DUO-bestanden tot en met een gedeeld rapport. Je hoeft niet te kunnen programmeren. Je typt of plakt een paar opdrachten; elke opdracht staat hieronder uitgeschreven.

---

## Wat levert het op?

- **Een dashboard** met tabbladen voor instroom, rendement (diploma binnen 3, 5 en 8 jaar), uitval (binnen 1 en 3 jaar) en studiewissel. Je filtert op jaar, locatie, sector, opleiding, niveau, opleidingsvorm, geslacht, vooropleiding en of een student voor het eerst in het hoger onderwijs zit.
- **Een benchmarkrapport** (Excel) met percentages per sector, opleidingsvorm, niveau en instroomjaar. Er staan geen persoonsnummers in en kleine groepen zijn weggelaten, zodat je het met andere instellingen kunt uitwisselen.
- **Een volledig analysebestand** (CSV) met één regel per student. Dat bevat wel persoonsgegevens en blijft binnen je instelling.

---

## De route in het kort

```
 DUO-bestanden      1cijferho            staat1cho             resultaat
 (.asc en .txt) ──► omzetten naar ──► dashboard of     ──► cijfers en
                    leesbare CSV        pipeline              benchmarkrapport
```

| Stap | Wat je doet | Hoe vaak |
|---|---|---|
| 1 | De DUO-bestanden verzamelen | Elke nieuwe levering |
| 2 | De bestanden omzetten met 1cijferho | Elke nieuwe levering |
| 3 | R, RStudio en staat1cho installeren | Eenmalig |
| 4 | Het dashboard starten en je bestand laden | Elke keer dat je de cijfers wilt bekijken |
| 5 | De cijfers lezen | |
| 6 | Het benchmarkrapport delen | Na een nieuwe levering |

Reken voor de eerste keer op een ochtend, vooral voor het installeren. Daarna kost een nieuwe levering verwerken een half uur tot een uur.

---

## Voordat je begint

**Privacy.** De 1cijferHO-bestanden bevatten persoonsgegevens, waaronder het BSN. Werk op een beveiligde werkplek van je instelling, zet de bestanden niet op een USB-stick of in je mail, en stem met je privacy officer of FG af dat je deze analyse mag doen. Het benchmarkrapport is gemaakt om te delen; laat de privacy officer of FG ook daar eenmalig naar kijken.

**Wat je nodig hebt:**

- Een Windows- of Mac-computer waarop je programma's mag installeren. Mag dat niet, vraag dan je IT-afdeling om R, RStudio en uv te installeren (zie stap 2 en 3).
- Enkele gigabytes vrije schijfruimte: de omgezette bestanden zijn veel groter dan de originelen.

**Een paar begrippen:**

| Begrip | Wat het is |
|---|---|
| Opdrachtregel of PowerShell | Een venster waarin je opdrachten typt en met Enter uitvoert, zoals vroeger in MS-DOS. Op de Mac heet dit Terminal. |
| Map en pad | Een map is een directory. Het pad is het volledige adres ervan, zoals `C:\Tools\1cijferho`. |
| R en RStudio | R is de rekentaal waarin staat1cho is geschreven. RStudio is het programma waarin je met R werkt. |
| Console | Het venster in RStudio (meestal linksonder) waarin je R-opdrachten plakt en met Enter uitvoert. |
| Pakket | Een uitbreiding voor R. staat1cho is zo'n pakket. |

---

## Stap 1: Verzamel de DUO-bestanden

DUO levert de 1cijferHO-bestanden elk jaar aan je instelling. Meestal ontvangt de afdeling BI, informatiemanagement of de studentenadministratie ze. Vraag daar naar de meest recente levering. Een levering bevat de hele geschiedenis van je studenten, dus je hebt alleen de nieuwste nodig.

Je hebt deze bestanden nodig:

| Bestand | Herken je aan | Nodig? |
|---|---|---|
| Het inschrijvingsbestand | Naam begint met `EV`, eindigt op `.asc` | **Ja** |
| De bestandsbeschrijving | `Bestandsbeschrijving_1cyferho_...txt` | **Ja** |
| De decodeertabellen en hun beschrijving | `Dec_...asc` en `Bestandsbeschrijving_Dec-bestanden.txt` | **Ja** |
| Het vakcijferbestand van havo/vwo | Naam begint met `VAKHAVW`, plus `Bestandsbeschrijving_Vakkenbestanden.txt` | Optioneel, [zie hieronder](#vakhavw-havo--en-vwo-cijfers) |
| Het bekostigingsbestand | Voorlopige bekostiging (VLPBEK) van DUO | Optioneel, nog niet te gebruiken, [zie hieronder](#vlpbek-bekostiging) |

Zijn de bestanden ingepakt (`.zip`), pak ze dan eerst uit.

---

## Stap 2: Zet de bestanden om met 1cijferho

De DUO-bestanden zijn tekstbestanden met vaste kolombreedtes en codes in plaats van omschrijvingen. De gratis tool [1cijferho](https://github.com/cedanl/1cijferho) (ook van CEDA) zet ze om naar gewone CSV-bestanden met leesbare omschrijvingen. Dat is het bestand dat staat1cho inleest.

### 2a. Installeer uv (eenmalig)

1cijferho draait op Python. Het hulpprogramma uv regelt dat voor je.

1. Open PowerShell: klik op Start, typ `PowerShell` en druk op Enter.
2. Plak deze regel en druk op Enter:

   ```powershell
   powershell -ExecutionPolicy ByPass -c "irm https://astral.sh/uv/install.ps1 | iex"
   ```

3. Sluit PowerShell en open het opnieuw, zodat de nieuwe opdracht `uv` bekend is.

Op een Mac gebruik je in Terminal: `curl -LsSf https://astral.sh/uv/install.sh | sh`.

### 2b. Download 1cijferho (eenmalig)

1. Download [het ZIP-bestand van 1cijferho](https://github.com/cedanl/1cijferho/archive/refs/heads/main.zip).
2. Pak het uit naar een vaste plek, bijvoorbeeld `C:\Tools\1cijferho`.

### 2c. Start 1cijferho

1. Open in de Verkenner de map `C:\Tools\1cijferho`.
2. Klik in de adresbalk bovenin, typ `powershell` en druk op Enter. Er opent een PowerShell-venster dat al in de juiste map staat.
3. De eerste keer installeer je de onderdelen. Dit duurt een paar minuten:

   ```powershell
   uv sync --extra frontend
   ```

4. Start de tool:

   ```powershell
   uv run streamlit run src/main.py
   ```

De tool opent in je browser. Laat het PowerShell-venster open zolang je met de tool werkt; sluit het venster als je klaar bent.

### 2d. Zet de DUO-bestanden in de invoermap

Kopieer alle bestanden uit stap 1 naar de map `data\01-input` binnen de 1cijferho-map, dus `C:\Tools\1cijferho\data\01-input`. Zet ze direct in die map, niet in een submap.

### 2e. Doorloop de stappen in 1cijferho

Klik op de startpagina op **Eigen data uploaden**. Volg daarna de pagina's in het menu links:

1. **Bestanden uploaden**: klik op *Bestanden controleren*. De tool laat zien welke bestanden hij heeft gevonden.
2. **Stap 1 · Metadata extraheren**: de tool leest de bestandsbeschrijvingen.
3. **Stap 2 · Metadata valideren**: de tool controleert of elk databestand een passende beschrijving heeft.
4. **Stap 3 · Turbo Conversie**: kies de instellingen uit de tabel hieronder en klik op *Start Turbo Convert*.

| Instelling in Turbo Conversie | Kies | Waarom |
|---|---|---|
| Instelmodus | **Eigen instellingen** | De voorinstellingen voor andere projecten (NFWA, Evaluatietool) zetten te weinig kolommen om voor staat1cho. |
| EV-bestanden | **Aan** | Dit is het hoofdbestand. |
| VAKHAVW-bestanden | Aan als je een VAKHAVW-bestand hebt | |
| Gedecodeerde variant | **Aan** | Nodig voor de verrijkte variant. |
| Verrijkte variant | **Aan** | staat1cho leest de verrijkte variant. |
| Kolomselectie | **Alles aan laten** | staat1cho gebruikt onder meer opleidingsnaam, vooropleiding en soort diploma. |
| Parquet | Maakt niet uit | staat1cho gebruikt de CSV-bestanden. |
| snake_case | **Aan** | staat1cho verwacht kolomnamen als `persoonsgebonden_nummer`. |
| Studentnummer koppeling | **Leeg laten** | staat1cho heeft dit niet nodig. |

Stap 4 (*Output valideren*) is optioneel. Het is een extra controle op de omgezette bestanden.

### 2f. Het resultaat

In de map `data\02-output` staan nu nieuwe bestanden. Voor staat1cho heb je het bestand nodig dat begint met `EV` en eindigt op **`_enriched.csv`**, bijvoorbeeld `EV21PL24_enriched.csv`.

Heb je ook een VAKHAVW-bestand omgezet, dan staat daar ook een bestand dat begint met `VAKHAVW` en eindigt op **`_decoded.csv`**. Dat is het bestand voor de havo/vwo-cijfers.

Het EV-bestand is groot: al snel honderden megabytes, bij een grote instelling enkele gigabytes. Zie [stap 4](#stap-4-start-het-dashboard-en-laad-je-bestand) voor wat je computer daarvoor nodig heeft.

---

## Stap 3: Installeer R, RStudio en staat1cho (eenmalig)

1. Installeer **R** via [cran.r-project.org](https://cran.r-project.org/) (kies je besturingssysteem en dan *base*).
2. Installeer **RStudio Desktop** via [posit.co](https://posit.co/download/rstudio-desktop/).
3. Open RStudio. Plak in de Console (linksonder) deze drie regels en druk op Enter:

   ```r
   install.packages("pak")
   pak::pak("cedanl/staat-van-onderwijsinstelling")
   install.packages(c("bslib", "DT", "ggplot2", "plotly", "scales", "tidyr", "writexl"))
   ```

Het installeren duurt enkele minuten en er verschijnt veel tekst, soms in rood. Rode tekst is niet altijd een fout. Het is gelukt als de Console weer een `>` toont en er niet `Error` staat.

> **Let op:** installeer staat1cho niet met `install.packages("staat1cho")`. Die versie (0.1.0) is verouderd en rekent voor recente cohorten een onterecht rendement van 0%. Gebruik de opdracht hierboven tot versie 0.2.0 officieel verschenen is.

---

## Stap 4: Start het dashboard en laad je bestand

Plak in de Console van RStudio:

```r
library(staat1cho)
start_dashboard()
```

Het dashboard opent in je browser. Dan:

1. Klik bij **1CHO CSV-bestand** op *Bladeren...* en kies het `_enriched.csv`-bestand uit stap 2f.
2. Kies het **Analyseniveau**:
   - **Studentniveau**: elke student telt één keer, bij de opleiding waarin die begon. Geschikt voor cijfers over de instelling als geheel.
   - **Inschrijvingsniveau**: elke opleiding is een eigen cohort. Geschikt om opleidingen met elkaar te vergelijken.
3. Heb je een VAKHAVW-bestand? Kies dan bij **VAKHAVW-bestand** het `VAKHAVW..._decoded.csv`-bestand uit dezelfde map. Laat **VLPBEK-bestand** leeg, [zie hieronder](#optionele-bestanden-vakhavw-en-bekostiging).
4. Klik op **Data verwerken**. Bij een groot bestand duurt dit enkele minuten.

Controleer daarna twee dingen:

- Bovenin staat de **peildatum**, bijvoorbeeld *Peildatum 01-10-2024*. Die moet passen bij de levering die je gebruikt.
- Het aantal studenten in de linkerkolom moet ongeveer overeenkomen met wat je van je eigen instelling verwacht.

**Stoppen:** sluit het browsertabblad en klik in RStudio op het rode stopteken boven de Console.

### Grote bestanden en werkgeheugen

Het dashboard accepteert bestanden tot 10 GB. De echte grens is het werkgeheugen van je computer: reken op vrij geheugen van ongeveer **drie keer de bestandsgrootte**. Voor een EV-bestand van 2 GB heb je dus zo'n 6 GB vrij werkgeheugen nodig. Sluit andere zware programma's tijdens het verwerken. Is je laptop te krap, gebruik dan een computer of virtuele werkplek met meer geheugen.

### Liever een vast script? Gebruik de pipeline

Wil je het rapport elk jaar op precies dezelfde manier maken, zonder door het dashboard te klikken? Gebruik dan de pipeline:

1. Download [het ZIP-bestand van staat1cho](https://github.com/cedanl/staat-van-onderwijsinstelling/archive/refs/heads/main.zip) en pak het uit.
2. Open in RStudio het bestand `pipeline.R` (menu *File* > *Open File...*).
3. Vul bovenin het pad naar je bestand in. Gebruik in R schuine strepen naar voren (`/`), ook op Windows:

   ```r
   pad_1cho    <- "C:/Tools/1cijferho/data/02-output/EV21PL24_enriched.csv"
   niveau      <- "student"    # of "inschrijving"
   vakhawv_pad <- "C:/Tools/1cijferho/data/02-output/VAKHAVW21PL_decoded.csv"  # of "" zonder VAKHAVW
   vlpbek_pad  <- ""           # nog leeg laten, zie hieronder
   ```

4. Kies in het menu *Session* > *Set Working Directory* > *To Source File Location*. Zo komt de uitvoer naast `pipeline.R` terecht.
5. Klik rechtsboven in het scriptvenster op **Source**.

De pipeline maakt een map `Output/<jaar>` aan naast `pipeline.R`, met daarin het benchmarkrapport (Excel) en de tussenbestanden.

---

## Stap 5: De cijfers lezen

Elk getal in het dashboard heeft een **?**-icoon. Houd je muis erboven voor de precieze definitie. De belangrijkste keuzes:

- **Instroom**: studenten in hun eerste jaar aan de instelling (studentniveau) of in de opleiding (inschrijvingsniveau). Alleen hoofdinschrijvingen tellen mee.
- **Rendement**: het percentage dat binnen 3, 5 of 8 jaar een diploma haalt aan de instelling. Propedeuses tellen niet mee.
- **Uitval**: niet meer ingeschreven aan de instelling en geen diploma. Een student die naar een andere instelling gaat, telt hier dus als uitval.
- **Studiewissel**: overstappen naar een andere opleiding binnen de instelling. Alleen zichtbaar op studentniveau.
- **Nog niet waarneembaar**: voor recente cohorten is bijvoorbeeld rendement na 5 jaar nog niet te meten. Die studenten tellen niet mee in het percentage. Lege waarden bij recente jaren zijn dus normaal.
- **Vooropleiding**: de hoogste vooropleiding vóór het hoger onderwijs, samengevat als havo, vwo, mbo, ho, buitenlands, overig of onbekend.
- **Eerstejaars HO of eerder in HO**: of het eerste jaar aan de instelling ook het eerste jaar in het hoger onderwijs is. Filter op *Eerstejaars HO* als je wilt vergelijken met landelijke cijfers over eerstejaars.
- **Peildatum**: de cijfers beschrijven de stand op 1 oktober van het laatste jaar in de levering. Bij een nieuwe levering kunnen cijfers van eerdere cohorten iets veranderen, bijvoorbeeld als een student na een tussenjaar terugkomt en dan niet meer als uitgevallen telt. Vergelijk daarom alleen cijfers met dezelfde peildatum.

---

## Stap 6: Delen

In de linkerkolom van het dashboard staan twee knoppen:

| Knop | Wat je krijgt | Delen? |
|---|---|---|
| **Download benchmarkrapport** | Excel met percentages per groep, een toelichting per kolom en de peildatum. Geen persoonsnummers. Groepen van minder dan 30 studenten zijn weggelaten. Een percentage blijft leeg als het over minder dan 5 studenten gaat, of als op minder dan 5 na iedereen in de groep het betreft (bijvoorbeeld 0% of 100% uitval). | Ja, geschikt om met andere instellingen te delen. |
| **Download als CSV** | Het volledige analysebestand, één regel per student, met persoonsnummers. | **Nee**, alleen binnen de instelling. |

---

## Optionele bestanden: VAKHAVW en bekostiging

### VAKHAVW: havo- en vwo-cijfers

Het VAKHAVW-bestand voegt de eindcijfers van havo en vwo toe: gemiddeld eindcijfer, wiskundecijfer en aantal vakken. Het dashboard krijgt dan een extra tabblad *Vooropleiding*.

- Zet het VAKHAVW-bestand samen met het EV-bestand om in dezelfde 1cijferho-run (stap 2e).
- Upload in het dashboard het `VAKHAVW..._decoded.csv`-bestand.
- Na het verwerken meldt het dashboard rechtsonder hoeveel VAKHAVW-studenten het heeft teruggevonden. Dat zijn er nooit 100%: VAKHAVW bevat ook studenten die al vóór het eerste jaar in je data begonnen. Alleen studenten met een havo- of vwo-diploma hebben cijfers, dus mbo-instromers en buitenlandse studenten blijven leeg.

### VLPBEK: bekostiging

Het VLPBEK-bestand (voorlopige bekostiging) laat zien welke inschrijvingen DUO bekostigt, waarom andere niet, en welke je nog kunt herstellen door ze alsnog tijdig aan te leveren.

> **Status: nog niet gebruiken.** Het VLPBEK-bestand gebruikt het BSN, terwijl staat1cho koppelt op het eigen persoonsnummer van DUO uit het EV-bestand. Die twee zijn verschillende nummers, dus het dashboard vindt (vrijwel) geen inschrijvingen terug. We passen de koppeling aan. De rest van het dashboard werkt zonder dit bestand gewoon.

---

## Als het niet lukt

| Wat je ziet | Wat je kunt doen |
|---|---|
| In PowerShell: *uv wordt niet herkend* | Sluit PowerShell en open het opnieuw. Helpt dat niet, herstart dan de computer. |
| In 1cijferho: *Geen bestanden gevonden* | Staan de bestanden direct in `data\01-input` en niet in een submap? Klik daarna opnieuw op *Bestanden controleren*. |
| In het dashboard: *Ontbrekende kolom(men)* | Je hebt waarschijnlijk niet het `_enriched.csv`-bestand gekozen, of in 1cijferho stond *snake_case* of *Verrijkte variant* uit, of je gebruikte een voorinstelling. Zet de bestanden opnieuw om met de instellingen uit stap 2e. |
| Het verwerken stopt halverwege of R loopt vast | Waarschijnlijk te weinig werkgeheugen. Sluit andere programma's of gebruik een computer met meer geheugen, zie [Grote bestanden en werkgeheugen](#grote-bestanden-en-werkgeheugen). |
| VAKHAVW: bijna niemand teruggevonden | Gebruik het EV- en VAKHAVW-bestand uit dezelfde levering en dezelfde 1cijferho-run. |
| In RStudio: *there is no package called 'pak'* | Voer eerst `install.packages("pak")` uit. |
| De cijfers wijken af van vorig jaar | Kijk naar de peildatum. Een nieuwe levering kan oudere cohorten iets bijwerken. |
| Rendement is 0% voor recente cohorten | Je gebruikt de verouderde versie 0.1.0. Installeer opnieuw met de opdrachten uit stap 3. |

Kom je er niet uit, meld het dan via [GitHub Issues](https://github.com/cedanl/staat-van-onderwijsinstelling/issues). Stuur daarbij nooit echte studentgegevens mee.

---

## Oefenen zonder echte data

Wil je eerst oefenen? staat1cho kan een verzonnen bestand maken met dezelfde opbouw als een echte levering. Plak in de Console van RStudio:

```r
library(staat1cho)
readr::write_delim(maak_synthetische_1cho(), "oefenbestand_1cho.csv", delim = ";", na = "")
getwd()
```

De laatste regel toont de map waarin `oefenbestand_1cho.csv` is opgeslagen. Start daarna het dashboard en upload dit bestand.

1cijferho heeft ook een demomodus met voorbeeldbestanden in DUO-formaat: klik op de startpagina van 1cijferho op *Probeer met demo*.

---

## Voor ontwikkelaars

### Losse functies

Alle stappen van het dashboard zijn ook los te gebruiken:

```r
library(staat1cho)

basis      <- maak_basisbestand("pad/naar/EV..._enriched.csv")
soort_ho   <- unique(basis$soort_hoger_onderwijs)
cohort     <- maak_instroom_cohort(basis, soort_ho)
diploma    <- maak_diploma_behaald(basis)
rendement  <- bereken_rendement(cohort, diploma)
uitval     <- bereken_uitval(basis, diploma, cohort)  # peiljaar uit de data
wissel     <- bereken_studiewissel(basis, cohort, diploma, uitval)
resultaat  <- combineer_indicatoren(cohort, rendement, uitval, wissel)
rapport    <- maak_benchmarkrapport(resultaat)
schrijf_benchmarkrapport(rapport, "benchmark.xlsx")
```

Zie `vignette("staat1cho")` voor een uitgewerkt voorbeeld.

| Functie | Wat het doet |
|---|---|
| `start_dashboard()` | Start het interactieve Shiny-dashboard |
| `maak_basisbestand()` | Laadt het 1CHO-bestand en voegt labelkolommen toe |
| `maak_instroom_cohort()` | Maakt het cohortbestand aan (nieuwe instromers) |
| `maak_diploma_behaald()` | Bepaalt diplomaresultaten per student |
| `bereken_rendement()` | Rendement binnen 3, 5 en 8 jaar |
| `bereken_uitval()` | Uitvalstatus binnen 1 en 3 jaar |
| `bereken_studiewissel()` | Studiewissel binnen 1 en 3 jaar |
| `combineer_indicatoren()` | Voegt alle indicatoren samen, met vooropleiding, eerstejaars HO en het attribuut `peildatum` |
| `lees_vakhawv()` / `verrijk_met_vakhawv()` | Leest VAKHAVW-vakcijfers en koppelt ze per student |
| `lees_bekostiging()` / `verrijk_met_bekostiging()` | Leest een VLPBEK-bestand en koppelt de bekostigingsstatus |
| `is_gepseudonimiseerd()` | Herkent gepseudonimiseerde persoonsnummers |
| `maak_benchmarkrapport()` | Geaggregeerd rapport met peildatum en privacyonderdrukking (groepen < 30, cellen < 5) |
| `schrijf_benchmarkrapport()` | Slaat het benchmarkrapport op als Excel met toelichting, validatie en metadata |
| `maak_synthetische_1cho()` | Synthetisch 1CHO-bestand met bekende uitkomsten voor demo en validatie |
| `DEFINITIES`, `BEKOSTIGINGSTATUS_CODES` | Definities van de indicatoren en de DUO-redencodes |

### Invoer

staat1cho verwacht de `_enriched.csv` van 1cijferho (puntkomma-gescheiden, UTF-8, kolomnamen in snake_case). Het dashboard controleert na het uploaden of de verplichte kolommen aanwezig zijn, zoals `persoonsgebonden_nummer`, `inschrijvingsjaar`, `verblijfsjaar_actuele_instelling`, `verblijfsjaar_actuele_opleiding_instelling`, `diplomajaar`, `soort_hoger_onderwijs`, `soort_inschrijving_actuele_instelling`, `soort_diploma_instelling`, `opleiding_actueel_equivalent` en `opleidingscode_naam_opleiding`. De kolommen `hoogste_vooropleiding_voor_het_ho_omschrijving_vooropleiding` en `eerste_jaar_in_het_hoger_onderwijs` zijn optioneel; zonder die kolommen worden vooropleiding en eerstejaars HO "onbekend".

### Vereisten

- R 4.1.0 of nieuwer
- Tidyverse-pakketten (`dplyr`, `ggplot2`, `readr`, `tidyr`, `forcats`, `scales`)
- Voor het dashboard: `shiny`, `bslib`, `DT`, `plotly`, `writexl`
