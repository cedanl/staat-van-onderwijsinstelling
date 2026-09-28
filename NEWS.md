# staat1cho 0.2.0

## Nieuwe functies

* `lees_vakhawv()` en `verrijk_met_vakhawv()`: vooropleidingscijfers uit
  VAKHAVW koppelen per student.
* `lees_bekostiging()`, `verrijk_met_bekostiging()` en
  `BEKOSTIGINGSTATUS_CODES`: bekostigingsstatus uit een VLPBEK-bestand
  koppelen, inclusief leesbare reden en of die herstelbaar is.
* `maak_benchmarkrapport()`: geaggregeerd rapport per sector,
  opleidingsvorm, niveau en instroomjaar met privacyonderdrukking.
  `schrijf_benchmarkrapport()` slaat het op als Excel met toelichting,
  validatiepunten en metadata.
* `maak_synthetische_1cho()`: synthetisch 1CHO-bestand met complete
  loopbanen en bekende uitkomsten, voor demo's en validatie (#43).

## Gewijzigd gedrag

* **Onvolledige cohorten** (#38): rendement, uitval en studiewissel zijn
  `"Nog niet waarneembaar"` voor cohorten waarvan het meetvenster nog niet
  in de data zit. Die rijen tellen niet mee in percentages. Voorheen kregen
  recente cohorten 0% rendement.
* `bereken_rendement()` heeft een argument `laatste_jaar`;
  `bereken_uitval()` leidt `jaar` af uit de data en geeft een fout als de
  data inschrijvingen na `jaar - 1` bevat.
* **Inlezen** (#39): `maak_basisbestand()` en `lees_vakhawv()` lezen alles
  als tekst en zetten getalkolommen expliciet om. Voorloopnullen in ID's en
  postcodes blijven behouden; leeftijd is een geheel getal (daardoor was
  `gem_leeftijd_instroom` altijd leeg).
* **Benchmarkrapport** (#41): secundaire onderdrukking, optionele
  celonderdrukking (`min_cel`), afronding op hele procenten en een
  `metadata`-attribuut met niveau, drempel en packageversie.
* **VLPBEK** (#42): studenten zonder BSN krijgen hun onderwijsnummer als
  sleutel, de kolomindeling wordt gecontroleerd, "herstelbaar" vereist dat
  alle redenen herstelbaar zijn, `pd` wordt "deels bekostigd" en er is een
  kolom `bekostiging_jaar`. Beide verrijkingsfuncties melden het
  koppelpercentage.
* **Welke 1cijferho-uitvoer** (#42): VLPBEK bevat altijd het echte BSN, dus
  koppelen vereist 1cijferho-uitvoer met het BSN behouden (niet
  gepseudonimiseerd, niet omgezet naar studentnummer). README, dashboard en
  `pipeline.R` zeggen dit expliciet. Nieuwe functie `is_gepseudonimiseerd()`;
  een koppeling tussen een gepseudonimiseerd en een niet-gepseudonimiseerd
  bestand (VLPBEK of VAKHAVW) geeft een foutmelding met deze instructie. Het
  koppelpercentage wordt vanuit het bronbestand berekend.
* **Studentniveau** (#45): `combineer_indicatoren()` voegt
  `opleidingscode_diploma` en `diploma_in_instroomopleiding` toe; het
  dashboard legt uit dat filters op opleiding/sector de instroomopleiding
  betreffen.
* `maak_diploma_behaald()` gebruikt op inschrijvingsniveau het verblijfsjaar
  in de opleiding en levert `soort_diploma` en `opleidingscode_diploma`.
  `soortdiploma` in het eindbestand komt nu uit het behaalde diploma (#44).
* `bereken_studiewissel()` weigert cohorten op inschrijvingsniveau;
  `niveau` wordt overal gevalideerd (#44).
* `start_dashboard()` vraagt om ontbrekende dashboard-packages.

# staat1cho 0.1.0

* Eerste CRAN-release.
