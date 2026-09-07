#' Beschrijvingen van de studie-indicatoren
#'
#' Korte definities van alle indicatoren die het package berekent, uitgesplitst
#' naar analyseniveau. Afgeleid uit de berekeningslogica in
#' [maak_instroom_cohort()], [bereken_rendement()], [bereken_uitval()] en
#' [bereken_studiewissel()]. Bedoeld voor gebruik als tooltip-tekst in
#' dashboards.
#'
#' Gebruik `DEFINITIES[["student"]]$instroom` of
#' `DEFINITIES[["inschrijving"]]$instroom` om de juiste definitie op te halen.
#'
#' @format Een nested named list met sub-lijsten `student` en `inschrijving`
#' @export
DEFINITIES <- list(
  student = list(
    instroom =
      "Eerstejaars studenten aan de instelling: ingeschreven als
      hoofdinschrijving en voor het eerst aan deze instelling
      (verblijfsjaar aan de instelling = 1). Een student telt slechts
      eenmaal mee, ongeacht hoeveel opleidingen zij volgen.",

    status =
      "Eindstatus van de student na de observatieperiode.
      'Diploma behaald': behaalde een bachelor-, master- of ad-diploma
      aan de instelling (excl. propedeuse). 'Zittend': nog ingeschreven,
      geen diploma behaald. 'Uitgevallen': niet meer ingeschreven en
      geen diploma behaald.",

    rendement_3jr =
      "Percentage studenten dat een diploma behaalde binnen 3 academische
      jaren na instroom aan de instelling. Berekend als:
      diplomajaar - instroomjaar + 1 <= 3.",

    rendement_5jr =
      "Percentage studenten dat een diploma behaalde binnen 5 academische
      jaren na instroom aan de instelling.",

    rendement_8jr =
      "Percentage studenten dat een diploma behaalde binnen 8 academische
      jaren na instroom. Dit is de maximale observatietermijn.",

    uitval_1jr =
      "Percentage studenten dat na het eerste jaar niet meer ingeschreven
      is aan de instelling en geen diploma heeft behaald. Studenten die
      naar een andere opleiding binnen de instelling zijn overgestapt
      tellen niet mee als uitgevallen.",

    uitval_3jr =
      "Percentage studenten dat binnen 3 jaar na instroom aan de
      instelling niet meer ingeschreven is en geen diploma heeft behaald.
      Telt cumulatief: ook studenten die al in jaar 1 uitvielen.",

    studiewissel_1jr =
      "Percentage studenten dat na jaar 1 een andere opleiding volgt dan
      bij instroom. Vastgesteld door de opleiding in verblijfsjaar 2 te
      vergelijken met verblijfsjaar 1.",

    studiewissel_3jr =
      "Percentage studenten dat uiterlijk in jaar 4 naar een andere
      opleiding is overgestapt, gemeten bij verblijfsjaar 4 ten opzichte
      van verblijfsjaar 1."
  ),

  inschrijving = list(
    instroom =
      "Eerstejaars inschrijvingen per opleiding: een student telt mee
      zodra zij voor het eerst in een specifieke opleiding aan deze
      instelling staan (verblijfsjaar in de opleiding = 1). Een student
      die van opleiding wisselt start een nieuw cohort bij de nieuwe
      opleiding.",

    status =
      "Eindstatus van de inschrijving na de observatieperiode.
      'Diploma behaald': behaalde een bachelor-, master- of ad-diploma
      voor deze opleiding (excl. propedeuse). 'Zittend': nog ingeschreven
      in de opleiding, geen diploma behaald. 'Uitgevallen': niet meer
      ingeschreven in de opleiding en geen diploma behaald.",

    rendement_3jr =
      "Percentage inschrijvingen met een diploma binnen 3 academische
      jaren na eerste inschrijving in de opleiding. Berekend als:
      diplomajaar - instroomjaar + 1 <= 3.",

    rendement_5jr =
      "Percentage inschrijvingen met een diploma binnen 5 academische
      jaren na eerste inschrijving in de opleiding.",

    rendement_8jr =
      "Percentage inschrijvingen met een diploma binnen 8 academische
      jaren na eerste inschrijving in de opleiding. Dit is de maximale
      observatietermijn.",

    uitval_1jr =
      "Percentage inschrijvingen waarbij de student na het eerste jaar
      niet meer in deze opleiding ingeschreven is en geen diploma heeft
      behaald. Een overstap naar een andere opleiding telt hier als uitval
      uit de opleiding.",

    uitval_3jr =
      "Percentage inschrijvingen waarbij de student binnen 3 jaar na
      instroom in de opleiding niet meer ingeschreven is en geen diploma
      heeft behaald. Telt cumulatief.",

    ## Studiewissel is niet beschikbaar op inschrijvingsniveau
    studiewissel_1jr = NULL,
    studiewissel_3jr = NULL
  )
)
