#' Beschrijvingen van de studie-indicatoren
#'
#' Korte definities van alle indicatoren die het package berekent. Afgeleid
#' uit de berekeningslogica in [maak_instroom_cohort()], [bereken_rendement()],
#' [bereken_uitval()] en [bereken_studiewissel()]. Bedoeld voor gebruik als
#' tooltip-tekst in dashboards.
#'
#' @format Een named list met één tekst per indicator
#' @export
DEFINITIES <- list(
  ## Instroom
  instroom =
    "Eerstejaars studenten: ingeschreven als hoofdinschrijving en voor het eerst
    aan de instelling (bij studentniveau) of voor het eerst in de betreffende
    opleiding (bij inschrijvingsniveau). Elke nieuwe opleiding aan de instelling
    telt als een apart cohort.",

  ## Status
  status =
    "Eindstatus na de observatieperiode. 'Diploma behaald': behaalde een
    bachelor-, master- of ad-diploma aan de instelling (excl. propedeuse).
    'Zittend': nog ingeschreven, geen diploma behaald. 'Uitgevallen': niet meer
    ingeschreven en geen diploma behaald.",

  ## Rendement
  rendement_3jr =
    "Percentage studenten dat een diploma behaalde binnen 3 academische jaren
    na instroom. Berekend als: diplomajaar - instroomjaar + 1, waarbij zowel
    diplomajaar als instroomjaar het startjaar van het academisch jaar zijn.",

  rendement_5jr =
    "Percentage studenten dat een diploma behaalde binnen 5 academische jaren
    na instroom. Voor associate degree (2-jarig) en bachelor (4-jarig) is dit
    de 1,25 x nominale studieduur.",

  rendement_8jr =
    "Percentage studenten dat een diploma behaalde binnen 8 academische jaren
    na instroom. Dit is de maximale observatietermijn voor diplomaresultaten.",

  ## Uitval
  uitval_1jr =
    "Percentage studenten dat na het eerste jaar niet meer ingeschreven is
    aan de instelling en geen diploma heeft behaald. Studenten die zijn
    overgestapt naar een andere opleiding binnen de instelling tellen niet
    mee als uitgevallen.",

  uitval_3jr =
    "Percentage studenten dat binnen 3 jaar na instroom niet meer ingeschreven
    is en geen diploma heeft behaald. Telt cumulatief: ook studenten die al
    in jaar 1 uitvielen.",

  ## Studiewissel
  studiewissel_1jr =
    "Percentage studenten dat na jaar 1 een andere opleiding volgt dan bij
    instroom. Vastgesteld door de inschrijving in verblijfsjaar 2 te vergelijken
    met verblijfsjaar 1. Alleen beschikbaar op studentniveau.",

  studiewissel_3jr =
    "Percentage studenten dat uiterlijk in jaar 4 naar een andere opleiding is
    overgestapt, gemeten bij de inschrijving in verblijfsjaar 4 ten opzichte van
    verblijfsjaar 1. Alleen beschikbaar op studentniveau."
)
