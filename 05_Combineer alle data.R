# Lees de bestanden voor rendement, uitval en studiewissel in en combineer in één bestand
# Veerle van Son & Damiëtte Bakx-van den Brink
# mei 2021

## TO DO: Bij de aanpassingen aan opleidingsniveau en rendement ook ad meenemen, want dat gebeurt nu in de vervolgscripts.

library(tidyverse)
jaar <- 2025


## Data inlezen

Rendement_indicatoren <- readRDS(paste0(
  "Output/",
  jaar,
  "/Rendement_",
  jaar,
  ".RDS"
))
Uitval_indicatoren <- readRDS(paste0("Output/", jaar, "/Uitval_", jaar, ".RDS"))
Studiewissel_indicatoren <- readRDS(paste0(
  "Output/",
  jaar,
  "/Studiewissel_",
  jaar,
  ".RDS"
))

if (!"Cohorten_Instroom" %in% ls()) {
  Cohorten_Instroom <- readRDS(paste0(
    "Output/",
    jaar,
    "/Instroom_cohorten_",
    jaar,
    ".RDS"
  ))
}

cli::cli_alert_info("STAAT VAN ONDERWIJSINSTELLING --- Voeg alle data samen")


## Indicatoren samenvoegen

Data_1cHO_indicatoren <- Cohorten_Instroom |>
  left_join(Rendement_indicatoren, by = "persoonsgebonden_nummer") |>
  left_join(Uitval_indicatoren, by = "persoonsgebonden_nummer") |>
  left_join(Studiewissel_indicatoren, by = "persoonsgebonden_nummer") |>

  mutate(
    locatie = case_when(
      vestigingsnummer_actueel == 0 ~ "Breda",
      vestigingsnummer_actueel == 4 ~ "Tilburg",
      vestigingsnummer_actueel == 6 ~ "Den Bosch",
      vestigingsnummer_actueel == 7 ~ "Roosendaal"
    )
  ) |>

  select(
    inschrijvingsjaar,
    geslacht = geslacht_label,
    locatie,
    opleidingscode = opleiding_actueel_equivalent,
    opleidingsvorm = opleidingsvorm_label,
    opleidingsniveau = type_hoger_onderwijs_binnen_soort_hoger_onderwijs,
    int_student = indicatie_internationale_student_label,
    indicatie_EER = indicatie_eer_actueel_label,
    HBOsector = croho_onderdeel_actuele_opleiding_label,
    leeftijd_bij_instroom = leeftijd_per_peildatum_1_oktober,
    postcode4_student_1okt = postcodecijfers_student_op_1_oktober,
    postcode4_vooropleiding_voorHO = postcodecijfers_van_de_hoogste_vooropl_voor_het_ho,
    status,
    soortdiploma = soort_diploma_instelling_label,
    rendement_3jr:rendement_8jr,
    uitval_xjr:HBOsector_na_switch3jr
  ) |>

  mutate(
    opleidingscode = factor(opleidingscode),
    locatie = factor(locatie),
    opleidingsvorm = factor(opleidingsvorm),
    postcode4_student_1okt = factor(postcode4_student_1okt),
    postcode4_vooropleiding_voorHO = factor(postcode4_vooropleiding_voorHO)
  ) |>

  mutate(
    opleidingsvorm = fct_recode(
      opleidingsvorm,
      "duaal" = "coöp-student of duaal onderwijs (vanaf het studiejaar 1998-1999)"
    ),

    HBOsector = fct_recode(
      HBOsector,
      "gedrag & maatschappij" = "gedrag en maatschappij",
      "taal & cultuur" = "taal en cultuur"
    ),

    postcode4_student_1okt = fct_recode(
      postcode4_student_1okt,
      NULL = "0010",
      NULL = "0020",
      NULL = "0030",
      NULL = "0040"
    ),

    postcode4_vooropleiding_voorHO = fct_recode(
      postcode4_vooropleiding_voorHO,
      NULL = "0010",
      NULL = "0020",
      NULL = "0030",
      NULL = "0040"
    ),

    opleidingsniveau = fct_recode(
      opleidingsniveau,
      NULL = "postinitiele master",
      bachelor = "ba",
      master = "ma"
    )
  ) |>

  mutate(
    uitval = case_when(
      uitval_xjr == 1 ~ "Uitgevallen binnen 1 jaar",
      uitval_xjr %in% 2:3 ~ "Uitgevallen in 2e of 3e jaar",
      uitval_xjr > 3 ~ "Uitgevallen na 3 jaar",
      TRUE ~ "Niet uitgevallen"
    ),

    studiewissel = case_when(
      studiewissel_1jr == "Gewisseld binnen 1 jaar" ~ "Gewisseld binnen 1 jaar",
      studiewissel_3jr ==
        "Gewisseld binnen 3 jaar" ~ "Gewisseld in het 2e of 3e jaar",
      TRUE ~ "Niet gewisseld"
    ),

    rendement = case_when(
      rendement_5jr == "Diploma binnen 5 jaar" ~ "Diploma binnen 5 jaar",
      rendement_8jr == "Diploma binnen 8 jaar" ~ "Diploma binnen 5-8 jaar",
      rendement_8jr == "Diploma na 8 jaar" ~ "Diploma na 8 jaar",
      rendement_8jr == "Geen diploma" ~ "Geen diploma"
    )
  )


## Gecombineerd bestand opslaan

cli::cli_alert_info(
  "STAAT VAN ONDERWIJSINSTELLING --- Sla gecombineerde data op"
)
saveRDS(
  Data_1cHO_indicatoren,
  file = paste0("Output/", jaar, "/Indicatoren_1cHO_", jaar, ".RDS")
)
