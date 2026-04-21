# Lees 1CHO in en maak switchbestand
# Veerle van Son & Damiëtte Bakx-van den Brink
# mei 2021

## CHECK: Studenten die een AD-diploma en doorgaan met een bachelor -> niet als switcher aanmerken
## persnr 000011600998

## Studieonderbrekers? --> de nieuwe variabele voor verblijfsjaar (verblijfsjaaractueleinstelling) telt door,
## dus kun je niet zien of iemand een studieonderbreker is. Voorheen werden studieonderbrekers niet meegenomen in
## het switchbestand, omdat het eerder berekende verblijfsjaar was afgeleid van inschrijvingsjaar min eerstejaarinstelling.
## Bij een studieonderbreking komen bepaalde verblijfsjaren dan niet voor.

## Diplomabestand --> Bevat studenten die in verblijfsjaar 0 al hun diploma halen. Hoe kan dat? Antwoord: verblijfsjaar = 0 als de inschrijving niet actief is op 1 okt
## dus een student die bij nainschrijving een diploma haalt, heeft verblijfsjaar=0.

## Switch voortaan ook alleen bekijken bij voltijd bachelor en voltijd ad (grafieken). Voor deeltijd zijn er teveel ongeregeldheden met diploma en studieduur
## (bijv diploma gekregen voor eerste actieve studiejaar bij Avans). Deeltijd studenten hebben vaak dubbel inschrijvingen, waarbij niet duidelijk is
## welke de hoofd- en welke de neveninschrijving is. Vrijstellingen? Losse modules?

## Switch binnen 3 jaar is incl switch binnen 1 jaar, maar het verschil tussen switch 1 jr en switch 3 jaar is niet de switch in het 2de of 3de jaar tov het eerste jaar,
## want het wordt alleen op de zittende studenten bepaald. De groepen zijn niet precies hetzelfde, doordat er meer studenten zijn
## die zijn uitgevallen of diploma behaald in het 2de en 3de jaar.

library(tidyverse)
jaar <- 2025


## Data inlezen

if (!"Basisbestand1CHO" %in% ls()) {
  Basisbestand1CHO <- readRDS(paste0(
    "Output/",
    jaar,
    "/Basisbestand1CHO_",
    jaar,
    ".RDS"
  ))
}

if (!"Diploma_behaald" %in% ls()) {
  Diploma_behaald <- readRDS(paste0(
    "Output/",
    jaar,
    "/Diploma_instelling_",
    jaar,
    ".RDS"
  ))
}

if (!"Cohorten_Instroom" %in% ls()) {
  Cohorten_Instroom <- readRDS(paste0(
    "Output/",
    jaar,
    "/Instroom_cohorten_",
    jaar,
    ".RDS"
  ))
}

if (!"Uitval_indicatoren" %in% ls()) {
  Uitval_indicatoren <- readRDS(paste0(
    "Output/",
    jaar,
    "/Uitval_",
    jaar,
    ".RDS"
  ))
}


## Uitval binnen 1 en 3 jaar bepalen (om deze studenten eruit te kunnen filteren)

uitval_1_3_jaar <- Uitval_indicatoren |>
  select(persoonsgebonden_nummer, uitval_xjr) |>
  mutate(
    uitval_een_drie = factor(case_when(
      uitval_xjr == 1 ~ "binnen 1 jaar",
      uitval_xjr > 1 & uitval_xjr <= 3 ~ "binnen 2- en 3de jaar",
      TRUE ~ "niet uitgevallen eerste drie jaar"
    ))
  )


## Zittende studenten per cohort bepalen

zittend_per_cohort <- Cohorten_Instroom |>
  select(-soort_diploma_instelling_label) |>
  left_join(Diploma_behaald, by = "persoonsgebonden_nummer") |>
  select(
    persoonsgebonden_nummer,
    inschrijvingsjaar,
    eerstejaar_instelling,
    jaar_eerste_diploma,
    opleidingsvorm_label
  ) |>
  mutate(
    diploma_na_x_jaar = jaar_eerste_diploma - eerstejaar_instelling + 1,
    diploma_een_drie = factor(case_when(
      diploma_na_x_jaar <= 1 ~ "binnen 1 jaar of daarvoor",
      diploma_na_x_jaar > 1 & diploma_na_x_jaar <= 3 ~ "binnen 2- en 3de jaar",
      TRUE ~ "geen diploma in eerste drie jaar"
    ))
  ) |>
  left_join(uitval_1_3_jaar, by = "persoonsgebonden_nummer")

zittend_1jr <- zittend_per_cohort |>
  filter(diploma_een_drie != "binnen 1 jaar of daarvoor") |>
  filter(uitval_een_drie != "binnen 1 jaar")

zittend_3jr <- zittend_per_cohort |>
  filter(diploma_een_drie != "binnen 1 jaar of daarvoor") |>
  filter(diploma_een_drie != "binnen 2- en 3de jaar") |>
  filter(uitval_een_drie != "binnen 1 jaar") |>
  filter(uitval_een_drie != "binnen 2- en 3de jaar")


## Studiewissel binnen 1 jaar

cli::cli_alert_info("STUDIEWISSEL --- Switch binnen 1 jaar")

Basisbestand_filter <- Basisbestand1CHO |>
  filter(persoonsgebonden_nummer %in% zittend_1jr$persoonsgebonden_nummer) |>
  filter(
    verblijfsjaar_actuele_instelling %in% c(1, 2),
    soort_inschrijving_actuele_instelling_label ==
      "hoofdinschrijving binnen het domein actuele instelling"
  )

Freq_tabel_persnr_1jr <- Basisbestand_filter |>
  count(persoonsgebonden_nummer)

if (any(Freq_tabel_persnr_1jr$n > 2)) {
  rlang::abort(
    "Meer dan twee regels per student in het switchcohortbestand gevonden!"
  )
}

Persnr_met_2verblijfsjaren_1jr <- Freq_tabel_persnr_1jr |>
  filter(n == 2)

Studiewissel_1jr_dubbeleregels <- Basisbestand_filter |>
  filter(
    persoonsgebonden_nummer %in%
      Persnr_met_2verblijfsjaren_1jr$persoonsgebonden_nummer
  )

if (
  sum(Studiewissel_1jr_dubbeleregels$verblijfsjaar_actuele_instelling == 1) !=
    sum(Studiewissel_1jr_dubbeleregels$verblijfsjaar_actuele_instelling == 2)
) {
  rlang::abort("Aantal verblijfsjaren 1 en 2 is niet gelijk")
}

Studiewissel_1jr_instelling <- Studiewissel_1jr_dubbeleregels |>
  arrange(persoonsgebonden_nummer, verblijfsjaar_actuele_instelling) |>
  mutate(
    persnr_dubbel = dplyr::if_else(
      persoonsgebonden_nummer == lag(persoonsgebonden_nummer),
      "zelfde student",
      "andere student"
    ),
    opl_dubbel = dplyr::if_else(
      opleiding_actueel_equivalent == lag(opleiding_actueel_equivalent),
      "zelfde opleiding",
      "andere opleiding"
    ),
    studiewissel_1jr = dplyr::if_else(
      verblijfsjaar_actuele_instelling == 2 &
        persnr_dubbel == "zelfde student" &
        opl_dubbel == "andere opleiding",
      "Gewisseld binnen 1 jaar",
      "Niet gewisseld binnen 1 jaar"
    ),
    verschil_kalenderjaren = inschrijvingsjaar - lag(inschrijvingsjaar),
    opleidingscode_na_switch1jr = dplyr::if_else(
      studiewissel_1jr == "Gewisseld binnen 1 jaar",
      as.character(opleiding_actueel_equivalent),
      NA_character_
    ),
    opleidingsvorm_na_switch1jr = dplyr::if_else(
      studiewissel_1jr == "Gewisseld binnen 1 jaar",
      as.character(opleidingsvorm),
      NA_character_
    ),
    opleidingsniveau_na_switch1jr = dplyr::if_else(
      studiewissel_1jr == "Gewisseld binnen 1 jaar",
      as.character(type_hoger_onderwijs_binnen_soort_hoger_onderwijs),
      NA_character_
    ),
    HBOsector_na_switch1jr = dplyr::if_else(
      studiewissel_1jr == "Gewisseld binnen 1 jaar",
      as.character(croho_onderdeel_actuele_opleiding),
      NA_character_
    )
  ) |>
  mutate(
    studiewissel_1jr = factor(studiewissel_1jr),
    opleidingscode_na_switch1jr = factor(opleidingscode_na_switch1jr),
    opleidingsvorm_na_switch1jr = factor(opleidingsvorm_na_switch1jr),
    opleidingsniveau_na_switch1jr = factor(opleidingsniveau_na_switch1jr),
    HBOsector_na_switch1jr = factor(HBOsector_na_switch1jr)
  ) |>
  select(
    persoonsgebonden_nummer,
    verschil_kalenderjaren,
    studiewissel_1jr,
    opleidingscode_na_switch1jr,
    opleidingsvorm_na_switch1jr,
    opleidingsniveau_na_switch1jr,
    HBOsector_na_switch1jr
  ) |>
  filter(
    studiewissel_1jr == "Gewisseld binnen 1 jaar",
    verschil_kalenderjaren == 1
  )


## Studiewissel binnen 3 jaar

cli::cli_alert_info("STUDIEWISSEL --- Switch binnen 3 jaar")

Basisbestand1CHO_bewerkt <- Basisbestand1CHO |>
  filter(persoonsgebonden_nummer %in% zittend_3jr$persoonsgebonden_nummer) |>
  filter(
    verblijfsjaar_actuele_instelling %in% c(1, 4),
    soort_inschrijving_actuele_instelling_label ==
      "hoofdinschrijving binnen het domein actuele instelling"
  )

Freq_tabel_persnr_3jr <- Basisbestand1CHO_bewerkt |>
  count(persoonsgebonden_nummer)

if (any(Freq_tabel_persnr_3jr$n > 2)) {
  rlang::abort(
    "Meer dan twee regels per student in het switchcohortbestand gevonden!"
  )
}

Persnr_met_2verblijfsjaren_3jr <- Freq_tabel_persnr_3jr |>
  filter(n == 2)

Studiewissel_3jr_dubbeleregels <- Basisbestand1CHO_bewerkt |>
  filter(
    persoonsgebonden_nummer %in%
      Persnr_met_2verblijfsjaren_3jr$persoonsgebonden_nummer
  )

if (
  sum(Studiewissel_3jr_dubbeleregels$verblijfsjaar_actuele_instelling == 1) !=
    sum(Studiewissel_3jr_dubbeleregels$verblijfsjaar_actuele_instelling == 4)
) {
  rlang::abort("Aantal verblijfsjaren 1 en 4 is niet gelijk")
}

Studiewissel_3jr_instelling <- Studiewissel_3jr_dubbeleregels |>
  arrange(persoonsgebonden_nummer, verblijfsjaar_actuele_instelling) |>
  mutate(
    persnr_dubbel = dplyr::if_else(
      persoonsgebonden_nummer == lag(persoonsgebonden_nummer),
      "zelfde student",
      "andere student"
    ),
    opl_dubbel = dplyr::if_else(
      opleiding_actueel_equivalent == lag(opleiding_actueel_equivalent),
      "zelfde opleiding",
      "andere opleiding"
    ),
    studiewissel_3jr = dplyr::if_else(
      verblijfsjaar_actuele_instelling == 4 &
        persnr_dubbel == "zelfde student" &
        opl_dubbel == "andere opleiding",
      "Gewisseld binnen 3 jaar",
      "Niet gewisseld binnen 3 jaar"
    ),
    verschil_kalenderjaren = inschrijvingsjaar - lag(inschrijvingsjaar),
    opleidingscode_na_switch3jr = dplyr::if_else(
      studiewissel_3jr == "Gewisseld binnen 3 jaar",
      as.character(opleiding_actueel_equivalent),
      NA_character_
    ),
    opleidingsvorm_na_switch3jr = dplyr::if_else(
      studiewissel_3jr == "Gewisseld binnen 3 jaar",
      as.character(opleidingsvorm),
      NA_character_
    ),
    opleidingsniveau_na_switch3jr = dplyr::if_else(
      studiewissel_3jr == "Gewisseld binnen 3 jaar",
      as.character(type_hoger_onderwijs_binnen_soort_hoger_onderwijs),
      NA_character_
    ),
    HBOsector_na_switch3jr = dplyr::if_else(
      studiewissel_3jr == "Gewisseld binnen 3 jaar",
      as.character(croho_onderdeel_actuele_opleiding),
      NA_character_
    )
  ) |>
  mutate(
    studiewissel_3jr = factor(studiewissel_3jr),
    opleidingscode_na_switch3jr = factor(opleidingscode_na_switch3jr),
    opleidingsvorm_na_switch3jr = factor(opleidingsvorm_na_switch3jr),
    opleidingsniveau_na_switch3jr = factor(opleidingsniveau_na_switch3jr),
    HBOsector_na_switch3jr = factor(HBOsector_na_switch3jr)
  ) |>
  select(
    persoonsgebonden_nummer,
    verschil_kalenderjaren,
    studiewissel_3jr,
    opleidingscode_na_switch3jr,
    opleidingsvorm_na_switch3jr,
    opleidingsniveau_na_switch3jr,
    HBOsector_na_switch3jr
  ) |>
  filter(
    studiewissel_3jr == "Gewisseld binnen 3 jaar",
    verschil_kalenderjaren == 3
  )


## Switchbestand samenvoegen en opslaan

Studiewissel_indicatoren <- Cohorten_Instroom |>
  left_join(Studiewissel_1jr_instelling, by = "persoonsgebonden_nummer") |>
  left_join(Studiewissel_3jr_instelling, by = "persoonsgebonden_nummer") |>
  mutate(
    studiewissel_1jr = case_when(
      !(persoonsgebonden_nummer %in%
        zittend_1jr$persoonsgebonden_nummer) ~ "Geen switch bepaald",
      TRUE ~ studiewissel_1jr
    ),
    studiewissel_3jr = case_when(
      !(persoonsgebonden_nummer %in%
        zittend_3jr$persoonsgebonden_nummer) ~ "Geen switch bepaald",
      TRUE ~ studiewissel_3jr
    )
  ) |>
  mutate(
    studiewissel_1jr = fct_na_value_to_level(
      studiewissel_1jr,
      "Niet gewisseld binnen 1 jaar"
    ),
    studiewissel_3jr = fct_na_value_to_level(
      studiewissel_3jr,
      "Niet gewisseld binnen 3 jaar"
    )
  ) |>
  select(
    persoonsgebonden_nummer,
    starts_with("studiewissel"),
    contains("switch")
  )

cli::cli_alert_info("STUDIEWISSEL --- Switchbestand wordt opgeslagen")
saveRDS(
  Studiewissel_indicatoren,
  file = paste0("Output/", jaar, "/Studiewissel_", jaar, ".RDS")
)
