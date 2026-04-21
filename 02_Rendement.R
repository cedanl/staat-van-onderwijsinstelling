# Lees 1CHO in en maak diplomabestand
# Veerle van Son & Damiëtte Bakx-van den Brink
# mei 2021

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

diplomas <- c(
  "Hoofd-bachelor-diploma binnen de actuele instelling",
  "Neven-bachelor-diploma binnen de actuele instelling",
  "Hoofd-master-diploma binnen de actuele instelling",
  "Neven-master-diploma binnen de actuele instelling",
  "Hoofd-doctoraal-diploma binnen de actuele instelling",
  "Neven-doctoraal-diploma binnen de actuele instelling",
  "Hoofddiploma beroepsfase/voortgezet binnen de actuele instelling",
  "Nevendiploma beroepsfase/voortgezet binnen de actuele instelling",
  "Hoofddiploma associate degree binnen de actuele instelling",
  "Nevendiploma associate degree binnen de actuele instelling",
  "Hoofddiploma postinitiele master binnen de actuele instelling",
  "Nevendiploma postinitiele master binnen de actuele instelling"
)

cli::cli_alert_info("RENDEMENT --- Diplomabestand wordt aangemaakt")


## Diplomabestand aanmaken

Diploma_behaald <- Basisbestand1CHO |>
  filter(soort_diploma_instelling %in% diplomas) |>
  mutate(diplomajaar = na_if(diplomajaar, 0)) |>
  group_by(persoonsgebonden_nummer) |>
  arrange(diplomajaar, .by_group = TRUE) |>
  distinct(persoonsgebonden_nummer, .keep_all = TRUE) |>
  ungroup() |>
  mutate(
    jaar_eerste_diploma = diplomajaar,
    verblijfsjaar_eerste_diploma = verblijfsjaar_actuele_instelling,
    diploma = "Diploma behaald (excl. propedeuse)"
  ) |>
  select(
    persoonsgebonden_nummer,
    jaar_eerste_diploma,
    verblijfsjaar_eerste_diploma,
    diploma
  )

if (any(duplicated(Diploma_behaald$persoonsgebonden_nummer))) {
  rlang::abort("Dubbele studentnummers in het diplomabestand gevonden!")
}

cli::cli_alert_info("RENDEMENT --- Diplomabestand wordt opgeslagen")
saveRDS(
  Diploma_behaald,
  file = paste0("Output/", jaar, "/Diploma_instelling_", jaar, ".RDS")
)


## Rendementsbestand aanmaken

if (!"Cohorten_Instroom" %in% ls()) {
  Cohorten_Instroom <- readRDS(paste0(
    "Output/",
    jaar,
    "/Instroom_cohorten_",
    jaar,
    ".RDS"
  ))
}

cli::cli_alert_info("RENDEMENT --- Rendementsbestand wordt aangemaakt")

Rendement_indicatoren <- Cohorten_Instroom |>
  left_join(Diploma_behaald, by = "persoonsgebonden_nummer") |>
  select(
    persoonsgebonden_nummer,
    eerstejaar_instelling,
    jaar_eerste_diploma,
    verblijfsjaar_eerste_diploma,
    diploma
  ) |>
  mutate(
    rendement_xjaar = jaar_eerste_diploma - eerstejaar_instelling + 1,

    rendement_3jr = case_when(
      rendement_xjaar <= 3 ~ "Diploma binnen 3 jaar",
      rendement_xjaar > 3 ~ "Diploma na 3 jaar",
      is.na(jaar_eerste_diploma) ~ "Geen diploma",
      jaar_eerste_diploma <
        eerstejaar_instelling ~ "Onbekend, want diplomajaar ligt voor eerste jaar bij Avans"
    ),

    rendement_5jr = case_when(
      rendement_xjaar <= 5 ~ "Diploma binnen 5 jaar",
      rendement_xjaar > 5 ~ "Diploma na 5 jaar",
      is.na(jaar_eerste_diploma) ~ "Geen diploma",
      jaar_eerste_diploma <
        eerstejaar_instelling ~ "Onbekend, want diplomajaar ligt voor eerste jaar bij Avans"
    ),

    rendement_8jr = case_when(
      rendement_xjaar <= 8 ~ "Diploma binnen 8 jaar",
      rendement_xjaar > 8 ~ "Diploma na 8 jaar",
      is.na(jaar_eerste_diploma) ~ "Geen diploma",
      jaar_eerste_diploma <
        eerstejaar_instelling ~ "Onbekend, want diplomajaar ligt voor eerste jaar bij Avans"
    )
  ) |>
  mutate(across(starts_with("rendement"), as.factor))

cli::cli_alert_info("RENDEMENT --- Rendementsbestand wordt opgeslagen")
saveRDS(
  Rendement_indicatoren,
  file = paste0("Output/", jaar, "/Rendement_", jaar, ".RDS")
)
