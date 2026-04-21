# Lees 1CHO in en maak uitvalbestand
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


## Uitstroombestand aanmaken

## Laatste rij per persnr selecteren: dat is het laatste jaar van inschrijving
Uitstroom_instelling <- Basisbestand1CHO |>
  group_by(persoonsgebonden_nummer) |>
  arrange(
    desc(inschrijvingsjaar),
    soort_inschrijving_actuele_instelling,
    .by_group = TRUE
  ) |>
  distinct(persoonsgebonden_nummer, .keep_all = TRUE) |>
  ungroup() |>
  mutate(
    laatste_jaar_inschrijving = case_when(
      inschrijvingsjaar == jaar - 1 ~ NA_real_,
      is.na(inschrijvingsjaar) ~ NA_real_,
      TRUE ~ as.numeric(inschrijvingsjaar)
    )
  ) |>
  select(persoonsgebonden_nummer, inschrijvingsjaar, laatste_jaar_inschrijving)

if (any(duplicated(Uitstroom_instelling$persoonsgebonden_nummer))) {
  rlang::abort("Dubbele studentnummers in het uitvalbestand gevonden!")
}


## Uitvalbestand aanmaken

cli::cli_alert_info("UITVAL --- Uitvalbestand wordt aangemaakt")

Uitval_indicatoren <- Uitstroom_instelling |>
  left_join(Diploma_behaald, by = "persoonsgebonden_nummer") |>
  left_join(
    Cohorten_Instroom |> select(persoonsgebonden_nummer, eerstejaar_instelling),
    by = "persoonsgebonden_nummer"
  ) |>
  mutate(
    status = factor(case_when(
      diploma == "Diploma behaald (excl. propedeuse)" ~ "Diploma behaald",
      inschrijvingsjaar == jaar - 1 ~ "Zittend",
      TRUE ~ "Uitgevallen"
    )),
    uitval_xjr = case_when(
      status == "Uitgevallen" ~ laatste_jaar_inschrijving +
        1 -
        eerstejaar_instelling
    ),
    uitval_1jr = factor(case_when(
      uitval_xjr == 1 ~ "Uitgevallen binnen 1 jaar",
      TRUE ~ "Na 1 jaar nog ingeschreven of diploma behaald"
    )),
    uitval_3jr = factor(case_when(
      uitval_xjr <= 3 ~ "Uitgevallen binnen 3 jaar",
      TRUE ~ "Na 3 jaar nog ingeschreven of diploma behaald"
    ))
  ) |>
  select(
    persoonsgebonden_nummer,
    laatste_jaar_inschrijving,
    diploma,
    status,
    starts_with("uitval")
  )

if (any(is.na(Uitval_indicatoren$status))) {
  rlang::abort("Niet alle statussen zijn gevuld")
}


## Uitvalbestand opslaan

cli::cli_alert_info("UITVAL --- Uitvalbestand wordt opgeslagen")
saveRDS(
  Uitval_indicatoren,
  file = paste0("Output/", jaar, "/Uitval_", jaar, ".RDS")
)
