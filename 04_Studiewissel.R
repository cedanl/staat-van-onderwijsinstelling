# Lees 1CHO in en maak switchbestand
# Veerle van Son & Damiëtte Bakx-van den Brink
# mei 2021

library(tidyverse)
source("R/studiewissel.R")

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


## Studiewisselbestand aanmaken

cli::cli_alert_info("STUDIEWISSEL --- Switchbestand wordt aangemaakt")
Studiewissel_indicatoren <- bereken_studiewissel(
  Basisbestand1CHO,
  Cohorten_Instroom,
  Diploma_behaald,
  Uitval_indicatoren
)


## Switchbestand opslaan

cli::cli_alert_info("STUDIEWISSEL --- Switchbestand wordt opgeslagen")
saveRDS(
  Studiewissel_indicatoren,
  file = paste0("Output/", jaar, "/Studiewissel_", jaar, ".RDS")
)
