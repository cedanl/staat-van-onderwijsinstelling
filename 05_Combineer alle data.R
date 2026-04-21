# Lees de bestanden voor rendement, uitval en studiewissel in en combineer in één bestand
# Veerle van Son & Damiëtte Bakx-van den Brink
# mei 2021

## TO DO: Bij de aanpassingen aan opleidingsniveau en rendement ook ad meenemen, want dat gebeurt nu in de vervolgscripts.

library(tidyverse)
source("R/combineer.R")

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


## Indicatoren samenvoegen

cli::cli_alert_info("STAAT VAN ONDERWIJSINSTELLING --- Voeg alle data samen")
Data_1cHO_indicatoren <- combineer_indicatoren(
  Cohorten_Instroom,
  Rendement_indicatoren,
  Uitval_indicatoren,
  Studiewissel_indicatoren
)


## Gecombineerd bestand opslaan

cli::cli_alert_info(
  "STAAT VAN ONDERWIJSINSTELLING --- Sla gecombineerde data op"
)
saveRDS(
  Data_1cHO_indicatoren,
  file = paste0("Output/", jaar, "/Indicatoren_1cHO_", jaar, ".RDS")
)
