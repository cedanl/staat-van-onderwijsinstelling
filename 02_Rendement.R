# Lees 1CHO in en maak diplomabestand
# Veerle van Son & Damiëtte Bakx-van den Brink
# mei 2021

library(staat1cho)

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


## Diplomabestand aanmaken

cli::cli_alert_info("RENDEMENT --- Diplomabestand wordt aangemaakt")
Diploma_behaald <- maak_diploma_behaald(Basisbestand1CHO)

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
Rendement_indicatoren <- bereken_rendement(Cohorten_Instroom, Diploma_behaald)

cli::cli_alert_info("RENDEMENT --- Rendementsbestand wordt opgeslagen")
saveRDS(
  Rendement_indicatoren,
  file = paste0("Output/", jaar, "/Rendement_", jaar, ".RDS")
)
