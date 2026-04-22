# Lees 1CHO in en maak uitvalbestand
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


## Uitvalbestand aanmaken

cli::cli_alert_info("UITVAL --- Uitvalbestand wordt aangemaakt")
Uitval_indicatoren <- bereken_uitval(
  Basisbestand1CHO,
  Diploma_behaald,
  Cohorten_Instroom,
  jaar
)


## Uitvalbestand opslaan

cli::cli_alert_info("UITVAL --- Uitvalbestand wordt opgeslagen")
saveRDS(
  Uitval_indicatoren,
  file = paste0("Output/", jaar, "/Uitval_", jaar, ".RDS")
)
