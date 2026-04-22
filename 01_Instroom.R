# Lees het 1CHO in en maak een cohortbestand (één regel per student)
# Veerle van Son & Damiëtte Bakx-van den Brink
# mei 2021

library(staat1cho)

jaar <- 2025
soort_ho <- c("hoger beroepsonderwijs", "hbo")


## Data inlezen

dir.create(paste0("Output/", jaar), recursive = TRUE, showWarnings = FALSE)

cli::cli_alert_info("INSTROOM --- Data inlezen")
Basisbestand1CHO <- maak_basisbestand(
  "data/EV299XX24_DEMO_enriched_encrypted.csv",
  jaar
)


## Instroomcohort aanmaken

cli::cli_alert_info("INSTROOM --- Cohortbestand wordt aangemaakt")
Cohorten_Instroom <- maak_instroom_cohort(Basisbestand1CHO, soort_ho)


## Opslaan

cli::cli_alert_info("INSTROOM --- Cohortbestand wordt opgeslagen")
saveRDS(
  Cohorten_Instroom,
  file = paste0("Output/", jaar, "/Instroom_cohorten_", jaar, ".RDS")
)
saveRDS(
  Basisbestand1CHO,
  file = paste0("Output/", jaar, "/Basisbestand1CHO_", jaar, ".RDS")
)
