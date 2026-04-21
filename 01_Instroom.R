# Lees het 1CHO in en maak een cohortbestand (één regel per student)
# Veerle van Son & Damiëtte Bakx-van den Brink
# mei 2021

library(tidyverse)
jaar <- 2025
soort_ho <- c("hoger beroepsonderwijs", "hbo")


## Data inlezen

dir.create(paste0("Output/", jaar), recursive = TRUE, showWarnings = FALSE)

Basisbestand1CHO <- read_csv2(
  "data/EV299XX24_DEMO_enriched_encrypted.csv",
  locale = locale(encoding = "UTF-8")
)

## verblijfsjaar en diplomajaar komen als zero-padded string of verrijkte tekst binnen
Basisbestand1CHO <- Basisbestand1CHO |>
  mutate(
    verblijfsjaar_actuele_instelling = as.integer(
      verblijfsjaar_actuele_instelling
    ),
    diplomajaar = suppressWarnings(as.integer(diplomajaar)),
    soort_inschrijving_actuele_instelling_label = soort_inschrijving_actuele_instelling,
    geslacht_label = geslacht,
    opleidingsvorm_label = opleidingsvorm,
    indicatie_internationale_student_label = indicatie_internationale_student,
    indicatie_eer_actueel_label = indicatie_eer_actueel,
    croho_onderdeel_actuele_opleiding_label = croho_onderdeel_actuele_opleiding,
    soort_diploma_instelling_label = soort_diploma_instelling
  )


## Instroomcohort aanmaken (1 regel per student, nieuwe instromers)

Cohorten_Instroom <- Basisbestand1CHO |>
  filter(soort_hoger_onderwijs %in% soort_ho) |>
  filter(
    soort_inschrijving_actuele_instelling_label ==
      "hoofdinschrijving binnen het domein actuele instelling",
    verblijfsjaar_actuele_instelling == 1
  ) |>
  mutate(eerstejaar_instelling = inschrijvingsjaar)

if (any(duplicated(Cohorten_Instroom$persoonsgebonden_nummer))) {
  rlang::abort("Dubbele studentnummers in het instroomcohortbestand gevonden!")
}


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
