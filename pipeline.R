library(staat1cho)

jaar <- 2025
soort_ho <- c("wetenschappelijk onderwijs", "wo")

dir.create(paste0("Output/", jaar), recursive = TRUE, showWarnings = FALSE)


## Instroom ----

cli::cli_alert_info("INSTROOM --- Data inlezen")
Basisbestand1CHO <- maak_basisbestand("data/EV299XX24_DEMO_enriched_encrypted.csv")

cli::cli_alert_info("INSTROOM --- Cohortbestand wordt aangemaakt")
Cohorten_Instroom <- maak_instroom_cohort(Basisbestand1CHO, soort_ho)

saveRDS(Basisbestand1CHO, paste0("Output/", jaar, "/Basisbestand1CHO_", jaar, ".RDS"))
saveRDS(Cohorten_Instroom, paste0("Output/", jaar, "/Instroom_cohorten_", jaar, ".RDS"))


## Rendement ----

cli::cli_alert_info("RENDEMENT --- Diplomabestand wordt aangemaakt")
Diploma_behaald <- maak_diploma_behaald(Basisbestand1CHO)

cli::cli_alert_info("RENDEMENT --- Rendementsbestand wordt aangemaakt")
Rendement_indicatoren <- bereken_rendement(Cohorten_Instroom, Diploma_behaald)

saveRDS(Diploma_behaald, paste0("Output/", jaar, "/Diploma_instelling_", jaar, ".RDS"))
saveRDS(Rendement_indicatoren, paste0("Output/", jaar, "/Rendement_", jaar, ".RDS"))


## Uitval ----

cli::cli_alert_info("UITVAL --- Uitvalbestand wordt aangemaakt")
Uitval_indicatoren <- bereken_uitval(
  Basisbestand1CHO,
  Diploma_behaald,
  Cohorten_Instroom,
  jaar
)

saveRDS(Uitval_indicatoren, paste0("Output/", jaar, "/Uitval_", jaar, ".RDS"))


## Studiewissel ----

cli::cli_alert_info("STUDIEWISSEL --- Switchbestand wordt aangemaakt")
Studiewissel_indicatoren <- bereken_studiewissel(
  Basisbestand1CHO,
  Cohorten_Instroom,
  Diploma_behaald,
  Uitval_indicatoren
)

saveRDS(Studiewissel_indicatoren, paste0("Output/", jaar, "/Studiewissel_", jaar, ".RDS"))


## Combineer ----

cli::cli_alert_info("STAAT VAN ONDERWIJSINSTELLING --- Voeg alle data samen")
Data_1cHO_indicatoren <- combineer_indicatoren(
  Cohorten_Instroom,
  Rendement_indicatoren,
  Uitval_indicatoren,
  Studiewissel_indicatoren
)


## VAKHAVW (optioneel) ----
## Stel het pad in naar het VAKHAVW-bestand. Laat leeg ("") om deze stap over te slaan.

vakhawv_pad <- ""

if (nchar(vakhawv_pad) > 0 && file.exists(vakhawv_pad)) {
  cli::cli_alert_info("VAKHAVW --- Vakcijfers inlezen")
  Vakhawv <- lees_vakhawv(vakhawv_pad)

  cli::cli_alert_info("VAKHAVW --- Koppelen aan indicatorenbestand via persoonsgebonden_nummer")
  Data_1cHO_indicatoren <- verrijk_met_vakhawv(Data_1cHO_indicatoren, Vakhawv)

  saveRDS(Vakhawv, paste0("Output/", jaar, "/Vakhawv_", jaar, ".RDS"))
  cli::cli_alert_success("VAKHAVW --- Klaar")
} else {
  cli::cli_alert_info("VAKHAVW --- Overgeslagen (geen pad ingesteld)")
}


saveRDS(Data_1cHO_indicatoren, paste0("Output/", jaar, "/Indicatoren_1cHO_", jaar, ".RDS"))
