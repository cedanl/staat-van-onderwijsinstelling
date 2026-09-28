library(staat1cho)

## Instellingen ----
## Pad naar de 1cijferho-enriched 1CHO-export. Laat leeg ("") om met
## synthetische demodata te draaien.
pad_1cho <- ""

## Analyseniveau: "student" (standaard) of "inschrijving". Zie
## vignette("staat1cho") voor het verschil.
niveau <- "student"

## Optionele bestanden. Laat leeg ("") om de stap over te slaan.
vakhawv_pad <- ""
vlpbek_pad <- ""

## LET OP bij VLPBEK: het VLPBEK-bestand bevat het echte BSN. Koppelen kan
## alleen als pad_1cho (en vakhawv_pad) de 1cijferho-uitvoer is waarin het
## BSN behouden is: niet gepseudonimiseerd en niet omgezet naar studentnummer.


## Invoer ----

if (!nzchar(pad_1cho)) {
  cli::cli_alert_warning("Geen pad_1cho ingesteld: synthetische demodata wordt gebruikt")
  pad_1cho <- tempfile(fileext = ".csv")
  readr::write_delim(maak_synthetische_1cho(), pad_1cho, delim = ";", na = "")
}

cli::cli_alert_info("INSTROOM --- Data inlezen")
Basisbestand1CHO <- maak_basisbestand(pad_1cho)

## Het peiljaar volgt uit de data: het jaar na de laatste inschrijving. Een
## hardgecodeerd jaar dat niet bij de data past telt zittende studenten als
## uitgevallen.
laatste_jaar <- max(Basisbestand1CHO$inschrijvingsjaar, na.rm = TRUE)
jaar <- laatste_jaar + 1L
soort_ho <- unique(Basisbestand1CHO$soort_hoger_onderwijs)
cli::cli_alert_info("Laatste inschrijvingsjaar {laatste_jaar}, soort HO: {soort_ho}")

uitvoer <- file.path("Output", jaar)
dir.create(uitvoer, recursive = TRUE, showWarnings = FALSE)
bewaar <- function(object, naam) {
  saveRDS(object, file.path(uitvoer, paste0(naam, "_", jaar, ".RDS")))
}


## Instroom ----

cli::cli_alert_info("INSTROOM --- Cohortbestand wordt aangemaakt")
Cohorten_Instroom <- maak_instroom_cohort(Basisbestand1CHO, soort_ho, niveau = niveau)
bewaar(Basisbestand1CHO, "Basisbestand1CHO")
bewaar(Cohorten_Instroom, "Instroom_cohorten")


## Rendement ----

cli::cli_alert_info("RENDEMENT --- Diploma- en rendementsbestand worden aangemaakt")
Diploma_behaald <- maak_diploma_behaald(Basisbestand1CHO, niveau = niveau)
Rendement_indicatoren <- bereken_rendement(
  Cohorten_Instroom,
  Diploma_behaald,
  niveau = niveau,
  laatste_jaar = laatste_jaar
)
bewaar(Diploma_behaald, "Diploma_instelling")
bewaar(Rendement_indicatoren, "Rendement")


## Uitval ----

cli::cli_alert_info("UITVAL --- Uitvalbestand wordt aangemaakt")
Uitval_indicatoren <- bereken_uitval(
  Basisbestand1CHO,
  Diploma_behaald,
  Cohorten_Instroom,
  jaar,
  niveau = niveau
)
bewaar(Uitval_indicatoren, "Uitval")


## Studiewissel (alleen op studentniveau) ----

Studiewissel_indicatoren <- NULL
if (niveau == "student") {
  cli::cli_alert_info("STUDIEWISSEL --- Switchbestand wordt aangemaakt")
  Studiewissel_indicatoren <- bereken_studiewissel(
    Basisbestand1CHO,
    Cohorten_Instroom,
    Diploma_behaald,
    Uitval_indicatoren
  )
  bewaar(Studiewissel_indicatoren, "Studiewissel")
}


## Combineer ----

cli::cli_alert_info("STAAT VAN ONDERWIJSINSTELLING --- Voeg alle data samen")
Data_1cHO_indicatoren <- combineer_indicatoren(
  Cohorten_Instroom,
  Rendement_indicatoren,
  Uitval_indicatoren,
  Studiewissel_indicatoren,
  niveau = niveau
)


## VAKHAVW (optioneel) ----

if (nzchar(vakhawv_pad) && file.exists(vakhawv_pad)) {
  cli::cli_alert_info("VAKHAVW --- Vakcijfers inlezen en koppelen")
  Vakhawv <- lees_vakhawv(vakhawv_pad)
  Data_1cHO_indicatoren <- verrijk_met_vakhawv(Data_1cHO_indicatoren, Vakhawv)
  bewaar(Vakhawv, "Vakhawv")
} else {
  cli::cli_alert_info("VAKHAVW --- Overgeslagen (geen pad ingesteld)")
}


## VLPBEK (optioneel) ----

if (nzchar(vlpbek_pad) && file.exists(vlpbek_pad)) {
  cli::cli_alert_info("VLPBEK --- Bekostiging inlezen en koppelen")
  Bekostiging <- lees_bekostiging(vlpbek_pad)
  Data_1cHO_indicatoren <- verrijk_met_bekostiging(Data_1cHO_indicatoren, Bekostiging)
  bewaar(Bekostiging, "Bekostiging")
} else {
  cli::cli_alert_info("VLPBEK --- Overgeslagen (geen pad ingesteld)")
}

bewaar(Data_1cHO_indicatoren, "Indicatoren_1cHO")


## Benchmarkrapport ----

cli::cli_alert_info("BENCHMARK --- Geaggregeerd rapport wordt opgeslagen")
Benchmark <- maak_benchmarkrapport(Data_1cHO_indicatoren)
schrijf_benchmarkrapport(Benchmark, file.path(uitvoer, paste0("Benchmarkrapport_", jaar, ".xlsx")))

cli::cli_alert_success("Klaar. Uitvoer staat in {.path {uitvoer}}")
