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
vlpbek_pad <- ""  # nog leeg laten

## LET OP bij VLPBEK: het VLPBEK-bestand gebruikt het BSN, het 1CHO-bestand
## een eigen persoonsnummer van DUO. De koppeling vindt daardoor nog (vrijwel)
## niets terug.


## Invoer ----

if (!nzchar(pad_1cho)) {
  cli::cli_alert_warning("Geen pad_1cho ingesteld: synthetische demodata wordt gebruikt")
  pad_1cho <- tempfile(fileext = ".csv")
  readr::write_delim(maak_synthetische_1cho(), pad_1cho, delim = ";", na = "")
}

## Indicatoren ----

## Eén functie voert de hele rekenketen uit (dezelfde als het dashboard).
## Wil je een tussenstap inzien, gebruik dan de losse functies; zie
## vignette("staat1cho").
cli::cli_alert_info("INDICATOREN --- Analysebestand wordt aangemaakt")
Data_1cHO_indicatoren <- maak_analysebestand(pad_1cho, niveau = niveau)

## Het peiljaar volgt uit de data: het jaar na de laatste inschrijving.
peildatum <- attr(Data_1cHO_indicatoren, "peildatum")
jaar <- as.integer(format(peildatum, "%Y")) + 1L
cli::cli_alert_info("Peildatum van de data: {format(peildatum)}")

uitvoer <- file.path("Output", jaar)
dir.create(uitvoer, recursive = TRUE, showWarnings = FALSE)
bewaar <- function(object, naam) {
  saveRDS(object, file.path(uitvoer, paste0(naam, "_", jaar, ".RDS")))
}


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

## CSV voor Excel, Python, SPSS of Power BI. Bevat persoonsnummers: alleen
## voor gebruik binnen de instelling.
schrijf_analysebestand(
  Data_1cHO_indicatoren,
  file.path(uitvoer, paste0("Indicatoren_1cHO_", jaar, ".csv"))
)


## Benchmarkrapport ----

cli::cli_alert_info("BENCHMARK --- Geaggregeerd rapport wordt opgeslagen")
Benchmark <- maak_benchmarkrapport(Data_1cHO_indicatoren)
schrijf_benchmarkrapport(Benchmark, file.path(uitvoer, paste0("Benchmarkrapport_", jaar, ".xlsx")))

cli::cli_alert_success("Klaar. Uitvoer staat in {.path {uitvoer}}")
