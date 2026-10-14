#' Start het Staat van Onderwijsinstelling dashboard
#'
#' Opent de interactieve Shiny app waarmee je een 1CHO CSV-bestand kunt
#' uploaden en studie-indicatoren kunt verkennen.
#'
#' @param voorstap Optioneel: een stap vóór het dashboard, waarin de gebruiker
#'   bronbestanden uploadt die worden samengevoegd tot één 1CHO-bestand. Een
#'   `list(titel, uitleg, samenvoegen)`; `samenvoegen(bestanden)` krijgt de
#'   data.frame van `fileInput()` (kolommen `name` en `datapath`) en geeft
#'   `list(ok, pad, meldingen)` terug. Gebruikt door
#'   staat-van-onderwijsinstelling-mbo. Zonder voorstap opent de gewone
#'   upload van een 1CHO-bestand.
#' @param diplomasoorten Zie [maak_diploma_behaald()].
#' @return No return value, called for side effects.
#' @examples
#' if (interactive()) {
#'   start_dashboard()
#' }
#' @export
start_dashboard <- function(voorstap = NULL, diplomasoorten = DIPLOMASOORTEN_HO) {
  ## De app gebruikt packages die onder Suggests staan (zodat pipeline-
  ## gebruikers ze niet hoeven te installeren). Vraag er nu om in plaats van
  ## halverwege het opstarten te crashen.
  rlang::check_installed(
    DASHBOARD_PACKAGES,
    reason = "om het dashboard te starten."
  )
  app_pad <- system.file("app", package = "staat1cho")
  if (!nzchar(app_pad)) {
    cli::cli_abort(
      "De app kon niet gevonden worden. Zorg dat staat1cho correct is geinstalleerd."
    )
  }
  shiny::shinyOptions(
    staat1cho.voorstap = voorstap,
    staat1cho.diplomasoorten = diplomasoorten
  )
  shiny::runApp(app_pad, launch.browser = TRUE)
}

DASHBOARD_PACKAGES <- c("bslib", "DT", "ggplot2", "plotly", "scales", "tidyr", "writexl")
