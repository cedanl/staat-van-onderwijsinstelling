#' Maak een geaggregeerd benchmarkrapport
#'
#' Aggregeert het indicatorenbestand per sector, opleidingsvorm,
#' opleidingsniveau en instroomjaar. Berekent percentages voor de
#' kernuitkomsten en voegt een totaalrij per jaar toe. Groepen met minder
#' dan 30 studenten krijgen NA voor alle uitkomstkolommen om privacyredenen.
#'
#' @param indicatoren Tibble zoals gemaakt door [combineer_indicatoren()],
#'   eventueel aangevuld via [verrijk_met_vakhawv()] of
#'   [verrijk_met_bekostiging()].
#' @param drempel Minimale groepsgrootte. Groepen kleiner dan deze waarde
#'   krijgen NA voor alle uitkomstkolommen. Standaard 30.
#'
#' @return Een tibble in lang CSV-vriendelijk formaat met kolommen:
#'   `sector`, `opleidingsvorm`, `opleidingsniveau`, `inschrijvingsjaar`,
#'   `n`, `pct_uitval_1jr`, `pct_uitval_3jr`, `pct_rendement_3jr`,
#'   `pct_rendement_5jr`, `pct_rendement_8jr`, `pct_studiewissel_1jr`,
#'   `pct_studiewissel_3jr`, `pct_int_student`, `gem_leeftijd_instroom`,
#'   plus optioneel `gem_eindcijfer`, `pct_bekostigd` en
#'   `pct_hoofdinschrijving` als de bijbehorende kolommen aanwezig zijn.
#'   Het attribuut `gegenereerd_op` (POSIXct) registreert het tijdstip.
#'
#' @examples
#' df <- tibble::tibble(
#'   sector          = "gezondheidszorg",
#'   opleidingsvorm  = "voltijd",
#'   opleidingsniveau = "bachelor",
#'   inschrijvingsjaar = 2022L,
#'   uitval_1jr      = factor("Na 1 jaar nog ingeschreven of diploma behaald"),
#'   uitval_3jr      = factor("Na 3 jaar nog ingeschreven of diploma behaald"),
#'   rendement_3jr   = factor("Geen diploma"),
#'   rendement_5jr   = factor("Geen diploma"),
#'   rendement_8jr   = factor("Geen diploma"),
#'   int_student     = "geen internationale student",
#'   leeftijd_bij_instroom = 19L
#' )
#' maak_benchmarkrapport(df, drempel = 1L)
#' @export
maak_benchmarkrapport <- function(indicatoren, drempel = 30L) {
  heeft_studiewissel  <- "studiewissel_1jr"          %in% names(indicatoren)
  heeft_vakhawv       <- "vakhawv_gemiddeld_eindcijfer" %in% names(indicatoren)
  heeft_bekostiging   <- "indicatie_bekostigd"        %in% names(indicatoren)
  heeft_hoofdinschr   <- "indicatie_hoofdinschrijving" %in% names(indicatoren)

  .pct <- function(x, label) {
    mean(as.character(x) == label, na.rm = TRUE) * 100
  }

  .groepeer <- function(data) {
    data |>
      dplyr::summarise(
        n                  = dplyr::n(),
        pct_uitval_1jr     = .pct(uitval_1jr,  "Uitgevallen binnen 1 jaar"),
        pct_uitval_3jr     = .pct(uitval_3jr,  "Uitgevallen binnen 3 jaar"),
        pct_rendement_3jr  = .pct(rendement_3jr, "Diploma binnen 3 jaar"),
        pct_rendement_5jr  = .pct(rendement_5jr, "Diploma binnen 5 jaar"),
        pct_rendement_8jr  = .pct(rendement_8jr, "Diploma binnen 8 jaar"),
        pct_studiewissel_1jr = if (heeft_studiewissel) .pct(studiewissel_1jr, "Gewisseld binnen 1 jaar") else NA_real_,
        pct_studiewissel_3jr = if (heeft_studiewissel) .pct(studiewissel_3jr, "Gewisseld binnen 3 jaar") else NA_real_,
        pct_int_student    = .pct(int_student,  "internationale student"),
        gem_leeftijd_instroom = mean(leeftijd_bij_instroom, na.rm = TRUE),
        gem_eindcijfer     = if (heeft_vakhawv) mean(vakhawv_gemiddeld_eindcijfer, na.rm = TRUE) else NA_real_,
        pct_bekostigd      = if (heeft_bekostiging) mean(indicatie_bekostigd, na.rm = TRUE) * 100 else NA_real_,
        pct_hoofdinschrijving = if (heeft_hoofdinschr) mean(indicatie_hoofdinschrijving, na.rm = TRUE) * 100 else NA_real_,
        .groups = "drop"
      )
  }

  ## Per sector x opleidingsvorm x opleidingsniveau x instroomjaar
  per_groep <- indicatoren |>
    dplyr::group_by(
      sector, opleidingsvorm, opleidingsniveau, inschrijvingsjaar
    ) |>
    .groepeer()

  ## Totaalrij per instroomjaar (sector = "totaal", rest NA)
  per_jaar <- indicatoren |>
    dplyr::group_by(inschrijvingsjaar) |>
    .groepeer() |>
    dplyr::mutate(
      sector           = "totaal",
      opleidingsvorm   = NA_character_,
      opleidingsniveau = NA_character_,
      .before = inschrijvingsjaar
    )

  resultaat <- dplyr::bind_rows(per_groep, per_jaar) |>
    dplyr::arrange(inschrijvingsjaar, sector, opleidingsvorm, opleidingsniveau)

  ## Privacy: groepen kleiner dan drempel krijgen NA voor alle uitkomsten
  uitkomstkolommen <- c(
    "pct_uitval_1jr", "pct_uitval_3jr",
    "pct_rendement_3jr", "pct_rendement_5jr", "pct_rendement_8jr",
    "pct_studiewissel_1jr", "pct_studiewissel_3jr",
    "pct_int_student", "gem_leeftijd_instroom",
    "gem_eindcijfer", "pct_bekostigd", "pct_hoofdinschrijving"
  )
  aanwezige_uitkomsten <- intersect(uitkomstkolommen, names(resultaat))

  resultaat <- resultaat |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(aanwezige_uitkomsten),
      ~ dplyr::if_else(resultaat$n < drempel, NA_real_, .x)
    ))

  ## Verwijder optionele kolommen die nooit gevuld zijn
  altijd_na <- function(col) all(is.na(resultaat[[col]]))
  optioneel  <- c("pct_studiewissel_1jr", "pct_studiewissel_3jr",
                  "gem_eindcijfer", "pct_bekostigd", "pct_hoofdinschrijving")
  te_verwijderen <- Filter(altijd_na, optioneel)
  resultaat  <- dplyr::select(resultaat, -dplyr::all_of(te_verwijderen))

  attr(resultaat, "gegenereerd_op") <- Sys.time()
  resultaat
}
