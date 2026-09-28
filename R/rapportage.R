#' Maak een geaggregeerd benchmarkrapport
#'
#' Aggregeert het indicatorenbestand per sector, opleidingsvorm,
#' opleidingsniveau en instroomjaar. Berekent percentages voor de
#' kernuitkomsten en voegt een totaalrij per jaar toe.
#'
#' ## Privacyonderdrukking
#'
#' Groepen met minder dan `drempel` studenten (standaard 30) krijgen NA voor
#' alle uitkomsten, inclusief `n`. Dit volgt de DUO-richtlijn dat ook de
#' groepsgrootte zelf herleidbaar is en dus niet getoond mag worden. De kolom
#' `onderdrukt` (TRUE/FALSE) maakt zichtbaar welke rijen zijn onderdrukt
#' zonder iets te onthullen over de werkelijke omvang.
#'
#' Totaalrijen per instroomjaar vallen doorgaans buiten de drempel en worden
#' niet onderdrukt; ze zijn bedoeld als validatiereferentie (zie hieronder).
#'
#' ## Gebruik als validatiemiddel
#'
#' CEDA kan met dit rapport controleren of de tool correct werkt:
#'
#' - **Totalen vs. DUO-publicaties**: `n` in de totaalrij per instroomjaar
#'   moet overeenkomen met het aantal studenten dat DUO voor die instelling
#'   en dat jaar publiceert. Een grote afwijking wijst op een verwerkingsfout.
#' - **Plausibele marges**: uitvalpercentages buiten circa 5-40 % of
#'   rendement van 0 % zijn een signaal om de pipelinestap te controleren.
#' - **Reproduceerbaarheid**: hetzelfde invoerbestand moet op elk moment
#'   identieke cijfers opleveren. Het attribuut `gegenereerd_op` registreert
#'   het tijdstip zodat runs vergeleken kunnen worden.
#' - **Optionele bestanden**: als VLPBEK of VAKHAVW is geladen, moeten de
#'   bijbehorende kolommen (`pct_bekostigd`, `gem_eindcijfer`) voor de meeste
#'   rijen gevuld zijn. Kolommen die volledig leeg zijn duiden op een
#'   koppelfout.
#'
#' @param indicatoren Tibble zoals gemaakt door [combineer_indicatoren()],
#'   eventueel aangevuld via [verrijk_met_vakhawv()] of
#'   [verrijk_met_bekostiging()].
#' @param drempel Minimale groepsgrootte. Groepen kleiner dan deze waarde
#'   krijgen NA voor alle kolommen inclusief `n`. Standaard 30.
#'
#' @return Een tibble met kolommen `sector`, `opleidingsvorm`,
#'   `opleidingsniveau`, `inschrijvingsjaar`, `onderdrukt`, `n`,
#'   `pct_uitval_1jr`, `pct_uitval_3jr`, `pct_rendement_3jr`,
#'   `pct_rendement_5jr`, `pct_rendement_8jr`, `pct_studiewissel_1jr`,
#'   `pct_studiewissel_3jr`, `pct_int_student`, `gem_leeftijd_instroom`,
#'   plus optioneel `gem_eindcijfer`, `gem_wiskundecijfer`,
#'   `gem_aantal_vakken`, `pct_bekostigd`, `pct_hoofdinschrijving` en
#'   `pct_herstelbaar` (van de niet-bekostigde rijen: percentage waarvan de
#'   reden een te late aanlevering is en dus hersteld kan worden, zie
#'   [BEKOSTIGINGSTATUS_CODES]). Het attribuut `gegenereerd_op` (POSIXct)
#'   registreert het tijdstip.
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
  heeft_studiewissel  <- "studiewissel_1jr"             %in% names(indicatoren)
  heeft_vakhawv       <- "vakhawv_gemiddeld_eindcijfer"  %in% names(indicatoren)
  heeft_wiskunde      <- "vakhawv_wiskundecijfer"        %in% names(indicatoren)
  heeft_aantal_vakken <- "vakhawv_aantal_vakken"         %in% names(indicatoren)
  heeft_bekostiging   <- "indicatie_bekostigd"           %in% names(indicatoren)
  heeft_hoofdinschr   <- "indicatie_hoofdinschrijving"   %in% names(indicatoren)
  heeft_herstelbaar   <- "indicatie_herstelbaar"         %in% names(indicatoren)

  ## Percentage over de waarneembare rijen: studenten in cohorten waarvan het
  ## meetvenster nog niet in de data zit tellen niet mee in de noemer. Is
  ## niemand in de groep waarneembaar, dan is het percentage NA.
  .pct <- function(x, label) {
    x <- as.character(x)
    waarneembaar <- !is.na(x) & x != NIET_WAARNEEMBAAR
    if (!any(waarneembaar)) {
      return(NA_real_)
    }
    mean(x[waarneembaar] == label) * 100
  }

  .groepeer <- function(data) {
    data |>
      dplyr::summarise(
        n                    = dplyr::n(),
        pct_uitval_1jr       = .pct(uitval_1jr,  "Uitgevallen binnen 1 jaar"),
        pct_uitval_3jr       = .pct(uitval_3jr,  "Uitgevallen binnen 3 jaar"),
        pct_rendement_3jr    = .pct(rendement_3jr, "Diploma binnen 3 jaar"),
        pct_rendement_5jr    = .pct(rendement_5jr, "Diploma binnen 5 jaar"),
        pct_rendement_8jr    = .pct(rendement_8jr, "Diploma binnen 8 jaar"),
        pct_studiewissel_1jr = if (heeft_studiewissel) .pct(studiewissel_1jr, "Gewisseld binnen 1 jaar") else NA_real_,
        pct_studiewissel_3jr = if (heeft_studiewissel) .pct(studiewissel_3jr, "Gewisseld binnen 3 jaar") else NA_real_,
        pct_int_student      = .pct(int_student, "internationale student"),
        gem_leeftijd_instroom = mean(leeftijd_bij_instroom, na.rm = TRUE),
        gem_eindcijfer        = if (heeft_vakhawv)      mean(vakhawv_gemiddeld_eindcijfer, na.rm = TRUE) else NA_real_,
        gem_wiskundecijfer    = if (heeft_wiskunde)      mean(vakhawv_wiskundecijfer,       na.rm = TRUE) else NA_real_,
        gem_aantal_vakken     = if (heeft_aantal_vakken) mean(vakhawv_aantal_vakken,        na.rm = TRUE) else NA_real_,
        pct_bekostigd         = if (heeft_bekostiging)   mean(indicatie_bekostigd,          na.rm = TRUE) * 100 else NA_real_,
        pct_hoofdinschrijving = if (heeft_hoofdinschr)   mean(indicatie_hoofdinschrijving,  na.rm = TRUE) * 100 else NA_real_,
        ## indicatie_herstelbaar is NA voor bekostigde rijen, dus na.rm=TRUE
        ## beperkt dit al automatisch tot de niet-bekostigde rijen.
        pct_herstelbaar       = if (heeft_herstelbaar)   mean(indicatie_herstelbaar,        na.rm = TRUE) * 100 else NA_real_,
        .groups = "drop"
      )
  }

  ## Per sector x opleidingsvorm x opleidingsniveau x instroomjaar
  per_groep <- indicatoren |>
    dplyr::group_by(sector, opleidingsvorm, opleidingsniveau, inschrijvingsjaar) |>
    .groepeer()

  ## Totaalrij per instroomjaar voor validatie tegen DUO-publicaties
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

  ## Privacyonderdrukking: groepen kleiner dan drempel krijgen nergens een
  ## waarde, ook niet voor n. De kolom `onderdrukt` laat zien welke rijen
  ## zijn weggelaten zonder iets over de werkelijke omvang te onthullen.
  te_klein <- resultaat$n < drempel

  uitkomstkolommen <- c(
    "n",
    "pct_uitval_1jr", "pct_uitval_3jr",
    "pct_rendement_3jr", "pct_rendement_5jr", "pct_rendement_8jr",
    "pct_studiewissel_1jr", "pct_studiewissel_3jr",
    "pct_int_student", "gem_leeftijd_instroom",
    "gem_eindcijfer", "gem_wiskundecijfer", "gem_aantal_vakken",
    "pct_bekostigd", "pct_hoofdinschrijving", "pct_herstelbaar"
  )
  aanwezige_uitkomsten <- intersect(uitkomstkolommen, names(resultaat))

  resultaat <- resultaat |>
    dplyr::mutate(
      onderdrukt = te_klein,
      dplyr::across(
        dplyr::all_of(aanwezige_uitkomsten),
        ~ dplyr::if_else(te_klein, NA_real_, as.double(.x))
      ),
      .before = "n"
    )

  ## Verwijder optionele kolommen die volledig leeg zijn (optioneel bestand
  ## was niet geladen of koppeling leverde geen matches op)
  altijd_na  <- function(col) all(is.na(resultaat[[col]]))
  optioneel  <- c(
    "pct_studiewissel_1jr", "pct_studiewissel_3jr",
    "gem_eindcijfer", "gem_wiskundecijfer", "gem_aantal_vakken",
    "pct_bekostigd", "pct_hoofdinschrijving", "pct_herstelbaar"
  )
  resultaat  <- dplyr::select(
    resultaat, -dplyr::all_of(Filter(altijd_na, optioneel))
  )

  attr(resultaat, "gegenereerd_op") <- Sys.time()
  resultaat
}
