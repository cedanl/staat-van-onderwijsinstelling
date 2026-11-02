#' Maak in één stap het analysebestand uit een 1CHO-bestand
#'
#' Voert de hele rekenketen uit: [maak_basisbestand()],
#' [maak_instroom_cohort()], [maak_diploma_behaald()], [bereken_rendement()],
#' [bereken_uitval()], [bereken_studiewissel()] (alleen op studentniveau) en
#' [combineer_indicatoren()]. Het dashboard rekent met deze functie, dus de
#' uitkomst is gelijk aan de CSV-download van het dashboard. Gebruik deze
#' functie om staat1cho in een eigen pipeline op te nemen; de losse functies
#' zijn er voor wie een tussenstap wil inzien of aanpassen.
#'
#' @param invoer Pad naar de `_enriched.csv` van 1cijferho, of een data frame
#'   met dezelfde kolommen (bijvoorbeeld al ingelezen uit parquet of een
#'   database).
#' @param niveau Analyseniveau: `"student"` (standaard) of `"inschrijving"`.
#'   Op studentniveau telt elke student één keer, bij de opleiding waarin die
#'   begon. Op inschrijvingsniveau is elke opleiding een eigen cohort.
#'
#' @return Een tibble met één rij per student (of per student-opleiding bij
#'   `niveau = "inschrijving"`), met attributen `niveau`, `peildatum` en, als
#'   DUO studenten dubbel leverde, `dubbel` (hun persoonsgebonden nummers).
#'   Kolommen:
#'   \describe{
#'     \item{persoonsgebonden_nummer}{Persoonsnummer uit het 1CHO-bestand.}
#'     \item{inschrijvingsjaar}{Instroomjaar (het cohort).}
#'     \item{geslacht, leeftijd_bij_instroom, int_student, indicatie_EER}{
#'       Persoonskenmerken in het instroomjaar.}
#'     \item{opleidingscode, opleidingsnaam, opleidingsvorm, opleidingsniveau,
#'       sector, locatie}{Opleiding van instroom. `sector` is het
#'       CROHO-onderdeel.}
#'     \item{vooropleiding}{havo, vwo, mbo, ho, buitenlands, overig of
#'       onbekend.}
#'     \item{eerstejaars_ho}{"eerstejaars HO", "eerder in HO" of "onbekend".}
#'     \item{postcode4_student_1okt, postcode4_vooropleiding_voorHO}{Postcode
#'       (4 cijfers) van de student en van de vooropleiding.}
#'     \item{status}{Diploma behaald, Zittend of Uitgevallen op de peildatum.}
#'     \item{soortdiploma, opleidingscode_diploma,
#'       diploma_in_instroomopleiding}{Eerste diploma aan de instelling en of
#'       dat in de instroomopleiding is behaald.}
#'     \item{rendement_3jr, rendement_5jr, rendement_8jr}{Diploma binnen x
#'       jaar, of "Nog niet waarneembaar".}
#'     \item{uitval_xjr, uitval_1jr, uitval_3jr}{Jaar van uitval na instroom,
#'       en uitval binnen 1 en 3 jaar.}
#'     \item{studiewissel_1jr, studiewissel_3jr, ..._na_switch1jr,
#'       ..._na_switch3jr}{Alleen op studentniveau: wissel van opleiding en de
#'       nieuwe opleiding.}
#'     \item{rendement, uitval, studiewissel}{Samenvattingen in één kolom.}
#'   }
#'   De precieze definities staan in [DEFINITIES].
#'
#' @examples
#' synth <- maak_synthetische_1cho(n_per_jaar = 20, jaren = 2018:2023)
#' analyse <- maak_analysebestand(synth)
#' table(analyse$inschrijvingsjaar, analyse$rendement_3jr)
#' @export
maak_analysebestand <- function(invoer, niveau = c("student", "inschrijving")) {
  niveau <- match.arg(niveau)
  basis <- maak_basisbestand(invoer)
  soort_ho <- unique(basis$soort_hoger_onderwijs)
  laatste_jaar <- max(basis$inschrijvingsjaar, na.rm = TRUE)

  cohort <- maak_instroom_cohort(basis, soort_ho, niveau = niveau)
  diploma <- maak_diploma_behaald(basis, niveau = niveau)
  rendement <- bereken_rendement(
    cohort, diploma, niveau = niveau, laatste_jaar = laatste_jaar
  )
  uitval <- bereken_uitval(
    basis, diploma, cohort, jaar = laatste_jaar + 1L, niveau = niveau
  )
  wissel <- if (niveau == "student") {
    bereken_studiewissel(basis, cohort, diploma, uitval)
  }
  result <- combineer_indicatoren(
    cohort, rendement, uitval, wissel, niveau = niveau
  )

  ## Opleidingsnaam erbij, zodat het bestand zonder codetabel leesbaar is
  if ("opleidingscode_naam_opleiding" %in% names(basis)) {
    namen <- dplyr::distinct(
      basis,
      opleidingscode = as.character(opleiding_actueel_equivalent),
      opleidingsnaam = opleidingscode_naam_opleiding
    ) |>
      dplyr::distinct(opleidingscode, .keep_all = TRUE)
    attrs <- attributes(result)[c("niveau", "peildatum")]
    result <- result |>
      dplyr::mutate(opleidingscode_tekst = as.character(opleidingscode)) |>
      dplyr::left_join(namen, by = c("opleidingscode_tekst" = "opleidingscode")) |>
      dplyr::select(-opleidingscode_tekst) |>
      dplyr::relocate(opleidingsnaam, .after = opleidingscode)
    attributes(result)[names(attrs)] <- attrs
  }

  attr(result, "dubbel") <- attr(cohort, "dubbel")
  result
}

#' Sla het analysebestand op als CSV voor Excel
#'
#' Schrijft met puntkomma als scheidingsteken en komma als decimaalteken,
#' zodat een Nederlandse Excel het bestand direct in kolommen en met de
#' juiste letters (UTF-8 met BOM) opent. Ook in
#' Python (`sep=";"`), SPSS of Power BI is het zo in te lezen.
#'
#' Het bestand bevat persoonsnummers en is alleen voor gebruik binnen de
#' instelling. Deel met andere instellingen alleen het benchmarkrapport.
#'
#' @param analysebestand Tibble zoals gemaakt door [maak_analysebestand()].
#' @param pad Pad van het CSV-bestand.
#'
#' @return `pad`, onzichtbaar.
#'
#' @examples
#' synth <- maak_synthetische_1cho(n_per_jaar = 20, jaren = 2018:2023)
#' pad <- tempfile(fileext = ".csv")
#' schrijf_analysebestand(maak_analysebestand(synth), pad)
#' @export
schrijf_analysebestand <- function(analysebestand, pad) {
  readr::write_excel_csv2(analysebestand, pad, na = "")
  invisible(pad)
}
