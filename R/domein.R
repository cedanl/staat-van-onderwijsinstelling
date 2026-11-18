#' Koppel een eigen domeinindeling aan het analysebestand
#'
#' Voegt de kolom `domein` toe, direct na `sector`, op basis van een
#' koppeltabel van opleidingscode naar domein. Het pakket kent geen vaste
#' domeinindeling: de indeling is altijd die van de instelling zelf. Eén
#' indeling geldt voor alle cohortjaren. Opleidingscodes zonder domein krijgen
#' `"Onbekend"`, met een waarschuwing.
#'
#' @param analysebestand Een data frame met een kolom `opleidingscode` en
#'   `sector`, zoals de uitvoer van [maak_analysebestand()].
#' @param domeinen De koppeltabel: een data frame of het pad naar een CSV
#'   (`;` als scheidingsteken, UTF-8, met of zonder BOM, zoals
#'   [schrijf_analysebestand()] schrijft) met de kolommen `opleidingscode` en
#'   `domein`. Een opleidingscode mag maar één domein hebben.
#'
#' @return Het analysebestand met een extra kolom `domein` (tekst).
#'
#' @examples
#' analyse <- data.frame(opleidingscode = c("34001", "56000"),
#'                       sector = c("ECONOMIE", "ONDERWIJS"))
#' koppel <- data.frame(opleidingscode = c("34001", "56000"),
#'                      domein = c("Bedrijf", "Mens"))
#' koppel_domeinen(analyse, koppel)
#' @export
koppel_domeinen <- function(analysebestand, domeinen) {
  if (is.character(domeinen) && length(domeinen) == 1) {
    if (!file.exists(domeinen)) {
      cli::cli_abort("Koppeltabel {.file {domeinen}} niet gevonden.")
    }
    domeinen <- utils::read.csv2(domeinen, fileEncoding = "UTF-8-BOM",
                                 colClasses = "character")
  }
  if (!is.data.frame(domeinen) ||
      !all(c("opleidingscode", "domein") %in% names(domeinen))) {
    cli::cli_abort(c(
      "De koppeltabel heeft de kolommen {.field opleidingscode} en {.field domein} nodig.",
      "i" = "Gevonden kolommen: {.val {names(domeinen)}}."
    ))
  }
  if (!"opleidingscode" %in% names(analysebestand)) {
    cli::cli_abort("Het analysebestand heeft geen kolom {.field opleidingscode}.")
  }

  koppel <- dplyr::tibble(
    opleidingscode = trimws(as.character(domeinen$opleidingscode)),
    domein = trimws(as.character(domeinen$domein))
  )
  koppel <- dplyr::distinct(koppel)
  dubbel <- unique(koppel$opleidingscode[duplicated(koppel$opleidingscode)])
  if (length(dubbel) > 0) {
    cli::cli_abort(c(
      "Opleidingscodes komen dubbel voor met een ander domein.",
      "x" = "Dubbel: {.val {dubbel}}."
    ))
  }

  sleutel <- trimws(as.character(analysebestand$opleidingscode))
  domein <- koppel$domein[match(sleutel, koppel$opleidingscode)]
  ontbreekt <- is.na(domein)
  if (any(ontbreekt)) {
    n <- sum(ontbreekt)
    codes <- unique(sleutel[ontbreekt])
    cli::cli_warn(c(
      "{n} student{?en} zonder domein in de koppeltabel; ingedeeld als {.val Onbekend}.",
      "i" = "Opleidingscodes zonder domein: {.val {codes}}."
    ))
    domein[ontbreekt] <- "Onbekend"
  }

  analysebestand$domein <- domein
  dplyr::relocate(analysebestand, "domein", .after = "sector")
}
