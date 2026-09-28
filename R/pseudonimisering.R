## Persoonsnummers in 1cijferho-uitvoer
##
## 1cijferho kan de persoonsnummers in de EV- en VAKHAVW-bestanden op drie
## manieren uitleveren: met het BSN behouden, omgezet naar studentnummer, of
## gepseudonimiseerd. Het VLPBEK-bestand komt rechtstreeks van DUO en bevat
## altijd het echte BSN of onderwijsnummer. Koppelen met VLPBEK kan dus alleen
## als de 1cijferho-uitvoer het BSN behoudt. staat1cho pseudonimiseert of
## vertaalt zelf niets: de gebruiker kiest de juiste uitvoer in 1cijferho.

## Instructie die in foutmeldingen, het dashboard en de documentatie terugkomt
INSTRUCTIE_BSN <- paste(
  "Gebruik voor koppeling met VLPBEK de 1cijferho-uitvoer waarin het BSN",
  "behouden is (niet gepseudonimiseerd en niet omgezet naar studentnummer)."
)

#' Is een ID-kolom gepseudonimiseerd door 1cijferho?
#'
#' Herkent de pseudoniemen die 1cijferho kan maken: 64 hexadecimale tekens.
#' Een gepseudonimiseerd 1CHO-bestand kan niet gekoppeld worden aan een
#' VLPBEK-bestand, dat altijd echte BSN's bevat. Gebruik daarvoor de
#' 1cijferho-uitvoer waarin het BSN behouden is.
#'
#' @param x Vector met persoonsnummers, bijv. `basis$persoonsgebonden_nummer`
#'
#' @return `TRUE` als alle niet-lege waarden pseudoniemen zijn, anders `FALSE`
#'
#' @examples
#' is_gepseudonimiseerd(c("123456789", "987654321"))
#' is_gepseudonimiseerd(strrep("a1", 32))
#' @export
is_gepseudonimiseerd <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & x != ""]
  length(x) > 0 && all(grepl("^[0-9a-f]{64}$", x))
}

## Controleer dat beide kanten van een koppeling hetzelfde soort ID gebruiken.
## Een mismatch geeft altijd 0% koppeling; beter meteen een duidelijke fout.
controleer_id_soort <- function(indicatoren_ids, bron_ids, bron, hint) {
  links <- is_gepseudonimiseerd(indicatoren_ids)
  rechts <- is_gepseudonimiseerd(bron_ids)
  if (links && !rechts) {
    cli::cli_abort(c(
      "Het 1CHO-bestand is gepseudonimiseerd, het {bron}-bestand niet.",
      "i" = hint
    ))
  }
  if (!links && rechts) {
    cli::cli_abort(c(
      "Het {bron}-bestand is gepseudonimiseerd, het 1CHO-bestand niet.",
      "i" = "Gebruik voor het 1CHO- en {bron}-bestand dezelfde 1cijferho-uitvoer."
    ))
  }
  invisible(TRUE)
}
