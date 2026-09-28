## Pseudonimisering, compatibel met 1cijferho (src/eencijferho/utils/
## pseudonymizer.py). 1cijferho vervangt BSN, persoonsgebonden nummer en
## onderwijsnummer in EV- en VAKHAVW-bestanden door
## HMAC-SHA256(sleutel, waarde) als hex. VLPBEK gaat niet door die stap, dus
## om te kunnen koppelen passen we hier exact dezelfde bewerking toe.

## Zelfde omgevingsvariabele en minimale sleutellengte als 1cijferho, zodat
## een instelling de sleutel maar op één plek hoeft in te stellen.
SLEUTEL_ENV <- "EENCIJFERHO_ENCRYPT_KEY"
MIN_SLEUTEL_BYTES <- 64L

#' Is een ID-kolom gepseudonimiseerd door 1cijferho?
#'
#' Herkent de HMAC-SHA256-pseudoniemen die 1cijferho maakt: 64 hexadecimale
#' tekens. Handig om te bepalen of een VLPBEK-bestand met
#' `pseudonimiseer = TRUE` moet worden ingelezen (zie [lees_bekostiging()]).
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

## Bepaal de sleutel: expliciete sleutel -> sleutelbestand -> omgevingsvariabele.
## Zelfde volgorde en eisen als 1cijferho. De sleutel wordt nooit gelogd.
laad_sleutel <- function(sleutel = NULL, sleutelbestand = NULL) {
  if (is.null(sleutel) && !is.null(sleutelbestand)) {
    sleutel <- trimws(paste(readLines(sleutelbestand, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  }
  if (is.null(sleutel) || !nzchar(sleutel)) {
    sleutel <- Sys.getenv(SLEUTEL_ENV)
  }
  if (!nzchar(sleutel)) {
    cli::cli_abort(c(
      "Geen pseudonimiseringssleutel gevonden.",
      "i" = "Het 1CHO-bestand is gepseudonimiseerd door 1cijferho. Vul dezelfde sleutel in bij 'Pseudonimiseringssleutel' in het dashboard, of geef hem mee via {.arg sleutel}, {.arg sleutelbestand} of de omgevingsvariabele {.envvar {SLEUTEL_ENV}}."
    ))
  }
  sleutel_raw <- charToRaw(enc2utf8(sleutel))
  if (length(sleutel_raw) < MIN_SLEUTEL_BYTES) {
    cli::cli_abort(
      "Pseudonimiseringssleutel is te kort: {length(sleutel_raw)} bytes, minimaal {MIN_SLEUTEL_BYTES} (zoals in 1cijferho)."
    )
  }
  sleutel_raw
}

## HMAC-SHA256 als hex per waarde, identiek aan 1cijferho's
## pseudonymize_value(): lege waarden blijven leeg (NA).
pseudonimiseer_ids <- function(x, sleutel_raw) {
  x <- as.character(x)
  uit <- rep(NA_character_, length(x))
  gevuld <- !is.na(x) & x != ""
  if (any(gevuld)) {
    uit[gevuld] <- as.character(openssl::sha256(enc2utf8(x[gevuld]), key = sleutel_raw))
  }
  uit
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
      "i" = "Gebruik beide bestanden gepseudonimiseerd met dezelfde sleutel, of beide niet."
    ))
  }
  invisible(TRUE)
}
