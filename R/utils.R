niveau_sleutels <- function(niveau) {
  niveau <- rlang::arg_match0(niveau, c("student", "inschrijving"))
  if (niveau == "inschrijving") {
    c("persoonsgebonden_nummer", "opleiding_actueel_equivalent")
  } else {
    "persoonsgebonden_nummer"
  }
}

## 1CHO-kolommen die als geheel getal gebruikt worden. Alle andere kolommen
## blijven tekst (zie maak_basisbestand()).
INTEGER_KOLOMMEN_1CHO <- c(
  "inschrijvingsjaar",
  "verblijfsjaar_actuele_instelling",
  "verblijfsjaar_actuele_opleiding_instelling",
  "diplomajaar",
  "leeftijd_per_peildatum_1_oktober"
)

## Zet de opgegeven (aanwezige) kolommen om naar integer en waarschuwt als
## daarbij waarden verloren gaan, zodat een afwijkende aanlevering niet stil
## tot NA's leidt.
zet_om_naar_integer <- function(data, kolommen) {
  for (kol in intersect(kolommen, names(data))) {
    ruw <- trimws(as.character(data[[kol]]))
    ruw[ruw == ""] <- NA_character_
    omgezet <- suppressWarnings(as.integer(ruw))
    verloren <- sum(!is.na(ruw) & is.na(omgezet))
    if (verloren > 0) {
      voorbeelden <- unique(ruw[!is.na(ruw) & is.na(omgezet)])
      voorbeelden <- voorbeelden[seq_len(min(3, length(voorbeelden)))]
      cli::cli_warn(c(
        "{verloren} waarde{?n} in kolom {.field {kol}} {?is/zijn} geen geheel getal en {?wordt/worden} NA.",
        "i" = "Voorbeelden: {.val {voorbeelden}}"
      ))
    }
    data[[kol]] <- omgezet
  }
  data
}

ONBEKENDE_POSTCODES <- c("0010", "0020", "0030", "0040")

## Hercodeer factorniveaus die daadwerkelijk voorkomen. In tegenstelling tot
## forcats::fct_recode() geeft dit geen waarschuwing als een niveau in deze
## aanlevering niet voorkomt. `nieuw_oud` is een named vector (nieuw = oud);
## niveaus in `verwijder` worden NA.
hercodeer <- function(x, nieuw_oud = character(), verwijder = character()) {
  if (!is.factor(x)) {
    x <- factor(x)
  }
  aanwezig <- nieuw_oud[nieuw_oud %in% levels(x)]
  if (length(aanwezig) > 0) {
    x <- forcats::fct_recode(x, !!!aanwezig)
  }
  if (any(verwijder %in% levels(x))) {
    x[x %in% verwijder] <- NA
    x <- droplevels(x)
  }
  x
}
