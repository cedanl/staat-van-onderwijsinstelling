#' Maak een bestand met het vroegst behaalde diploma per student (of inschrijving)
#'
#' Filtert het basisbestand op diplomasoorten die gelden als afgeronde opleiding
#' en behoudt per sleutel alleen het eerste diploma op basis van diplomajaar.
#'
#' @param basisbestand Tibble zoals gemaakt door [maak_basisbestand()]
#' @param niveau Analyseniveau: `"student"` (standaard) of `"inschrijving"`.
#'   Bepaalt de sleutel waarop gededupliceerd wordt.
#'
#' @return Een tibble met één rij per student (bij `niveau = "student"`) of per
#'   student-opleidingcombinatie (bij `niveau = "inschrijving"`), met kolommen
#'   voor de sleutel(s), `jaar_eerste_diploma`, `verblijfsjaar_eerste_diploma`
#'   en `diploma`. Gooit een fout bij dubbele sleutelcombinaties.
#'
#' @examples
#' basis <- tibble::tibble(
#'   persoonsgebonden_nummer = c("S1", "S2"),
#'   soort_diploma_instelling = c(
#'     "Hoofd-bachelor-diploma binnen de actuele instelling",
#'     NA_character_
#'   ),
#'   diplomajaar = c(2022L, NA_integer_),
#'   verblijfsjaar_actuele_instelling = c(3L, 1L)
#' )
#' maak_diploma_behaald(basis)
#' @export
maak_diploma_behaald <- function(basisbestand, niveau = "student") {
  diplomas <- c(
    "Hoofd-bachelor-diploma binnen de actuele instelling",
    "Neven-bachelor-diploma binnen de actuele instelling",
    "Hoofd-master-diploma binnen de actuele instelling",
    "Neven-master-diploma binnen de actuele instelling",
    "Hoofd-doctoraal-diploma binnen de actuele instelling",
    "Neven-doctoraal-diploma binnen de actuele instelling",
    "Hoofddiploma beroepsfase/voortgezet binnen de actuele instelling",
    "Nevendiploma beroepsfase/voortgezet binnen de actuele instelling",
    "Hoofddiploma associate degree binnen de actuele instelling",
    "Nevendiploma associate degree binnen de actuele instelling",
    "Hoofddiploma postinitiele master binnen de actuele instelling",
    "Nevendiploma postinitiele master binnen de actuele instelling"
  )

  sleutels <- niveau_sleutels(niveau)

  diploma_behaald <- basisbestand |>
    dplyr::filter(soort_diploma_instelling %in% diplomas) |>
    ## diplomajaar == 0 betekent geen jaar geregistreerd in de 1CHO-data
    dplyr::mutate(diplomajaar = dplyr::na_if(diplomajaar, 0)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(sleutels))) |>
    dplyr::arrange(diplomajaar, .by_group = TRUE) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(sleutels)), .keep_all = TRUE) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      jaar_eerste_diploma = diplomajaar,
      verblijfsjaar_eerste_diploma = verblijfsjaar_actuele_instelling,
      diploma = "Diploma behaald (excl. propedeuse)"
    ) |>
    dplyr::select(
      dplyr::all_of(sleutels),
      jaar_eerste_diploma,
      verblijfsjaar_eerste_diploma,
      diploma
    )

  if (anyDuplicated(diploma_behaald[sleutels]) > 0) {
    rlang::abort("Dubbele sleutelcombinaties in het diplomabestand gevonden!")
  }

  diploma_behaald
}

#' Bereken rendementsindicatoren per cohort
#'
#' Koppelt diplomagegevens aan het instroomcohort en berekent of een student
#' binnen 3, 5 of 8 jaar een diploma heeft behaald.
#'
#' @param cohorten_instroom Tibble zoals gemaakt door [maak_instroom_cohort()]
#' @param diploma_behaald Tibble zoals gemaakt door [maak_diploma_behaald()]
#' @param niveau Analyseniveau: `"student"` (standaard) of `"inschrijving"`.
#'   Moet overeenkomen met het niveau waarop `cohorten_instroom` en
#'   `diploma_behaald` zijn aangemaakt.
#'
#' @return Een tibble met kolommen voor de sleutel(s), `eerstejaar_instelling`,
#'   `jaar_eerste_diploma`, `verblijfsjaar_eerste_diploma`, `diploma`,
#'   `rendement_xjaar`, en factorkolommen `rendement_3jr`, `rendement_5jr`,
#'   `rendement_8jr`
#'
#' @examples
#' cohort <- tibble::tibble(
#'   persoonsgebonden_nummer = c("S1", "S2"),
#'   eerstejaar_instelling = 2020L
#' )
#' diploma <- tibble::tibble(
#'   persoonsgebonden_nummer = "S1",
#'   jaar_eerste_diploma = 2022L,
#'   verblijfsjaar_eerste_diploma = 3L,
#'   diploma = "Diploma behaald (excl. propedeuse)"
#' )
#' bereken_rendement(cohort, diploma)
#' @export
bereken_rendement <- function(
  cohorten_instroom,
  diploma_behaald,
  niveau = "student"
) {
  sleutels <- niveau_sleutels(niveau)

  cohorten_instroom |>
    dplyr::left_join(diploma_behaald, by = sleutels) |>
    dplyr::select(
      dplyr::all_of(sleutels),
      eerstejaar_instelling,
      jaar_eerste_diploma,
      verblijfsjaar_eerste_diploma,
      diploma
    ) |>
    dplyr::mutate(
      rendement_xjaar = jaar_eerste_diploma - eerstejaar_instelling + 1,

      rendement_3jr = dplyr::case_when(
        is.na(jaar_eerste_diploma) ~ "Geen diploma",
        ## diplomajaar voor instroomjaar kan voorkomen door data-inconsistentie
        jaar_eerste_diploma <
          eerstejaar_instelling ~ "Onbekend (diplomajaar voor instroomjaar)",
        rendement_xjaar <= 3 ~ "Diploma binnen 3 jaar",
        rendement_xjaar > 3 ~ "Diploma na 3 jaar"
      ),

      rendement_5jr = dplyr::case_when(
        is.na(jaar_eerste_diploma) ~ "Geen diploma",
        jaar_eerste_diploma <
          eerstejaar_instelling ~ "Onbekend (diplomajaar voor instroomjaar)",
        rendement_xjaar <= 5 ~ "Diploma binnen 5 jaar",
        rendement_xjaar > 5 ~ "Diploma na 5 jaar"
      ),

      rendement_8jr = dplyr::case_when(
        is.na(jaar_eerste_diploma) ~ "Geen diploma",
        jaar_eerste_diploma <
          eerstejaar_instelling ~ "Onbekend (diplomajaar voor instroomjaar)",
        rendement_xjaar <= 8 ~ "Diploma binnen 8 jaar",
        rendement_xjaar > 8 ~ "Diploma na 8 jaar"
      )
    ) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("rendement"), as.factor))
}
