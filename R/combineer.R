#' Combineer alle indicatoren tot één analysebestand
#'
#' Voegt rendement-, uitval- en studiewisselindicatoren samen met het
#' instroomcohort. Past kolomnamen en factorniveaus aan voor gebruik in
#' rapportages.
#'
#' @param cohorten_instroom Tibble zoals gemaakt door [maak_instroom_cohort()]
#' @param rendement_indicatoren Tibble zoals gemaakt door [bereken_rendement()]
#' @param uitval_indicatoren Tibble zoals gemaakt door [bereken_uitval()]
#' @param studiewissel_indicatoren Tibble zoals gemaakt door
#'   [bereken_studiewissel()]
#'
#' @return Een tibble met één rij per student en gecombineerde indicator-
#'   kolommen, klaar voor rapportage. Bevat o.a. `status`, `rendement`,
#'   `uitval`, `studiewissel` en alle onderliggende deelscores.
#'
#' @export
combineer_indicatoren <- function(
  cohorten_instroom,
  rendement_indicatoren,
  uitval_indicatoren,
  studiewissel_indicatoren
) {
  cohorten_instroom |>
    dplyr::left_join(rendement_indicatoren, by = "persoonsgebonden_nummer") |>
    dplyr::left_join(uitval_indicatoren, by = "persoonsgebonden_nummer") |>
    dplyr::left_join(
      studiewissel_indicatoren,
      by = "persoonsgebonden_nummer"
    ) |>

    dplyr::select(
      inschrijvingsjaar,
      geslacht = geslacht_label,
      locatie = locatie_label,
      opleidingscode = opleiding_actueel_equivalent,
      opleidingsvorm = opleidingsvorm_label,
      opleidingsniveau = type_hoger_onderwijs_binnen_soort_hoger_onderwijs,
      int_student = indicatie_internationale_student_label,
      indicatie_EER = indicatie_eer_actueel_label,
      sector = croho_onderdeel_actuele_opleiding_label,
      leeftijd_bij_instroom = leeftijd_per_peildatum_1_oktober,
      postcode4_student_1okt = postcodecijfers_student_op_1_oktober,
      postcode4_vooropleiding_voorHO = postcodecijfers_van_de_hoogste_vooropl_voor_het_ho,
      status,
      soortdiploma = soort_diploma_instelling_label,
      rendement_3jr:rendement_8jr,
      uitval_xjr:sector_na_switch3jr
    ) |>

    dplyr::mutate(
      opleidingscode = factor(opleidingscode),
      locatie = factor(locatie),
      opleidingsvorm = factor(opleidingsvorm),
      postcode4_student_1okt = factor(postcode4_student_1okt),
      postcode4_vooropleiding_voorHO = factor(postcode4_vooropleiding_voorHO)
    ) |>

    ## Lange categorielabels inkorten en ongeldige postcodes verwijderen
    dplyr::mutate(
      opleidingsvorm = forcats::fct_recode(
        opleidingsvorm,
        "duaal" = "coöp-student of duaal onderwijs (vanaf het studiejaar 1998-1999)"
      ),
      sector = forcats::fct_recode(
        sector,
        "gedrag & maatschappij" = "gedrag en maatschappij",
        "taal & cultuur" = "taal en cultuur"
      ),
      ## 0010-0040 zijn onbekende postcodewaarden in de 1CHO-data
      postcode4_student_1okt = forcats::fct_recode(
        postcode4_student_1okt,
        NULL = "0010",
        NULL = "0020",
        NULL = "0030",
        NULL = "0040"
      ),
      postcode4_vooropleiding_voorHO = forcats::fct_recode(
        postcode4_vooropleiding_voorHO,
        NULL = "0010",
        NULL = "0020",
        NULL = "0030",
        NULL = "0040"
      ),
      opleidingsniveau = forcats::fct_recode(
        opleidingsniveau,
        NULL = "postinitiele master",
        bachelor = "ba",
        master = "ma"
      )
    ) |>

    ## Samengevatte indicatoren op basis van de gedetailleerde xjr-waarden
    dplyr::mutate(
      uitval = dplyr::case_when(
        uitval_xjr == 1 ~ "Uitgevallen binnen 1 jaar",
        uitval_xjr %in% 2:3 ~ "Uitgevallen in 2e of 3e jaar",
        uitval_xjr > 3 ~ "Uitgevallen na 3 jaar",
        TRUE ~ "Niet uitgevallen"
      ),
      studiewissel = dplyr::case_when(
        studiewissel_1jr ==
          "Gewisseld binnen 1 jaar" ~ "Gewisseld binnen 1 jaar",
        studiewissel_3jr ==
          "Gewisseld binnen 3 jaar" ~ "Gewisseld in het 2e of 3e jaar",
        TRUE ~ "Niet gewisseld"
      ),
      rendement = dplyr::case_when(
        rendement_5jr == "Diploma binnen 5 jaar" ~ "Diploma binnen 5 jaar",
        rendement_8jr == "Diploma binnen 8 jaar" ~ "Diploma binnen 5-8 jaar",
        rendement_8jr == "Diploma na 8 jaar" ~ "Diploma na 8 jaar",
        rendement_8jr == "Geen diploma" ~ "Geen diploma",
        rendement_8jr == "Onbekend (diplomajaar voor instroomjaar)" ~ "Onbekend"
      )
    )
}
