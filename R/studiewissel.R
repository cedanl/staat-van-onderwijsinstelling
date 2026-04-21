bereken_studiewissel <- function(
  basisbestand,
  cohorten_instroom,
  diploma_behaald,
  uitval_indicatoren
) {
  uitval_1_3_jaar <- uitval_indicatoren |>
    select(persoonsgebonden_nummer, uitval_xjr) |>
    mutate(
      uitval_een_drie = factor(case_when(
        uitval_xjr == 1 ~ "binnen 1 jaar",
        uitval_xjr > 1 & uitval_xjr <= 3 ~ "binnen 2- en 3de jaar",
        TRUE ~ "niet uitgevallen eerste drie jaar"
      ))
    )

  zittend_per_cohort <- cohorten_instroom |>
    select(-soort_diploma_instelling_label) |>
    left_join(diploma_behaald, by = "persoonsgebonden_nummer") |>
    select(
      persoonsgebonden_nummer,
      inschrijvingsjaar,
      eerstejaar_instelling,
      jaar_eerste_diploma,
      opleidingsvorm_label
    ) |>
    mutate(
      diploma_na_x_jaar = jaar_eerste_diploma - eerstejaar_instelling + 1,
      diploma_een_drie = factor(case_when(
        diploma_na_x_jaar <= 1 ~ "binnen 1 jaar of daarvoor",
        diploma_na_x_jaar > 1 &
          diploma_na_x_jaar <= 3 ~ "binnen 2- en 3de jaar",
        TRUE ~ "geen diploma in eerste drie jaar"
      ))
    ) |>
    left_join(uitval_1_3_jaar, by = "persoonsgebonden_nummer")

  zittend_1jr <- zittend_per_cohort |>
    filter(diploma_een_drie != "binnen 1 jaar of daarvoor") |>
    filter(uitval_een_drie != "binnen 1 jaar")

  zittend_3jr <- zittend_per_cohort |>
    filter(diploma_een_drie != "binnen 1 jaar of daarvoor") |>
    filter(diploma_een_drie != "binnen 2- en 3de jaar") |>
    filter(uitval_een_drie != "binnen 1 jaar") |>
    filter(uitval_een_drie != "binnen 2- en 3de jaar")

  wissel_1jr <- bereken_wissel_xjr(
    basisbestand = basisbestand,
    zittend = zittend_1jr,
    verblijfsjaren = c(1, 2),
    doeljaar = 2,
    label_gewisseld = "Gewisseld binnen 1 jaar",
    label_niet = "Niet gewisseld binnen 1 jaar",
    suffix = "1jr"
  )

  wissel_3jr <- bereken_wissel_xjr(
    basisbestand = basisbestand,
    zittend = zittend_3jr,
    verblijfsjaren = c(1, 4),
    doeljaar = 4,
    label_gewisseld = "Gewisseld binnen 3 jaar",
    label_niet = "Niet gewisseld binnen 3 jaar",
    suffix = "3jr"
  )

  cohorten_instroom |>
    left_join(wissel_1jr, by = "persoonsgebonden_nummer") |>
    left_join(wissel_3jr, by = "persoonsgebonden_nummer") |>
    mutate(
      studiewissel_1jr = case_when(
        !(persoonsgebonden_nummer %in%
          zittend_1jr$persoonsgebonden_nummer) ~ "Geen switch bepaald",
        TRUE ~ studiewissel_1jr
      ),
      studiewissel_3jr = case_when(
        !(persoonsgebonden_nummer %in%
          zittend_3jr$persoonsgebonden_nummer) ~ "Geen switch bepaald",
        TRUE ~ studiewissel_3jr
      )
    ) |>
    mutate(
      studiewissel_1jr = fct_na_value_to_level(
        studiewissel_1jr,
        "Niet gewisseld binnen 1 jaar"
      ),
      studiewissel_3jr = fct_na_value_to_level(
        studiewissel_3jr,
        "Niet gewisseld binnen 3 jaar"
      )
    ) |>
    select(
      persoonsgebonden_nummer,
      starts_with("studiewissel"),
      contains("switch")
    )
}

## Hulpfunctie: berekent wissel voor een specifiek verblijfsjaar-paar
bereken_wissel_xjr <- function(
  basisbestand,
  zittend,
  verblijfsjaren,
  doeljaar,
  label_gewisseld,
  label_niet,
  suffix
) {
  bewerkt <- basisbestand |>
    filter(persoonsgebonden_nummer %in% zittend$persoonsgebonden_nummer) |>
    filter(
      verblijfsjaar_actuele_instelling %in% verblijfsjaren,
      soort_inschrijving_actuele_instelling_label ==
        "hoofdinschrijving binnen het domein actuele instelling"
    )

  freq <- bewerkt |> count(persoonsgebonden_nummer)

  if (any(freq$n > 2)) {
    rlang::abort(
      "Meer dan twee regels per student in het switchcohortbestand gevonden!"
    )
  }

  persnr_2jr <- freq |> filter(n == 2)

  dubbel <- bewerkt |>
    filter(persoonsgebonden_nummer %in% persnr_2jr$persoonsgebonden_nummer)

  if (
    sum(dubbel$verblijfsjaar_actuele_instelling == verblijfsjaren[1]) !=
      sum(dubbel$verblijfsjaar_actuele_instelling == verblijfsjaren[2])
  ) {
    rlang::abort(paste(
      "Aantal verblijfsjaren",
      verblijfsjaren[1],
      "en",
      verblijfsjaren[2],
      "is niet gelijk"
    ))
  }

  wissel_col <- paste0("studiewissel_", suffix)
  opl_col <- paste0("opleidingscode_na_switch", suffix)
  vorm_col <- paste0("opleidingsvorm_na_switch", suffix)
  niveau_col <- paste0("opleidingsniveau_na_switch", suffix)
  sector_col <- paste0("HBOsector_na_switch", suffix)

  dubbel |>
    arrange(persoonsgebonden_nummer, verblijfsjaar_actuele_instelling) |>
    mutate(
      persnr_dubbel = dplyr::if_else(
        persoonsgebonden_nummer == lag(persoonsgebonden_nummer),
        "zelfde student",
        "andere student"
      ),
      opl_dubbel = dplyr::if_else(
        opleiding_actueel_equivalent == lag(opleiding_actueel_equivalent),
        "zelfde opleiding",
        "andere opleiding"
      ),
      !!wissel_col := dplyr::if_else(
        verblijfsjaar_actuele_instelling == doeljaar &
          persnr_dubbel == "zelfde student" &
          opl_dubbel == "andere opleiding",
        label_gewisseld,
        label_niet
      ),
      verschil_kalenderjaren = inschrijvingsjaar - lag(inschrijvingsjaar),
      !!opl_col := dplyr::if_else(
        .data[[wissel_col]] == label_gewisseld,
        as.character(opleiding_actueel_equivalent),
        NA_character_
      ),
      !!vorm_col := dplyr::if_else(
        .data[[wissel_col]] == label_gewisseld,
        as.character(opleidingsvorm),
        NA_character_
      ),
      !!niveau_col := dplyr::if_else(
        .data[[wissel_col]] == label_gewisseld,
        as.character(type_hoger_onderwijs_binnen_soort_hoger_onderwijs),
        NA_character_
      ),
      !!sector_col := dplyr::if_else(
        .data[[wissel_col]] == label_gewisseld,
        as.character(croho_onderdeel_actuele_opleiding),
        NA_character_
      )
    ) |>
    mutate(across(
      c(!!wissel_col, !!opl_col, !!vorm_col, !!niveau_col, !!sector_col),
      factor
    )) |>
    select(
      persoonsgebonden_nummer,
      verschil_kalenderjaren,
      !!wissel_col,
      !!opl_col,
      !!vorm_col,
      !!niveau_col,
      !!sector_col
    ) |>
    filter(
      .data[[wissel_col]] == label_gewisseld,
      verschil_kalenderjaren == (doeljaar - 1)
    )
}
