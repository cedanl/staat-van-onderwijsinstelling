bereken_uitval <- function(
  basisbestand,
  diploma_behaald,
  cohorten_instroom,
  jaar
) {
  uitstroom <- basisbestand |>
    group_by(persoonsgebonden_nummer) |>
    arrange(
      desc(inschrijvingsjaar),
      soort_inschrijving_actuele_instelling,
      .by_group = TRUE
    ) |>
    distinct(persoonsgebonden_nummer, .keep_all = TRUE) |>
    ungroup() |>
    mutate(
      laatste_jaar_inschrijving = case_when(
        inschrijvingsjaar == jaar - 1 ~ NA_real_,
        is.na(inschrijvingsjaar) ~ NA_real_,
        TRUE ~ as.numeric(inschrijvingsjaar)
      )
    ) |>
    select(
      persoonsgebonden_nummer,
      inschrijvingsjaar,
      laatste_jaar_inschrijving
    )

  if (any(duplicated(uitstroom$persoonsgebonden_nummer))) {
    rlang::abort("Dubbele studentnummers in het uitvalbestand gevonden!")
  }

  uitval <- uitstroom |>
    left_join(diploma_behaald, by = "persoonsgebonden_nummer") |>
    left_join(
      cohorten_instroom |>
        select(persoonsgebonden_nummer, eerstejaar_instelling),
      by = "persoonsgebonden_nummer"
    ) |>
    mutate(
      status = factor(case_when(
        diploma == "Diploma behaald (excl. propedeuse)" ~ "Diploma behaald",
        inschrijvingsjaar == jaar - 1 ~ "Zittend",
        TRUE ~ "Uitgevallen"
      )),
      uitval_xjr = case_when(
        status == "Uitgevallen" ~ laatste_jaar_inschrijving +
          1 -
          eerstejaar_instelling
      ),
      uitval_1jr = factor(case_when(
        uitval_xjr == 1 ~ "Uitgevallen binnen 1 jaar",
        TRUE ~ "Na 1 jaar nog ingeschreven of diploma behaald"
      )),
      uitval_3jr = factor(case_when(
        uitval_xjr <= 3 ~ "Uitgevallen binnen 3 jaar",
        TRUE ~ "Na 3 jaar nog ingeschreven of diploma behaald"
      ))
    ) |>
    select(
      persoonsgebonden_nummer,
      laatste_jaar_inschrijving,
      diploma,
      status,
      starts_with("uitval")
    )

  if (any(is.na(uitval$status))) {
    rlang::abort("Niet alle statussen zijn gevuld")
  }

  uitval
}
