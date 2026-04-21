maak_basisbestand <- function(pad_invoer) {
  read_csv2(pad_invoer, locale = locale(encoding = "UTF-8")) |>
    mutate(
      verblijfsjaar_actuele_instelling = as.integer(
        verblijfsjaar_actuele_instelling
      ),
      diplomajaar = suppressWarnings(as.integer(diplomajaar)),
      soort_inschrijving_actuele_instelling_label = soort_inschrijving_actuele_instelling,
      geslacht_label = geslacht,
      opleidingsvorm_label = opleidingsvorm,
      indicatie_internationale_student_label = indicatie_internationale_student,
      indicatie_eer_actueel_label = indicatie_eer_actueel,
      croho_onderdeel_actuele_opleiding_label = croho_onderdeel_actuele_opleiding,
      soort_diploma_instelling_label = soort_diploma_instelling
    )
}

maak_instroom_cohort <- function(basisbestand, soort_ho) {
  cohort <- basisbestand |>
    filter(soort_hoger_onderwijs %in% soort_ho) |>
    filter(
      soort_inschrijving_actuele_instelling_label ==
        "hoofdinschrijving binnen het domein actuele instelling",
      verblijfsjaar_actuele_instelling == 1
    ) |>
    mutate(eerstejaar_instelling = inschrijvingsjaar)

  if (any(duplicated(cohort$persoonsgebonden_nummer))) {
    rlang::abort(
      "Dubbele studentnummers in het instroomcohortbestand gevonden!"
    )
  }

  cohort
}
