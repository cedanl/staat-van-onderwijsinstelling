maak_basisbestand_wissel <- function(pgn, verblijfsjaren, opleidingen, inschrijvingsjaren) {
  tibble(
    persoonsgebonden_nummer = pgn,
    verblijfsjaar_actuele_instelling = verblijfsjaren,
    soort_inschrijving_actuele_instelling_label = HOOFD_INSCHRIJVING,
    opleiding_actueel_equivalent = opleidingen,
    inschrijvingsjaar = inschrijvingsjaren,
    opleidingsvorm = "voltijd",
    type_hoger_onderwijs_binnen_soort_hoger_onderwijs = "bachelor",
    croho_onderdeel_actuele_opleiding = "techniek"
  )
}

test_that("bereken_wissel_xjr detecteert opleidingswissel correct", {
  ## Student A wisselt (opl1 -> opl2), student B wisselt niet (opl1 -> opl1)
  basisbestand <- bind_rows(
    maak_basisbestand_wissel(c("A", "A"), c(1L, 2L), c("opl1", "opl2"), c(2019, 2020)),
    maak_basisbestand_wissel(c("B", "B"), c(1L, 2L), c("opl1", "opl1"), c(2019, 2020))
  )
  zittend <- tibble(persoonsgebonden_nummer = c("A", "B"))

  result <- bereken_wissel_xjr(
    basisbestand = basisbestand,
    zittend = zittend,
    verblijfsjaren = c(1, 2),
    doeljaar = 2,
    label_gewisseld = "Gewisseld binnen 1 jaar",
    label_niet = "Niet gewisseld binnen 1 jaar",
    suffix = "1jr"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$persoonsgebonden_nummer, "A")
  expect_equal(as.character(result$studiewissel_1jr), "Gewisseld binnen 1 jaar")
})

test_that("bereken_wissel_xjr negeert studenten die niet in zittend zitten", {
  basisbestand <- maak_basisbestand_wissel(
    c("A", "A", "B", "B"),
    c(1L, 2L, 1L, 2L),
    c("opl1", "opl2", "opl1", "opl2"),
    c(2019, 2020, 2019, 2020)
  )
  ## Alleen A zit in de zittend-populatie
  zittend <- tibble(persoonsgebonden_nummer = "A")

  result <- bereken_wissel_xjr(
    basisbestand = basisbestand,
    zittend = zittend,
    verblijfsjaren = c(1, 2),
    doeljaar = 2,
    label_gewisseld = "Gewisseld binnen 1 jaar",
    label_niet = "Niet gewisseld binnen 1 jaar",
    suffix = "1jr"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$persoonsgebonden_nummer, "A")
})

test_that("bereken_wissel_xjr slaat studenten over met verkeerd verschil in kalenderjaren", {
  ## Student A wisselt maar het verschil is 2 jaar in plaats van 1
  basisbestand <- maak_basisbestand_wissel(
    c("A", "A"),
    c(1L, 2L),
    c("opl1", "opl2"),
    c(2018, 2020) ## verschil = 2, maar doeljaar - 1 = 1
  )
  zittend <- tibble(persoonsgebonden_nummer = "A")

  result <- bereken_wissel_xjr(
    basisbestand = basisbestand,
    zittend = zittend,
    verblijfsjaren = c(1, 2),
    doeljaar = 2,
    label_gewisseld = "Gewisseld binnen 1 jaar",
    label_niet = "Niet gewisseld binnen 1 jaar",
    suffix = "1jr"
  )

  expect_equal(nrow(result), 0)
})

test_that("bereken_wissel_xjr geeft fout bij meer dan 2 regels per student", {
  basisbestand <- maak_basisbestand_wissel(
    c("A", "A", "A"),
    c(1L, 1L, 2L),
    c("opl1", "opl1", "opl2"),
    c(2019, 2019, 2020)
  )
  zittend <- tibble(persoonsgebonden_nummer = "A")

  expect_error(
    bereken_wissel_xjr(
      basisbestand = basisbestand,
      zittend = zittend,
      verblijfsjaren = c(1, 2),
      doeljaar = 2,
      label_gewisseld = "Gewisseld binnen 1 jaar",
      label_niet = "Niet gewisseld binnen 1 jaar",
      suffix = "1jr"
    )
  )
})
