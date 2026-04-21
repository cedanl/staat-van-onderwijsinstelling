test_that("maak_instroom_cohort filtert op soort_ho", {
  basisbestand <- tibble(
    persoonsgebonden_nummer = c("A", "B"),
    soort_hoger_onderwijs = c("hbo", "wo"),
    soort_inschrijving_actuele_instelling_label = HOOFD_INSCHRIJVING,
    verblijfsjaar_actuele_instelling = 1L,
    inschrijvingsjaar = 2020
  )

  result <- maak_instroom_cohort(basisbestand, "hbo")

  expect_equal(nrow(result), 1)
  expect_equal(result$persoonsgebonden_nummer, "A")
})

test_that("maak_instroom_cohort behoudt alleen verblijfsjaar 1", {
  basisbestand <- tibble(
    persoonsgebonden_nummer = c("A", "A"),
    soort_hoger_onderwijs = "hbo",
    soort_inschrijving_actuele_instelling_label = HOOFD_INSCHRIJVING,
    verblijfsjaar_actuele_instelling = c(1L, 2L),
    inschrijvingsjaar = c(2020, 2021)
  )

  result <- maak_instroom_cohort(basisbestand, "hbo")

  expect_equal(nrow(result), 1)
  expect_equal(result$eerstejaar_instelling, 2020)
})

test_that("maak_instroom_cohort filtert niet-hoofdinschrijvingen eruit", {
  basisbestand <- tibble(
    persoonsgebonden_nummer = c("A", "B"),
    soort_hoger_onderwijs = "hbo",
    soort_inschrijving_actuele_instelling_label = c(
      HOOFD_INSCHRIJVING,
      "neveninschrijving binnen het domein actuele instelling"
    ),
    verblijfsjaar_actuele_instelling = 1L,
    inschrijvingsjaar = 2020
  )

  result <- maak_instroom_cohort(basisbestand, "hbo")

  expect_equal(nrow(result), 1)
  expect_equal(result$persoonsgebonden_nummer, "A")
})

test_that("maak_instroom_cohort geeft fout bij dubbele persoonsgebonden_nummers", {
  basisbestand <- tibble(
    persoonsgebonden_nummer = c("A", "A"),
    soort_hoger_onderwijs = "hbo",
    soort_inschrijving_actuele_instelling_label = HOOFD_INSCHRIJVING,
    verblijfsjaar_actuele_instelling = 1L,
    inschrijvingsjaar = 2020
  )

  expect_error(maak_instroom_cohort(basisbestand, "hbo"))
})

test_that("maak_instroom_cohort geeft lege tibble terug bij geen matches", {
  basisbestand <- tibble(
    persoonsgebonden_nummer = "A",
    soort_hoger_onderwijs = "wo",
    soort_inschrijving_actuele_instelling_label = HOOFD_INSCHRIJVING,
    verblijfsjaar_actuele_instelling = 1L,
    inschrijvingsjaar = 2020
  )

  result <- maak_instroom_cohort(basisbestand, "hbo")

  expect_equal(nrow(result), 0)
})
