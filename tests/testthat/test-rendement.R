test_that("maak_diploma_behaald behoudt alleen afgeronde diploma's", {
  basisbestand <- tibble(
    persoonsgebonden_nummer = c("A", "B"),
    soort_diploma_instelling = c(
      BACHELOR_DIPLOMA,
      "Propedeusediploma binnen de actuele instelling"
    ),
    diplomajaar = c(2022L, 2020L),
    verblijfsjaar_actuele_instelling = c(3L, 1L)
  )

  result <- maak_diploma_behaald(basisbestand)

  expect_equal(nrow(result), 1)
  expect_equal(result$persoonsgebonden_nummer, "A")
})

test_that("maak_diploma_behaald zet diplomajaar 0 om naar NA", {
  basisbestand <- tibble(
    persoonsgebonden_nummer = "A",
    soort_diploma_instelling = BACHELOR_DIPLOMA,
    diplomajaar = 0L,
    verblijfsjaar_actuele_instelling = 3L
  )

  result <- maak_diploma_behaald(basisbestand)

  expect_true(is.na(result$jaar_eerste_diploma))
})

test_that("maak_diploma_behaald kiest het vroegste diploma bij meerdere", {
  basisbestand <- tibble(
    persoonsgebonden_nummer = c("A", "A"),
    soort_diploma_instelling = BACHELOR_DIPLOMA,
    diplomajaar = c(2024L, 2022L),
    verblijfsjaar_actuele_instelling = c(5L, 3L)
  )

  result <- maak_diploma_behaald(basisbestand)

  expect_equal(nrow(result), 1)
  expect_equal(result$jaar_eerste_diploma, 2022)
})

test_that("bereken_rendement berekent rendement_xjaar correct", {
  cohorten_instroom <- tibble(
    persoonsgebonden_nummer = "A",
    eerstejaar_instelling = 2019
  )
  diploma_behaald <- tibble(
    persoonsgebonden_nummer = "A",
    jaar_eerste_diploma = 2022,
    verblijfsjaar_eerste_diploma = 3L,
    diploma = "Diploma behaald (excl. propedeuse)"
  )

  result <- bereken_rendement(cohorten_instroom, diploma_behaald)

  ## 2022 - 2019 + 1 = 4
  expect_equal(as.numeric(as.character(result$rendement_xjaar)), 4)
})

test_that("bereken_rendement categoriseert rendement_3jr, _5jr en _8jr correct", {
  cohorten_instroom <- tibble(
    persoonsgebonden_nummer = c("A", "B", "C", "D"),
    eerstejaar_instelling = 2015
  )
  diploma_behaald <- tibble(
    persoonsgebonden_nummer = c("A", "B", "C"),
    jaar_eerste_diploma = c(2017, 2019, 2022),
    verblijfsjaar_eerste_diploma = c(3L, 5L, 8L),
    diploma = "Diploma behaald (excl. propedeuse)"
  )

  result <- bereken_rendement(cohorten_instroom, diploma_behaald)

  verwacht_3jr <- c(
    "Diploma binnen 3 jaar",
    "Diploma na 3 jaar",
    "Diploma na 3 jaar",
    "Geen diploma"
  )
  verwacht_5jr <- c(
    "Diploma binnen 5 jaar",
    "Diploma binnen 5 jaar",
    "Diploma na 5 jaar",
    "Geen diploma"
  )
  verwacht_8jr <- c(
    "Diploma binnen 8 jaar",
    "Diploma binnen 8 jaar",
    "Diploma binnen 8 jaar",
    "Geen diploma"
  )

  expect_equal(as.character(result$rendement_3jr), verwacht_3jr)
  expect_equal(as.character(result$rendement_5jr), verwacht_5jr)
  expect_equal(as.character(result$rendement_8jr), verwacht_8jr)
})

test_that("bereken_rendement geeft factorkolommen terug", {
  cohorten_instroom <- tibble(
    persoonsgebonden_nummer = "A",
    eerstejaar_instelling = 2019
  )
  diploma_behaald <- tibble(
    persoonsgebonden_nummer = character(0),
    jaar_eerste_diploma = integer(0),
    verblijfsjaar_eerste_diploma = integer(0),
    diploma = character(0)
  )

  result <- bereken_rendement(cohorten_instroom, diploma_behaald)

  expect_true(is.factor(result$rendement_3jr))
  expect_true(is.factor(result$rendement_5jr))
  expect_true(is.factor(result$rendement_8jr))
})
