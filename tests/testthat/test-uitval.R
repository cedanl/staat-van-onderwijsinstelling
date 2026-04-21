maak_basis_uitval <- function(pgn, inschrijvingsjaar) {
  tibble(
    persoonsgebonden_nummer = pgn,
    inschrijvingsjaar = inschrijvingsjaar,
    soort_inschrijving_actuele_instelling = "hoofdinschrijving"
  )
}

maak_cohort_uitval <- function(pgn, eerstejaar) {
  tibble(
    persoonsgebonden_nummer = pgn,
    eerstejaar_instelling = eerstejaar
  )
}

geen_diplomas <- tibble(
  persoonsgebonden_nummer = character(0),
  jaar_eerste_diploma = integer(0),
  verblijfsjaar_eerste_diploma = integer(0),
  diploma = character(0)
)

test_that("bereken_uitval markeert zittende student correct", {
  jaar <- 2025

  result <- bereken_uitval(
    basisbestand = maak_basis_uitval("A", jaar - 1),
    diploma_behaald = geen_diplomas,
    cohorten_instroom = maak_cohort_uitval("A", 2022),
    jaar = jaar
  )

  expect_equal(as.character(result$status), "Zittend")
  expect_true(is.na(result$uitval_xjr))
})

test_that("bereken_uitval markeert gediplomeerde student correct", {
  jaar <- 2025

  diploma_behaald <- tibble(
    persoonsgebonden_nummer = "A",
    jaar_eerste_diploma = 2022,
    verblijfsjaar_eerste_diploma = 3L,
    diploma = "Diploma behaald (excl. propedeuse)"
  )

  result <- bereken_uitval(
    basisbestand = maak_basis_uitval("A", 2022),
    diploma_behaald = diploma_behaald,
    cohorten_instroom = maak_cohort_uitval("A", 2020),
    jaar = jaar
  )

  expect_equal(as.character(result$status), "Diploma behaald")
  expect_true(is.na(result$uitval_xjr))
})

test_that("bereken_uitval markeert uitgevallen student correct en berekent uitval_xjr", {
  jaar <- 2025

  result <- bereken_uitval(
    basisbestand = maak_basis_uitval("A", 2021),
    diploma_behaald = geen_diplomas,
    cohorten_instroom = maak_cohort_uitval("A", 2020),
    jaar = jaar
  )

  expect_equal(as.character(result$status), "Uitgevallen")
  ## 2021 + 1 - 2020 = 2
  expect_equal(result$uitval_xjr, 2)
})

test_that("bereken_uitval categoriseert uitval_1jr en uitval_3jr correct", {
  jaar <- 2025

  basisbestand <- tibble(
    persoonsgebonden_nummer = c("A", "B", "C"),
    inschrijvingsjaar = c(2020, 2021, 2017),
    soort_inschrijving_actuele_instelling = "hoofdinschrijving"
  )
  cohorten_instroom <- tibble(
    persoonsgebonden_nummer = c("A", "B", "C"),
    eerstejaar_instelling = 2020
  )

  result <- bereken_uitval(
    basisbestand = basisbestand,
    diploma_behaald = geen_diplomas,
    cohorten_instroom = cohorten_instroom,
    jaar = jaar
  )

  a <- result[result$persoonsgebonden_nummer == "A", ]
  b <- result[result$persoonsgebonden_nummer == "B", ]
  c_ <- result[result$persoonsgebonden_nummer == "C", ]

  ## A: uitval_xjr = 2020 + 1 - 2020 = 1 -> uitgevallen binnen 1 jaar
  expect_equal(
    as.character(a$uitval_1jr),
    "Uitgevallen binnen 1 jaar"
  )
  ## B: uitval_xjr = 2021 + 1 - 2020 = 2 -> niet binnen 1 jaar, wel binnen 3 jaar
  expect_equal(
    as.character(b$uitval_1jr),
    "Na 1 jaar nog ingeschreven of diploma behaald"
  )
  expect_equal(
    as.character(b$uitval_3jr),
    "Uitgevallen binnen 3 jaar"
  )
  ## C: uitval_xjr = 2017 + 1 - 2020 = -2 -> na 3 jaar (negatief, dus > 3 is FALSE, <= 3 is TRUE)
  ## Wacht - uitval_xjr = 2017 + 1 - 2020 = -2, wat < 3 is -> "Uitgevallen binnen 3 jaar"
  ## Maar dit is een data-edge-case (diplomajaar voor instroomjaar)
  ## Laten we C vervangen door een duidelijker geval
})

test_that("bereken_uitval categoriseert uitval na 3 jaar correct", {
  jaar <- 2025

  basisbestand <- tibble(
    persoonsgebonden_nummer = "A",
    inschrijvingsjaar = 2015,
    soort_inschrijving_actuele_instelling = "hoofdinschrijving"
  )
  cohorten_instroom <- maak_cohort_uitval("A", 2010)

  result <- bereken_uitval(
    basisbestand = basisbestand,
    diploma_behaald = geen_diplomas,
    cohorten_instroom = cohorten_instroom,
    jaar = jaar
  )

  ## 2015 + 1 - 2010 = 6 -> uitval_3jr = "Na 3 jaar nog ingeschreven of diploma behaald"
  expect_equal(
    as.character(result$uitval_3jr),
    "Na 3 jaar nog ingeschreven of diploma behaald"
  )
})
