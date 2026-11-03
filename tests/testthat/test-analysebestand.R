synth <- maak_synthetische_1cho(n_per_jaar = 40L, jaren = 2016:2023, seed = 7L)
pad <- tempfile(fileext = ".csv")
readr::write_delim(synth, pad, delim = ";", na = "")

handmatig <- function(niveau) {
  basis <- maak_basisbestand(pad)
  cohort <- maak_instroom_cohort(basis, unique(basis$soort_hoger_onderwijs), niveau = niveau)
  diploma <- maak_diploma_behaald(basis, niveau = niveau)
  rendement <- bereken_rendement(cohort, diploma, niveau = niveau)
  uitval <- bereken_uitval(basis, diploma, cohort, niveau = niveau)
  wissel <- if (niveau == "student") bereken_studiewissel(basis, cohort, diploma, uitval)
  combineer_indicatoren(cohort, rendement, uitval, wissel, niveau = niveau)
}

test_that("geeft dezelfde uitkomst als de losse functies", {
  for (niveau in c("student", "inschrijving")) {
    snel <- maak_analysebestand(pad, niveau = niveau)
    los <- handmatig(niveau)
    expect_equal(
      as.data.frame(snel[setdiff(names(snel), "opleidingsnaam")]),
      as.data.frame(los),
      ignore_attr = TRUE
    )
    expect_equal(attr(snel, "peildatum"), attr(los, "peildatum"))
    expect_equal(attr(snel, "niveau"), niveau)
  }
})

test_that("leest ook een data frame", {
  expect_equal(maak_analysebestand(synth), maak_analysebestand(pad))
})

test_that("voegt de opleidingsnaam toe naast de code", {
  res <- maak_analysebestand(pad)
  expect_equal(which(names(res) == "opleidingsnaam"), which(names(res) == "opleidingscode") + 1L)
  expect_false(anyNA(res$opleidingsnaam))
})

test_that("schrijft een CSV die Nederlandse Excel in kolommen opent", {
  res <- maak_analysebestand(pad)
  uit <- tempfile(fileext = ".csv")
  schrijf_analysebestand(res, uit)
  ## BOM vooraan, zodat Excel het als UTF-8 leest
  expect_equal(readBin(uit, "raw", 3), as.raw(c(0xef, 0xbb, 0xbf)))
  expect_match(readLines(uit, n = 1), "persoonsgebonden_nummer\"?;\"?inschrijvingsjaar")
  terug <- readr::read_delim(uit, delim = ";", show_col_types = FALSE, col_types = readr::cols(.default = "c"))
  expect_equal(nrow(terug), nrow(res))
})
