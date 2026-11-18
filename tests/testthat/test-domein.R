analyse_demo <- function() {
  tibble(
    persoonsgebonden_nummer = 1:4,
    opleidingscode = c(34001, 34001, 56000, 35000),
    sector = c("ECONOMIE", "ECONOMIE", "ONDERWIJS", "ZORG")
  )
}

test_that("koppel_domeinen koppelt codes en zet domein na sector", {
  koppel <- data.frame(opleidingscode = c("34001", "56000", "35000"),
                       domein = c("Bedrijf", "Mens", "Mens"))
  uit <- koppel_domeinen(analyse_demo(), koppel)
  expect_equal(uit$domein, c("Bedrijf", "Bedrijf", "Mens", "Mens"))
  expect_equal(names(uit)[match("domein", names(uit)) - 1], "sector")
  expect_equal(nrow(uit), 4)
})

test_that("ontbrekende code geeft Onbekend en een waarschuwing", {
  koppel <- data.frame(opleidingscode = c("34001", "56000"),
                       domein = c("Bedrijf", "Mens"))
  expect_warning(uit <- koppel_domeinen(analyse_demo(), koppel), "1 student")
  expect_equal(uit$domein[4], "Onbekend")
})

test_that("dubbele code met ander domein geeft een fout", {
  koppel <- data.frame(opleidingscode = c("34001", "34001"),
                       domein = c("Bedrijf", "Mens"))
  expect_error(koppel_domeinen(analyse_demo(), koppel), "dubbel")
})

test_that("dubbele identieke regels zijn geen probleem", {
  koppel <- data.frame(opleidingscode = c("34001", "34001", "56000", "35000"),
                       domein = c("Bedrijf", "Bedrijf", "Mens", "Mens"))
  uit <- koppel_domeinen(analyse_demo(), koppel)
  expect_equal(nrow(uit), 4)
})

test_that("numerieke en tekstcodes koppelen", {
  koppel <- data.frame(opleidingscode = c(34001, 56000, 35000),
                       domein = c("Bedrijf", "Mens", "Mens"))
  expect_equal(koppel_domeinen(analyse_demo(), koppel)$domein[1], "Bedrijf")
  analyse <- analyse_demo()
  analyse$opleidingscode <- as.character(analyse$opleidingscode)
  expect_equal(koppel_domeinen(analyse, koppel)$domein[3], "Mens")
})

test_that("verkeerde kolomnamen geven een fout", {
  koppel <- data.frame(code = "34001", domein = "Bedrijf")
  expect_error(koppel_domeinen(analyse_demo(), koppel), "opleidingscode")
})

test_that("een CSV-pad met ; en BOM werkt", {
  pad <- tempfile(fileext = ".csv")
  on.exit(unlink(pad))
  writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)),
             charToRaw("opleidingscode;domein\n34001;Bedrijf\n56000;Mens\n35000;Mens\n")),
           pad)
  uit <- koppel_domeinen(analyse_demo(), pad)
  expect_equal(uit$domein, c("Bedrijf", "Bedrijf", "Mens", "Mens"))
})
