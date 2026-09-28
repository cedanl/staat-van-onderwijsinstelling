## Helpers ----

basis_df <- function(n = 50L, jaar = 2022L) {
  tibble::tibble(
    sector            = rep("gezondheidszorg", n),
    opleidingsvorm    = rep("voltijd", n),
    opleidingsniveau  = rep("bachelor", n),
    inschrijvingsjaar = rep(jaar, n),
    uitval_1jr        = factor(rep("Na 1 jaar nog ingeschreven of diploma behaald", n)),
    uitval_3jr        = factor(rep("Na 3 jaar nog ingeschreven of diploma behaald", n)),
    rendement_3jr     = factor(rep("Geen diploma", n)),
    rendement_5jr     = factor(rep("Diploma binnen 5 jaar", n)),
    rendement_8jr     = factor(rep("Diploma binnen 8 jaar", n)),
    int_student       = rep("geen internationale student", n),
    leeftijd_bij_instroom = rep(19L, n)
  )
}

## Basisstructuur ----

test_that("maak_benchmarkrapport geeft een tibble terug", {
  result <- maak_benchmarkrapport(basis_df(), drempel = 1L)
  expect_s3_class(result, "tbl_df")
})

test_that("maak_benchmarkrapport bevat de vereiste kolommen", {
  result <- maak_benchmarkrapport(basis_df(), drempel = 1L)
  verwacht <- c(
    "sector", "opleidingsvorm", "opleidingsniveau", "inschrijvingsjaar",
    "onderdrukt", "n", "pct_uitval_1jr", "pct_uitval_3jr",
    "pct_rendement_3jr", "pct_rendement_5jr", "pct_rendement_8jr",
    "pct_int_student", "gem_leeftijd_instroom"
  )
  expect_true(all(verwacht %in% names(result)))
})

test_that("maak_benchmarkrapport heeft een gegenereerd_op attribuut", {
  result <- maak_benchmarkrapport(basis_df(), drempel = 1L)
  expect_s3_class(attr(result, "gegenereerd_op"), "POSIXct")
})

## Totaalrij ----

test_that("elke instroomjaar krijgt een totaalrij", {
  df <- dplyr::bind_rows(basis_df(50L, 2022L), basis_df(50L, 2023L))
  result <- maak_benchmarkrapport(df, drempel = 1L)

  totaalrijen <- result[result$sector == "totaal", ]
  expect_equal(nrow(totaalrijen), 2L)
  expect_true(all(is.na(totaalrijen$opleidingsvorm)))
  expect_true(all(is.na(totaalrijen$opleidingsniveau)))
})

test_that("totaalrij telt alle studenten van dat jaar", {
  df <- dplyr::bind_rows(basis_df(50L, 2022L), basis_df(50L, 2022L))
  result <- maak_benchmarkrapport(df, drempel = 1L)

  totaal <- result[result$sector == "totaal" & result$inschrijvingsjaar == 2022L, ]
  expect_equal(totaal$n, 100L)
})

## Berekeningen ----

test_that("pct_uitval_1jr wordt correct berekend", {
  df <- basis_df(50L)
  labels <- as.character(df$uitval_1jr)
  labels[1:10] <- "Uitgevallen binnen 1 jaar"
  df$uitval_1jr <- factor(labels)
  result <- maak_benchmarkrapport(df, drempel = 1L)

  rij <- result[result$sector == "gezondheidszorg" & !is.na(result$sector), ]
  expect_equal(rij$pct_uitval_1jr[1], 20)
})

test_that("pct_rendement_5jr wordt correct berekend", {
  df <- basis_df(50L)
  result <- maak_benchmarkrapport(df, drempel = 1L)

  rij <- result[result$sector == "gezondheidszorg" & !is.na(result$sector), ]
  expect_equal(rij$pct_rendement_5jr[1], 100)
})

test_that("gem_leeftijd_instroom is het gemiddelde van de groep", {
  df <- basis_df(50L)
  df$leeftijd_bij_instroom <- c(rep(18L, 25L), rep(22L, 25L))
  result <- maak_benchmarkrapport(df, drempel = 1L)

  rij <- result[result$sector == "gezondheidszorg" & !is.na(result$sector), ]
  expect_equal(rij$gem_leeftijd_instroom[1], 20)
})

## Privacyonderdrukking ----

test_that("groepen met n < drempel krijgen NA voor uitkomsten en voor n", {
  df <- basis_df(10L)
  result <- maak_benchmarkrapport(df, drempel = 30L)

  rij <- result[result$sector == "gezondheidszorg" & !is.na(result$sector), ]
  expect_true(is.na(rij$pct_uitval_1jr[1]))
  expect_true(is.na(rij$pct_rendement_5jr[1]))
  ## n wordt ook onderdrukt; alleen onderdrukt=TRUE is zichtbaar
  expect_true(is.na(rij$n[1]))
  expect_true(rij$onderdrukt[1])
})

test_that("groepen met n >= drempel krijgen geen NA door onderdrukking", {
  df <- basis_df(30L)
  result <- maak_benchmarkrapport(df, drempel = 30L)

  rij <- result[result$sector == "gezondheidszorg" & !is.na(result$sector), ]
  expect_false(is.na(rij$pct_uitval_1jr[1]))
  expect_false(is.na(rij$n[1]))
  expect_false(rij$onderdrukt[1])
})

test_that("onderdrukt is TRUE voor kleine groepen en FALSE voor grote", {
  df <- dplyr::bind_rows(
    dplyr::mutate(basis_df(5L),  sector = "klein"),
    dplyr::mutate(basis_df(50L), sector = "groot")
  )
  result <- maak_benchmarkrapport(df, drempel = 30L)

  expect_true(result$onderdrukt[result$sector == "klein"])
  expect_false(result$onderdrukt[result$sector == "groot"])
})

test_that("totaalrij wordt ook onderdrukt als n < drempel", {
  df <- basis_df(5L)
  result <- maak_benchmarkrapport(df, drempel = 30L)

  totaal <- result[result$sector == "totaal", ]
  expect_true(is.na(totaal$pct_uitval_1jr[1]))
  expect_true(is.na(totaal$n[1]))
  expect_true(totaal$onderdrukt[1])
})

## Optionele kolommen ----

test_that("studiewissel wordt meegenomen als de kolom aanwezig is", {
  df <- basis_df(50L)
  df$studiewissel_1jr <- factor("Niet gewisseld binnen 1 jaar")
  df$studiewissel_3jr <- factor("Niet gewisseld binnen 3 jaar")

  result <- maak_benchmarkrapport(df, drempel = 1L)
  expect_true("pct_studiewissel_1jr" %in% names(result))
  expect_true("pct_studiewissel_3jr" %in% names(result))
})

test_that("studiewissel kolommen zijn afwezig als ze niet zijn meegegeven", {
  result <- maak_benchmarkrapport(basis_df(50L), drempel = 1L)
  expect_false("pct_studiewissel_1jr" %in% names(result))
  expect_false("pct_studiewissel_3jr" %in% names(result))
})

test_that("gem_eindcijfer wordt berekend als vakhawv aanwezig is", {
  df <- basis_df(50L)
  df$vakhawv_gemiddeld_eindcijfer <- 7.0

  result <- maak_benchmarkrapport(df, drempel = 1L)
  expect_true("gem_eindcijfer" %in% names(result))

  rij <- result[result$sector == "gezondheidszorg" & !is.na(result$sector), ]
  expect_equal(rij$gem_eindcijfer[1], 7.0)
})

test_that("gem_eindcijfer ontbreekt als vakhawv niet aanwezig is", {
  result <- maak_benchmarkrapport(basis_df(50L), drempel = 1L)
  expect_false("gem_eindcijfer" %in% names(result))
})

test_that("pct_bekostigd wordt berekend als indicatie_bekostigd aanwezig is", {
  df <- basis_df(50L)
  df$indicatie_bekostigd <- c(rep(TRUE, 40L), rep(FALSE, 10L))

  result <- maak_benchmarkrapport(df, drempel = 1L)
  expect_true("pct_bekostigd" %in% names(result))

  rij <- result[result$sector == "gezondheidszorg" & !is.na(result$sector), ]
  expect_equal(rij$pct_bekostigd[1], 80)
})

test_that("pct_hoofdinschrijving wordt berekend als de kolom aanwezig is", {
  df <- basis_df(50L)
  df$indicatie_hoofdinschrijving <- TRUE

  result <- maak_benchmarkrapport(df, drempel = 1L)
  expect_true("pct_hoofdinschrijving" %in% names(result))
})

test_that("gem_wiskundecijfer wordt berekend als de kolom aanwezig is", {
  df <- basis_df(50L)
  df$vakhawv_wiskundecijfer <- 6.5

  result <- maak_benchmarkrapport(df, drempel = 1L)
  expect_true("gem_wiskundecijfer" %in% names(result))

  rij <- result[result$sector == "gezondheidszorg" & !is.na(result$sector), ]
  expect_equal(rij$gem_wiskundecijfer[1], 6.5)
})

test_that("gem_wiskundecijfer ontbreekt als vakhawv_wiskundecijfer niet aanwezig is", {
  result <- maak_benchmarkrapport(basis_df(50L), drempel = 1L)
  expect_false("gem_wiskundecijfer" %in% names(result))
})

test_that("gem_aantal_vakken wordt berekend als de kolom aanwezig is", {
  df <- basis_df(50L)
  df$vakhawv_aantal_vakken <- c(rep(6L, 25L), rep(8L, 25L))

  result <- maak_benchmarkrapport(df, drempel = 1L)
  expect_true("gem_aantal_vakken" %in% names(result))

  rij <- result[result$sector == "gezondheidszorg" & !is.na(result$sector), ]
  expect_equal(rij$gem_aantal_vakken[1], 7)
})

test_that("pct_herstelbaar wordt berekend over de niet-bekostigde rijen", {
  df <- basis_df(50L)
  df$indicatie_bekostigd    <- c(rep(TRUE, 40L), rep(FALSE, 10L))
  df$indicatie_herstelbaar  <- c(rep(NA, 40L), rep(TRUE, 4L), rep(FALSE, 6L))

  result <- maak_benchmarkrapport(df, drempel = 1L)
  expect_true("pct_herstelbaar" %in% names(result))

  rij <- result[result$sector == "gezondheidszorg" & !is.na(result$sector), ]
  expect_equal(rij$pct_herstelbaar[1], 40)
})

test_that("pct_herstelbaar ontbreekt als indicatie_herstelbaar niet aanwezig is", {
  result <- maak_benchmarkrapport(basis_df(50L), drempel = 1L)
  expect_false("pct_herstelbaar" %in% names(result))
})

## Meerdere groepen ----

test_that("meerdere sectoren en opleidingsvormen worden elk apart geaggregeerd", {
  df <- dplyr::bind_rows(
    dplyr::mutate(basis_df(50L), sector = "gezondheidszorg"),
    dplyr::mutate(basis_df(50L), sector = "economie")
  )
  result <- maak_benchmarkrapport(df, drempel = 1L)

  data_rijen <- result[result$sector != "totaal", ]
  expect_equal(nrow(data_rijen), 2L)
})

test_that("resultaat is gesorteerd op inschrijvingsjaar", {
  df <- dplyr::bind_rows(basis_df(50L, 2023L), basis_df(50L, 2022L))
  result <- maak_benchmarkrapport(df, drempel = 1L)

  jaren <- result$inschrijvingsjaar[result$sector != "totaal"]
  expect_equal(jaren, sort(jaren))
})


## Onvolledige cohorten ----

test_that("niet-waarneembare rijen tellen niet mee in de noemer", {
  df <- basis_df(40L)
  df$rendement_5jr <- factor(c(
    rep("Diploma binnen 5 jaar", 10),
    rep("Geen diploma", 10),
    rep("Nog niet waarneembaar", 20)
  ))
  result <- maak_benchmarkrapport(df, drempel = 1L)
  expect_equal(result$pct_rendement_5jr[result$sector != "totaal"], 50)
})

test_that("percentage is NA als niemand in de groep waarneembaar is", {
  df <- basis_df(40L)
  df$rendement_8jr <- factor(rep("Nog niet waarneembaar", 40))
  result <- maak_benchmarkrapport(df, drempel = 1L)
  expect_true(all(is.na(result$pct_rendement_8jr)))
})
