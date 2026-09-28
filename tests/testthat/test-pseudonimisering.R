## Referentiewaarden berekend met Python, precies zoals 1cijferho het doet:
## hmac.new(key.encode("utf-8"), str(value).encode("utf-8"), hashlib.sha256).hexdigest()
TEST_SLEUTEL <- substr(strrep("staat1cho-testsleutel-", 4), 1, 70)

test_that("pseudonimisering is identiek aan 1cijferho (Python HMAC-SHA256)", {
  sleutel <- laad_sleutel(TEST_SLEUTEL)
  expect_equal(
    pseudonimiseer_ids(c("700010001", "012345678", "800000001"), sleutel),
    c(
      "d55c0fb47323a6c34b0954d416624a7b6af9f54d378078f5d20dcd89be8a96df",
      "c15383ae6c537d9df307ada4ed55812bd1f77add0524c9819e640522662b5cbb",
      "80d8522771a5260e6f2d8621ac01bc5bb7bf172a01a3b529f624601b90373cbe"
    )
  )
})

test_that("niet-ASCII-sleutels worden als UTF-8 gebruikt, net als in 1cijferho", {
  ## intToUtf8(235) is e-trema; zo blijft dit testbestand ASCII
  sleutel <- laad_sleutel(paste0("sleutel-met-", intToUtf8(235), "-", strrep("x", 60)))
  expect_equal(
    pseudonimiseer_ids("700010001", sleutel),
    "5b11c8c75099ec213e38dbbf10ecbda0da1db7b8185210e7937f436306ff34f8"
  )
})

test_that("voorloopnullen tellen mee en lege waarden blijven leeg", {
  sleutel <- laad_sleutel(TEST_SLEUTEL)
  uit <- pseudonimiseer_ids(c("012345678", "12345678", "", NA), sleutel)
  expect_false(uit[1] == uit[2])
  expect_equal(uit[3:4], c(NA_character_, NA_character_))
})

test_that("laad_sleutel volgt de volgorde sleutel, bestand, omgevingsvariabele", {
  bestand <- tempfile()
  writeLines(paste0(TEST_SLEUTEL, "\n"), bestand)
  expect_identical(laad_sleutel(sleutelbestand = bestand), charToRaw(TEST_SLEUTEL))

  oud <- Sys.getenv("EENCIJFERHO_ENCRYPT_KEY", unset = NA)
  on.exit(if (is.na(oud)) Sys.unsetenv("EENCIJFERHO_ENCRYPT_KEY") else Sys.setenv(EENCIJFERHO_ENCRYPT_KEY = oud))
  Sys.setenv(EENCIJFERHO_ENCRYPT_KEY = TEST_SLEUTEL)
  expect_identical(laad_sleutel(), charToRaw(TEST_SLEUTEL))

  Sys.unsetenv("EENCIJFERHO_ENCRYPT_KEY")
  expect_error(laad_sleutel(), "Geen pseudonimiseringssleutel")
  expect_error(laad_sleutel("te-kort"), "te kort")
})

test_that("is_gepseudonimiseerd herkent 1cijferho-pseudoniemen", {
  expect_true(is_gepseudonimiseerd(c(strrep("ab", 32), NA, "")))
  expect_false(is_gepseudonimiseerd(c("700010001", strrep("ab", 32))))
  expect_false(is_gepseudonimiseerd(character(0)))
})

## --- koppeling met gepseudonimiseerd 1CHO ---

vlpbek_regels <- c(
  "VLP|TEST|2025|20240115|||||||||||||||||||||",
  "BRD|700010001||TEST|1|J|pi|x|31001|HBO-BA|B|20230901|20240831|x|S|VT|x|x|ECONOMIE|BEKOSTIGD||||x|x",
  "BRD||800000002|TEST|2|J|nf|x|31001|HBO-BA|B|20230901|20240831|x|S|VT|x|x|ECONOMIE|||||x|x"
)

test_that("VLPBEK koppelt aan een door 1cijferho gepseudonimiseerd 1CHO-bestand", {
  pad <- tempfile(fileext = ".csv")
  writeLines(vlpbek_regels, pad)
  sleutel <- laad_sleutel(TEST_SLEUTEL)

  ## Zo ziet het 1CHO-bestand eruit na 1cijferho-pseudonimisering
  indicatoren <- tibble::tibble(
    persoonsgebonden_nummer = pseudonimiseer_ids(c("700010001", "800000002", "999999999"), sleutel),
    opleidingscode = "31001"
  )

  bek <- lees_bekostiging(pad, pseudonimiseer = TRUE, sleutel = TEST_SLEUTEL)
  expect_true(is_gepseudonimiseerd(bek$persoonsgebonden_nummer))
  expect_true(attr(bek, "gepseudonimiseerd"))
  ## Het echte nummer staat nergens meer in de uitvoer
  expect_false(any(grepl("700010001|800000002", unlist(bek))))

  result <- suppressMessages(verrijk_met_bekostiging(indicatoren, bek))
  expect_equal(result$indicatie_bekostigd, c(TRUE, FALSE, NA))
  expect_equal(attr(result, "koppeling")$gekoppeld, 2L)
})

test_that("geeft een duidelijke fout als alleen 1CHO gepseudonimiseerd is", {
  pad <- tempfile(fileext = ".csv")
  writeLines(vlpbek_regels, pad)
  indicatoren <- tibble::tibble(
    persoonsgebonden_nummer = pseudonimiseer_ids("700010001", laad_sleutel(TEST_SLEUTEL)),
    opleidingscode = "31001"
  )
  expect_error(
    verrijk_met_bekostiging(indicatoren, lees_bekostiging(pad)),
    "pseudonimiseer = TRUE"
  )
})

test_that("geeft een fout als VAKHAVW niet gepseudonimiseerd is maar 1CHO wel", {
  indicatoren <- tibble::tibble(persoonsgebonden_nummer = strrep("ab", 32))
  vakhawv <- tibble::tibble(
    persoonsgebonden_nummer = "700010001", afkorting_vak = "ne",
    gemiddeld_cijfer_cijferlijst = 7, cijfer_eerste_centraal_examen = 7,
    cijfer_schoolexamen = 7
  )
  expect_error(verrijk_met_vakhawv(indicatoren, vakhawv), "VAKHAVW")
})
