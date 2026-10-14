## De optionele voorstap in het dashboard (gebruikt door
## staat-van-onderwijsinstelling-mbo): bronbestanden uploaden, samenvoegen,
## melding tonen en dan door naar het dashboard.

skip_if_not_installed("bslib")
skip_if_not_installed("plotly")

app_dir <- system.file("app", package = "staat1cho")

synthetisch_csv <- function() {
  csv <- tempfile(fileext = ".csv")
  readr::write_delim(
    maak_synthetische_1cho(n_per_jaar = 20L, jaren = 2018:2023, seed = 1L),
    csv,
    delim = ";",
    na = ""
  )
  csv
}

test_that("zonder voorstap start de app in de upload-fase", {
  shiny::shinyOptions(staat1cho.voorstap = NULL)
  shiny::testServer(shiny::shinyAppDir(app_dir), {
    expect_equal(fase(), "upload")
  })
})

test_that("voorstap: samenvoegen toont meldingen en verder opent het dashboard", {
  csv <- synthetisch_csv()
  ontvangen <- NULL
  shiny::shinyOptions(staat1cho.voorstap = list(
    titel = "Bronbestanden",
    uitleg = "test",
    samenvoegen = function(bestanden) {
      ontvangen <<- bestanden
      list(ok = TRUE, pad = csv, meldingen = c("Gelukt: test", "Studenten: 120"))
    }
  ))
  on.exit(shiny::shinyOptions(staat1cho.voorstap = NULL))
  shiny::testServer(shiny::shinyAppDir(app_dir), {
    expect_equal(fase(), "voorstap")
    session$setInputs(bron_upload = data.frame(name = "RO_X.csv", datapath = csv))
    session$setInputs(btn_samenvoegen = 1)
    expect_true(voorstap_resultaat()$ok)
    expect_equal(ontvangen$name, "RO_X.csv")
    session$setInputs(btn_verder = 1)
    expect_equal(fase(), "dashboard")
  })
})

test_that("voorstap: mislukt samenvoegen blijft in de voorstap", {
  shiny::shinyOptions(staat1cho.voorstap = list(
    titel = "Bronbestanden",
    uitleg = "test",
    samenvoegen = function(bestanden) {
      list(ok = FALSE, pad = NA_character_, meldingen = "Niet gelukt.")
    }
  ))
  on.exit(shiny::shinyOptions(staat1cho.voorstap = NULL))
  shiny::testServer(shiny::shinyAppDir(app_dir), {
    session$setInputs(bron_upload = data.frame(name = "x", datapath = "x"))
    session$setInputs(btn_samenvoegen = 1)
    expect_false(voorstap_resultaat()$ok)
    expect_equal(fase(), "voorstap")
  })
})
