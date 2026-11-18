## Dashboard: het domeinfilter bestaat alleen met een domeinindeling (#58).

app_scherm <- function(domeinen = NULL) {
  skip_if_not_installed("bslib")
  skip_if_not_installed("plotly")
  app <- system.file("app", package = "staat1cho")
  skip_if(!nzchar(app), "app niet gevonden")
  d <- maak_analysebestand(
    system.file("extdata", "voorbeeld_1cho.csv", package = "staat1cho"),
    domeinen = domeinen
  )
  uit <- NULL
  shiny::testServer(shiny::shinyAppDir(app), {
    analyse_niveau("student")
    df_data(d)
    fase("dashboard")
    session$setInputs(jaar_filter = range(d$inschrijvingsjaar))
    uit <<- list(html = as.character(output$scherm$html))
    if (!is.null(domeinen)) {
      session$setInputs(domein_filter = "Techniek")
      uit$n_gefilterd <<- nrow(df())
    }
  })
  uit
}

test_that("zonder domeinindeling geen domeinfilter", {
  uit <- suppressWarnings(app_scherm())
  expect_false(grepl("domein_filter", uit$html, fixed = TRUE))
  expect_false(grepl("plot_instroom_domein", uit$html, fixed = TRUE))
})

test_that("met domeinindeling filter met de domeinen en filtert de data", {
  uit <- suppressWarnings(app_scherm(
    system.file("extdata", "voorbeeld_domeinen.csv", package = "staat1cho")
  ))
  expect_true(grepl("domein_filter", uit$html, fixed = TRUE))
  for (dom in c("Techniek", "Economie", "Gedrag en maatschappij")) {
    expect_true(grepl(dom, uit$html, fixed = TRUE))
  }
  expect_equal(uit$n_gefilterd, 1)
})
