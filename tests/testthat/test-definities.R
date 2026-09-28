test_that("beide niveaus leggen de filterlogica uit", {
  expect_type(DEFINITIES[["student"]]$filter_instroom, "character")
  expect_type(DEFINITIES[["inschrijving"]]$filter_instroom, "character")
})

test_that("definities van tijdgebonden indicatoren noemen onvolledige cohorten", {
  for (niveau in c("student", "inschrijving")) {
    for (ind in c("rendement_3jr", "rendement_5jr", "rendement_8jr", "uitval_1jr", "uitval_3jr")) {
      expect_match(DEFINITIES[[niveau]][[ind]], "Nog niet waarneembaar", info = paste(niveau, ind))
    }
  }
})
