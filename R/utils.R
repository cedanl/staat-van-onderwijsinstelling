niveau_sleutels <- function(niveau) {
  if (niveau == "inschrijving") {
    c("persoonsgebonden_nummer", "opleiding_actueel_equivalent")
  } else {
    "persoonsgebonden_nummer"
  }
}
