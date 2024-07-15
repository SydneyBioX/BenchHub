kdeMetric <- function(gs, to_eval) {
  assertSuggestAvail("ks")
  ks::kde.test(
    x1 = as.numeric(gs), x2 = as.numeric(to_eval)
  ) |> purrr::pluck("zstat")
}

harrelCIndexMetric <- function(gs, to_eval) {
  cli::cli_abort(c(
    "Harrell's c-index is not supported yet! :("
  ))
}


beggCIndexMetric <- function(gs, to_eval) {
  cli::cli_abort(c(
    "Begg's c-index is not supported yet! :("
  ))
}

unoCIndexMetric <- function(gs, to_eval) {
  cli::cli_abort(c(
    "Uno's c-index is not supported yet! :("
  ))
}

ghCIndexMetric <- function(gs, to_eval) {
  cli::cli_abort(c(
    "GH's c-index is not supported yet! :("
  ))
}

brierScoreMetric <- function(gs, to_eval) {
  cli::cli_abort(c(
    "Brier's score is not supported yet! :("
  ))
}

timeDependentAUCMetric <- function(gs, to_eval) {
  cli::cli_abort(c(
    "Time dependent AUC is not supported yet! :("
  ))
}
