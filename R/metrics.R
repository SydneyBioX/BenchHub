kdeMetric <- function(gs, to_eval) {
  assertSuggestAvail("ks")
  ks::kde.test(
    x1 = as.numeric(gs), x2 = as.numeric(to_eval)
  ) |> purrr::pluck("zstat")
}

#' @importFrom Hmisc rcorr.cens
harrelCIndexMetric <- function(gs, to_eval) {
  harrelC1 <- Hmisc::rcorr.cens(-to_eval[[2]], gs[[2]])
  return(harrelC1["C Index"])
}

#' @import survAUC
beggCIndexMetric <- function(gs, to_eval) {
  # cli::cli_abort(c(
  #   "Begg's c-index is not supported yet! :("
  # ))
  # gs:list(surv_object[ train_index], surv_object[ test_index])
  # to_eval: list(predictors[ train_index], predictors[test_index])
  begg_cindex <- survAUC::BeggC(gs[[1]], gs[[2]],to_eval[[1]], to_eval[[2]])
  return(begg_cindex)
}

unoCIndexMetric <- function(gs, to_eval) {
  # cli::cli_abort(c(
  #   "Uno's c-index is not supported yet! :("
  # ))
  
  uno_cindex <- survAUC::UnoC(gs[[1]], gs[[2]], to_eval)
  return(uno_cindex)
  
}

ghCIndexMetric <- function(gs, to_eval) {
  # cli::cli_abort(c(
  #   "GH's c-index is not supported yet! :("
  # ))
  gh_cindex <- survAUC::GHCI(to_eval)
  return(gh_cindex)
}

brierScoreMetric <- function(gs, to_eval) {
  # cli::cli_abort(c(
  #   "Brier's score is not supported yet! :("
  # ))
  time <- gs[[1]][,"time"]
  brier_score <- survAUC::predErr(gs[[1]], gs[[2]],to_eval[[1]], to_eval[[2]],times=time, type = "brier", int.type = "unweighted")$error
  return(brier_score)
}

timeDependentAUCMetric <- function(gs, to_eval) {
  # cli::cli_abort(c(
  #   "Time dependent AUC is not supported yet! :("
  # ))
  time <- gs[[1]][,"time"]
  AUC_CD <- survAUC::AUC.uno(gs[[1]], gs[[2]], to_eval[[2]], time)
  return(AUC_CD)
}

