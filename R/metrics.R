balAccMetric <- function(auxData, predicted) {
  confusionMatrix <- table(auxData, predicted)
  classSizes <- rowSums(confusionMatrix)
  mean(diag(confusionMatrix) / classSizes, na.rm = TRUE)
}

balErrMetric <- function(auxData, predicted) {
  confusionMatrix <- table(auxData, predicted)
  classSizes <- rowSums(confusionMatrix)
  classErrors <- classSizes - diag(confusionMatrix)
  mean(classErrors / classSizes, na.rm = TRUE)
}

.positivesNegatives <- function(auxData, predicted) {
  confusionMatrix <- table(auxData, predicted)
  truePositives <- diag(confusionMatrix)
  falsePositives <- colSums(confusionMatrix) - truePositives
  falseNegatives <- rowSums(confusionMatrix) - truePositives
  trueNegatives <- sum(truePositives) - truePositives
  list(
    TP = truePositives, FP = falsePositives,
    FN = falseNegatives, TN = trueNegatives
  )
}

microPrecMetric <- function(auxData, predicted) {
  PN <- .positivesNegatives(auxData, predicted)
  sum(PN[["TP"]]) / sum(PN[["TP"]] + PN[["FP"]])
}

microRecMetric <- function(auxData, predicted) {
  PN <- .positivesNegatives(auxData, predicted)
  sum(PN[["TP"]]) / sum(PN[["TP"]] + PN[["FN"]])
}

microF1Metric <- function(auxData, predicted) {
  2 * microPrecMetric(auxData, predicted) * microRecMetric(auxData, predicted) /
    (microPrecMetric(auxData, predicted) + microRecMetric(auxData, predicted))
}


macroPrecMetric <- function(auxData, predicted) {
  PN <- .positivesNegatives(auxData, predicted)
  sum(PN[["TP"]] / (PN[["TP"]] + PN[["FP"]])) / length(levels(auxData))
}

macroRecMetric <- function(auxData, predicted) {
  PN <- .positivesNegatives(auxData, predicted)
  sum(PN[["TP"]] / (PN[["TP"]] + PN[["FN"]])) / length(levels(auxData))
}

macroF1Metric <- function(auxData, predicted) {
  2 * macroPrec(auxData, predicted) * macroRec(auxData, predicted) /
    (macroPrec(auxData, predicted) + macroRec(auxData, predicted))
}

MCCmetric <- function(auxData, predicted) {
  nClass <- length(levels(auxData))
  if (nClass != 2) {
    cli::cli_abort(c(
      "Matthews Correlation Coefficient (MCC) calculation failed.",
      "i" = "Selected data has {nClass} classes ({.val {levels(auxData)}}).",
      "i" = "MCC only supports 2 classes."
    ))
  }
  PN <- .positivesNegatives(auxData, predicted)
  (PN[["TP"]][2] * PN[["TN"]][2] - PN[["FP"]][2] * PN[["FN"]][2]) /
    sqrt(
      (PN[["TP"]][2] + PN[["FP"]][2]) * (PN[["TP"]][2] + PN[["FN"]][2]) *
        (PN[["TN"]][2] + PN[["FP"]][2]) * (PN[["TN"]][2] + PN[["FN"]][2])
    )
}

# One for numeric auxiliary data.
MSEmetric <- function(auxData, predicted) {
  mean((auxData - predicted)^2)
}

kdeMetric <- function(auxData, predicted) {
  assertSuggestAvail("ks")
  ks::kde.test(
    x1 = as.numeric(auxData), x2 = as.numeric(predicted)
  ) |> purrr::pluck("zstat")
}

#' @importFrom Hmisc rcorr.cens
harrelCIndexMetric <- function(auxData, predicted) {
  assertSuggestAvail("Hmisc")
  harrelC1 <- Hmisc::rcorr.cens(-predicted[[2]], auxData[[2]])
  return(harrelC1["C Index"])
}

#' @importFrom survAUC BeggC
beggCIndexMetric <- function(auxData, predicted) {
  assertSuggestAvail("survAUC")

  survAUC::BeggC(
    auxData[[1]], auxData[[2]], predicted[[1]], predicted[[2]]
  )
}

#' @importFrom survAUC UnoC
unoCIndexMetric <- function(auxData, predicted) {
  assertSuggestAvail("survAUC")

  survAUC::UnoC(auxData[[1]], auxData[[2]], predicted)
}

#' @importFrom survAUC GHCI
ghCIndexMetric <- function(auxData, predicted) {
  assertSuggestAvail("survAUC")

  survAUC::GHCI(predicted)
}

#' @importFrom survAUC predErr
brierScoreMetric <- function(auxData, predicted) {
  assertSuggestAvail("survAUC")

  time <- auxData[[1]][, "time"]
  survAUC::predErr(
    auxData[[1]], auxData[[2]], predicted[[1]], predicted[[2]],
    times = time, type = "brier", int.type = "unweighted"
  )$error
}

#' @importFrom survAUC AUC.uno
timeDependentAUCMetric <- function(auxData, predicted) {
  assertSuggestAvail("survAUC")

  time <- auxData[[1]][, "time"]
  AUC_CD <- survAUC::AUC.uno(auxData[[1]], auxData[[2]], predicted[[2]], time)
  return(AUC_CD)
}
