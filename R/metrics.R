#' Balanced Accuracy Metric
#' 
#' @description Computes the balanced accuracy of the predictions.
#' @param auxData The true labels.
#' @param predicted The predicted labels.
#' @return The balanced accuracy.
#' @export
balAccMetric <- function(auxData, predicted) {
  confusionMatrix <- table(auxData, predicted)
  classSizes <- rowSums(confusionMatrix)
  mean(diag(confusionMatrix) / classSizes, na.rm = TRUE)
}

#' Balanced Error Metric
#' 
#' @description Computes the balanced error of the predictions.
#' @param auxData The true labels.
#' @param predicted The predicted labels.
#' @return The balanced error.
#' @export
balErrMetric <- function(auxData, predicted) {
  confusionMatrix <- table(auxData, predicted)
  classSizes <- rowSums(confusionMatrix)
  classErrors <- classSizes - diag(confusionMatrix)
  mean(classErrors / classSizes, na.rm = TRUE)
}

#' Compute Positives and Negatives
#' 
#' @description Computes the true positives, false positives, false negatives, and true negatives.
#' @param auxData The true labels.
#' @param predicted The predicted labels.
#' @return A list containing the true positives, false positives, false negatives, and true negatives.
#' @export
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

#' Micro Precision Metric
#' 
#' @description Computes the micro precision of the predictions.
#' @param auxData The true labels.
#' @param predicted The predicted labels.
#' @return The micro precision.
#' @export
microPrecMetric <- function(auxData, predicted) {
  PN <- .positivesNegatives(auxData, predicted)
  sum(PN[["TP"]]) / sum(PN[["TP"]] + PN[["FP"]])
}

#' Micro Recall Metric
#' 
#' @description Computes the micro recall of the predictions.
#' @param auxData The true labels.
#' @param predicted The predicted labels.
#' @return The micro recall.
#' @export
microRecMetric <- function(auxData, predicted) {
  PN <- .positivesNegatives(auxData, predicted)
  sum(PN[["TP"]]) / sum(PN[["TP"]] + PN[["FN"]])
}

#' Micro F1 Score Metric
#' 
#' @description Computes the micro F1 score of the predictions.
#' @param auxData The true labels.
#' @param predicted The predicted labels.
#' @return The micro F1 score.
#' @export
microF1Metric <- function(auxData, predicted) {
  2 * microPrecMetric(auxData, predicted) * microRecMetric(auxData, predicted) /
    (microPrecMetric(auxData, predicted) + microRecMetric(auxData, predicted))
}

#' Macro Precision Metric
#' 
#' @description Computes the macro precision of the predictions.
#' @param auxData The true labels.
#' @param predicted The predicted labels.
#' @return The macro precision.
#' @export
macroPrecMetric <- function(auxData, predicted) {
  PN <- .positivesNegatives(auxData, predicted)
  sum(PN[["TP"]] / (PN[["TP"]] + PN[["FP"]])) / length(levels(auxData))
}

#' Macro Recall Metric
#' 
#' @description Computes the macro recall of the predictions.
#' @param auxData The true labels.
#' @param predicted The predicted labels.
#' @return The macro recall.
#' @export
macroRecMetric <- function(auxData, predicted) {
  PN <- .positivesNegatives(auxData, predicted)
  sum(PN[["TP"]] / (PN[["TP"]] + PN[["FN"]])) / length(levels(auxData))
}

#' Macro F1 Score Metric
#' 
#' @description Computes the macro F1 score of the predictions.
#' @param auxData The true labels.
#' @param predicted The predicted labels.
#' @return The macro F1 score.
#' @export
macroF1Metric <- function(auxData, predicted) {
  2 * macroPrecMetric(auxData, predicted) * macroRecMetric(auxData, predicted) /
    (macroPrecMetric(auxData, predicted) + macroRecMetric(auxData, predicted))
}

#' Matthews Correlation Coefficient (MCC) Metric
#' 
#' @description Computes the Matthews Correlation Coefficient (MCC) of the predictions.
#' @param auxData The true labels.
#' @param predicted The predicted labels.
#' @return The MCC.
#' @export
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

#' Mean Squared Error (MSE) Metric
#' 
#' @description Computes the mean squared error of the predictions.
#' @param auxData The true values.
#' @param predicted The predicted values.
#' @return The mean squared error.
#' @export
MSEmetric <- function(auxData, predicted) {
  mean((auxData - predicted)^2)
}

#' Kernel Density Estimation (KDE) Metric
#' 
#' @description Computes the kernel density estimation test statistic.
#' @param auxData The true values.
#' @param predicted The predicted values.
#' @return The KDE test statistic.
#' @export
kdeMetric <- function(auxData, predicted) {
  assertSuggestAvail("ks")
  ks::kde.test(
    x1 = as.numeric(auxData), x2 = as.numeric(predicted)
  ) |> purrr::pluck("zstat")
}

#' Harrel's C-Index Metric
#' 
#' @description Computes Harrel's C-Index for survival analysis.
#' @param auxData The true survival times and event indicators.
#' @param predicted The predicted survival times.
#' @return Harrel's C-Index.
#' @importFrom Hmisc rcorr.cens
#' @export
harrelCIndexMetric <- function(auxData, predicted) {
  assertSuggestAvail("Hmisc")
  harrelC1 <- Hmisc::rcorr.cens(-predicted[[2]], auxData[[2]])
  return(harrelC1["C Index"])
}

#' Begg's C-Index Metric
#' 
#' @description Computes Begg's C-Index for survival analysis.
#' @param auxData The true survival times and event indicators.
#' @param predicted The predicted survival times.
#' @return Begg's C-Index.
#' @importFrom survAUC BeggC
#' @export
beggCIndexMetric <- function(auxData, predicted) {
  assertSuggestAvail("survAUC")

  survAUC::BeggC(
    auxData[[1]], auxData[[2]], predicted[[1]], predicted[[2]]
  )
}

#' Uno's C-Index Metric
#' 
#' @description Computes Uno's C-Index for survival analysis.
#' @param auxData The true survival times and event indicators.
#' @param predicted The predicted survival times.
#' @return Uno's C-Index.
#' @importFrom survAUC UnoC
#' @export
unoCIndexMetric <- function(auxData, predicted) {
  assertSuggestAvail("survAUC")

  survAUC::UnoC(auxData[[1]], auxData[[2]], predicted)
}

#' GH C-Index Metric
#' 
#' @description Computes the GH C-Index for survival analysis.
#' @param auxData The true survival times and event indicators.
#' @param predicted The predicted survival times.
#' @return The GH C-Index.
#' @importFrom survAUC GHCI
#' @export
ghCIndexMetric <- function(auxData, predicted) {
  assertSuggestAvail("survAUC")

  survAUC::GHCI(predicted)
}

#' Brier Score Metric
#' 
#' @description Computes the Brier score for survival analysis.
#' @param auxData The true survival times and event indicators.
#' @param predicted The predicted survival times.
#' @return The Brier score.
#' @importFrom survAUC predErr
#' @export
brierScoreMetric <- function(auxData, predicted) {
  assertSuggestAvail("survAUC")

  time <- auxData[[1]][, "time"]
  survAUC::predErr(
    auxData[[1]], auxData[[2]], predicted[[1]], predicted[[2]],
    times = time, type = "brier", int.type = "unweighted"
  )$error
}

#' Time-Dependent AUC Metric
#' 
#' @description Computes the time-dependent AUC for survival analysis.
#' @param auxData The true survival times and event indicators.
#' @param predicted The predicted survival times.
#' @return The time-dependent AUC.
#' @importFrom survAUC AUC.uno
#' @export
timeDependentAUCMetric <- function(auxData, predicted) {
  assertSuggestAvail("survAUC")

  time <- auxData[[1]][, "time"]
  AUC_CD <- survAUC::AUC.uno(auxData[[1]], auxData[[2]], predicted[[2]], time)
  return(AUC_CD)
}
