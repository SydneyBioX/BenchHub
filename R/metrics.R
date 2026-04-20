#' Balanced Accuracy Metric
#'
#' @description Computes the balanced accuracy of the predictions.
#' @param evidence The true labels.
#' @param predicted The predicted labels.
#' @return The balanced accuracy.
#' @examples
#' evidence <- factor(c("A", "B", "A", "B"))
#' predicted <- factor(c("A", "A", "A", "B"))
#' balAccMetric(evidence, predicted)
#' @export
balAccMetric <- function(evidence, predicted) {
  confusionMatrix <- table(evidence, predicted)
  classSizes <- rowSums(confusionMatrix)
  mean(diag(confusionMatrix) / classSizes, na.rm = TRUE)
}

#' Balanced Error Metric
#'
#' @description Computes the balanced error of the predictions.
#' @param evidence The true labels.
#' @param predicted The predicted labels.
#' @return The balanced error.
#' @examples
#' evidence <- factor(c("A", "B", "A", "B"))
#' predicted <- factor(c("A", "A", "A", "B"))
#' balErrMetric(evidence, predicted)
#' @export
balErrMetric <- function(evidence, predicted) {
  confusionMatrix <- table(evidence, predicted)
  classSizes <- rowSums(confusionMatrix)
  classErrors <- classSizes - diag(confusionMatrix)
  mean(classErrors / classSizes, na.rm = TRUE)
}

#' Compute Positives and Negatives
#'
#' @description Computes the true positives, false positives, false negatives, and true negatives.
#' @param evidence The true labels.
#' @param predicted The predicted labels.
#' @return A list containing the true positives, false positives, false negatives, and true negatives.
#' @examples
#' evidence <- factor(c("A", "B", "A", "B"))
#' predicted <- factor(c("A", "A", "A", "B"))
#' .positivesNegatives(evidence, predicted)
#' @keywords internal
.positivesNegatives <- function(evidence, predicted) {
  confusionMatrix <- table(evidence, predicted)
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
#' @param evidence The true labels.
#' @param predicted The predicted labels.
#' @return The micro precision.
#' @examples
#' evidence <- factor(c("A", "B", "A", "B"))
#' predicted <- factor(c("A", "A", "A", "B"))
#' microPrecMetric(evidence, predicted)
#' @export
microPrecMetric <- function(evidence, predicted) {
  PN <- .positivesNegatives(evidence, predicted)
  sum(PN[["TP"]]) / sum(PN[["TP"]] + PN[["FP"]])
}

#' Micro Recall Metric
#'
#' @description Computes the micro recall of the predictions.
#' @param evidence The true labels.
#' @param predicted The predicted labels.
#' @return The micro recall.
#' @examples
#' evidence <- factor(c("A", "B", "A", "B"))
#' predicted <- factor(c("A", "A", "A", "B"))
#' microRecMetric(evidence, predicted)
#' @export
microRecMetric <- function(evidence, predicted) {
  PN <- .positivesNegatives(evidence, predicted)
  sum(PN[["TP"]]) / sum(PN[["TP"]] + PN[["FN"]])
}

#' Micro F1 Score Metric
#'
#' @description Computes the micro F1 score of the predictions.
#' @param evidence The true labels.
#' @param predicted The predicted labels.
#' @return The micro F1 score.
#' @examples
#' evidence <- factor(c("A", "B", "A", "B"))
#' predicted <- factor(c("A", "A", "A", "B"))
#' microF1Metric(evidence, predicted)
#' @export
microF1Metric <- function(evidence, predicted) {
  2 * microPrecMetric(evidence, predicted) * microRecMetric(evidence, predicted) /
    (microPrecMetric(evidence, predicted) + microRecMetric(evidence, predicted))
}

#' Macro Precision Metric
#'
#' @description Computes the macro precision of the predictions.
#' @param evidence The true labels.
#' @param predicted The predicted labels.
#' @return The macro precision.
#' @examples
#' evidence <- factor(c("A", "B", "A", "B"))
#' predicted <- factor(c("A", "A", "A", "B"))
#' macroPrecMetric(evidence, predicted)
#' @export
macroPrecMetric <- function(evidence, predicted) {
  PN <- .positivesNegatives(evidence, predicted)
  sum(PN[["TP"]] / (PN[["TP"]] + PN[["FP"]])) / length(levels(evidence))
}

#' Macro Recall Metric
#'
#' @description Computes the macro recall of the predictions.
#' @param evidence The true labels.
#' @param predicted The predicted labels.
#' @return The macro recall.
#' @examples
#' evidence <- factor(c("A", "B", "A", "B"))
#' predicted <- factor(c("A", "A", "A", "B"))
#' macroRecMetric(evidence, predicted)
#' @export
macroRecMetric <- function(evidence, predicted) {
  PN <- .positivesNegatives(evidence, predicted)
  sum(PN[["TP"]] / (PN[["TP"]] + PN[["FN"]])) / length(levels(evidence))
}

#' Macro F1 Score Metric
#'
#' @description Computes the macro F1 score of the predictions.
#' @param evidence The true labels.
#' @param predicted The predicted labels.
#' @return The macro F1 score.
#' @examples
#' evidence <- factor(c("A", "B", "A", "B"))
#' predicted <- factor(c("A", "A", "A", "B"))
#' macroF1Metric(evidence, predicted)
#' @export
macroF1Metric <- function(evidence, predicted) {
  2 * macroPrecMetric(evidence, predicted) * macroRecMetric(evidence, predicted) /
    (macroPrecMetric(evidence, predicted) + macroRecMetric(evidence, predicted))
}

#' Matthews Correlation Coefficient (MCC) Metric
#'
#' @description Computes the Matthews Correlation Coefficient (MCC) of the predictions.
#' @param evidence The true labels.
#' @param predicted The predicted labels.
#' @return The MCC.
#' @examples
#' evidence <- factor(c("A", "B", "A", "B"))
#' predicted <- factor(c("A", "A", "A", "B"))
#' MCCmetric(evidence, predicted)
#' @export
MCCmetric <- function(evidence, predicted) {
  nClass <- length(levels(evidence))
  if (nClass != 2) {
    cli::cli_abort(c(
      "Matthews Correlation Coefficient (MCC) calculation failed.",
      "i" = "Selected data has {nClass} classes ({.val {levels(evidence)}}).",
      "i" = "MCC only supports 2 classes."
    ))
  }
  PN <- .positivesNegatives(evidence, predicted)
  (PN[["TP"]][2] * PN[["TN"]][2] - PN[["FP"]][2] * PN[["FN"]][2]) /
    sqrt(
      (PN[["TP"]][2] + PN[["FP"]][2]) * (PN[["TP"]][2] + PN[["FN"]][2]) *
        (PN[["TN"]][2] + PN[["FP"]][2]) * (PN[["TN"]][2] + PN[["FN"]][2])
    )
}

#' Mean Squared Error (MSE) Metric
#'
#' @description Computes the mean squared error of the predictions.
#' @param evidence The true values.
#' @param predicted The predicted values.
#' @return The mean squared error.
#' @examples
#' evidence <- c(1, 2, 3, 4)
#' predicted <- c(1.1, 2.1, 2.9, 4.2)
#' MSEmetric(evidence, predicted)
#' @export
MSEmetric <- function(evidence, predicted) {
  mean((evidence - predicted)^2)
}

#' Root Mean Squared Error (RMSE) Metric
#'
#' @description Computes the root mean squared error of the predictions.
#' @param evidence The true values.
#' @param predicted The predicted values.
#' @return The root mean squared error.
#' @examples
#' evidence <- c(1, 2, 3, 4)
#' predicted <- c(1.1, 2.1, 2.9, 4.2)
#' RMSEmetric(evidence, predicted)
#' @export
RMSEmetric <- function(evidence, predicted) {
  unname(sqrt(MSEmetric(evidence, predicted)))
}

#' Adjusted Rand Index (ARI) Metric
#'
#' @description Computes the adjusted Rand index between two cluster labelings.
#' @param evidence The true labels.
#' @param predicted The predicted labels.
#' @return The adjusted Rand index.
#' @examples
#' evidence <- factor(c("A", "A", "B", "B"))
#' predicted <- factor(c("A", "A", "B", "B"))
#' ARImetric(evidence, predicted)
#' @export
ARImetric <- function(evidence, predicted) {
  contingency <- table(evidence, predicted)
  n <- sum(contingency)

  if (n < 2) {
    return(NA_real_)
  }

  sum_comb_cells <- sum(choose(contingency, 2))
  row_totals <- rowSums(contingency)
  col_totals <- colSums(contingency)
  sum_comb_rows <- sum(choose(row_totals, 2))
  sum_comb_cols <- sum(choose(col_totals, 2))
  total_comb <- choose(n, 2)

  expected_index <- (sum_comb_rows * sum_comb_cols) / total_comb
  max_index <- 0.5 * (sum_comb_rows + sum_comb_cols)
  denominator <- max_index - expected_index

  if (denominator == 0) {
    return(if (identical(as.character(evidence), as.character(predicted))) 1 else 0)
  }

  unname((sum_comb_cells - expected_index) / denominator)
}

#' Normalized Mutual Information (NMI) Metric
#'
#' @description Computes the normalized mutual information between two cluster
#'   labelings.
#' @param evidence The true labels.
#' @param predicted The predicted labels.
#' @return The normalized mutual information.
#' @examples
#' evidence <- factor(c("A", "A", "B", "B"))
#' predicted <- factor(c("A", "A", "B", "B"))
#' NMImetric(evidence, predicted)
#' @export
NMImetric <- function(evidence, predicted) {
  contingency <- table(evidence, predicted)
  n <- sum(contingency)

  if (n == 0) {
    return(NA_real_)
  }

  joint_prob <- contingency / n
  evidence_prob <- rowSums(joint_prob)
  predicted_prob <- colSums(joint_prob)

  mutual_information <- 0
  for (i in seq_len(nrow(joint_prob))) {
    for (j in seq_len(ncol(joint_prob))) {
      p_ij <- joint_prob[i, j]
      if (p_ij > 0) {
        mutual_information <- mutual_information + p_ij * log(
          p_ij / (evidence_prob[i] * predicted_prob[j])
        )
      }
    }
  }

  evidence_entropy <- -sum(evidence_prob[evidence_prob > 0] * log(evidence_prob[evidence_prob > 0]))
  predicted_entropy <- -sum(predicted_prob[predicted_prob > 0] * log(predicted_prob[predicted_prob > 0]))
  denominator <- sqrt(evidence_entropy * predicted_entropy)

  if (denominator == 0) {
    return(if (identical(as.character(evidence), as.character(predicted))) 1 else 0)
  }

  unname(mutual_information / denominator)
}

#' Jensen-Shannon Divergence (JSD) Metric
#'
#' @description Computes the Jensen-Shannon divergence between two non-negative
#'   numeric vectors after normalizing them to probability distributions.
#' @param evidence The true values.
#' @param predicted The predicted values.
#' @return The Jensen-Shannon divergence.
#' @examples
#' evidence <- c(0.2, 0.3, 0.5)
#' predicted <- c(0.1, 0.4, 0.5)
#' JSDmetric(evidence, predicted)
#' @export
JSDmetric <- function(evidence, predicted) {
  evidence <- as.numeric(evidence)
  predicted <- as.numeric(predicted)

  if (length(evidence) != length(predicted)) {
    cli::cli_abort("{.arg evidence} and {.arg predicted} must have the same length.")
  }

  if (any(evidence < 0, na.rm = TRUE) || any(predicted < 0, na.rm = TRUE)) {
    cli::cli_abort("{.arg evidence} and {.arg predicted} must contain only non-negative values.")
  }

  evidence_sum <- sum(evidence)
  predicted_sum <- sum(predicted)
  if (evidence_sum == 0 || predicted_sum == 0) {
    cli::cli_abort("{.arg evidence} and {.arg predicted} must each sum to a positive value.")
  }

  p <- evidence / evidence_sum
  q <- predicted / predicted_sum
  m <- 0.5 * (p + q)

  kl_divergence <- function(x, y) {
    non_zero <- x > 0
    sum(x[non_zero] * log2(x[non_zero] / y[non_zero]))
  }

  unname(0.5 * kl_divergence(p, m) + 0.5 * kl_divergence(q, m))
}

#' Kernel Density Estimation (KDE) Metric
#'
#' @description Computes the kernel density estimation test statistic.
#' @param evidence The true values.
#' @param predicted The predicted values.
#' @return The KDE test statistic.
#' @examples
#' evidence <- c(1, 2, 3, 4)
#' predicted <- c(1.1, 2.1, 2.9, 4.2)
#' kdeMetric(evidence, predicted)
#' @export
kdeMetric <- function(evidence, predicted) {
  assertSuggestAvail("ks")
  ks::kde.test(
    x1 = as.numeric(evidence), x2 = as.numeric(predicted)
  ) |> purrr::pluck("zstat")
}

#' Harrel's C-Index Metric
#'
#' @description Computes Harrel's C-Index for survival analysis.
#' @param evidence The true survival times and event indicators.
#' @param predicted The predicted survival times.
#' @return Harrel's C-Index.
#' @examples
#' # More realistic training dataset (8 patients)
#' evidence <- list(
#'   survival::Surv(time = c(5, 10, 15, 20, 25, 30, 35, 40), 
#'   event = c(1, 1, 0, 1, 0, 1, 1, 0)),  # Training
#'   survival::Surv(time = c(12, 18, 25, 32),
#'   event = c(1, 0, 1, 0))  # Testing
#' )
#' # Predicted risk scores
#' predicted <- list(
#'   c(0.5142118, 0.3902035, 0.9057381, 0.4469696, 
#'   0.8360043, 0.7375956, 0.8110551, 0.3881083),  # Training predictions
#'   c(0.685169729, 0.003948339, 0.832916080, 0.007334147)  # Testing predictions
#' )
#' harrelCIndexMetric(evidence, predicted)
#' @importFrom Hmisc rcorr.cens
#' @export
harrelCIndexMetric <- function(evidence, predicted) {
  assertSuggestAvail("Hmisc")
  harrelC1 <- Hmisc::rcorr.cens(-predicted[[2]], evidence[[2]])
  return(harrelC1["C Index"])
}

#' Begg's C-Index Metric
#'
#' @description Computes Begg's C-Index for survival analysis.
#' @param evidence The true survival times and event indicators.
#' @param predicted The predicted survival times.
#' @return Begg's C-Index.
#' @examples
#' # More realistic training dataset (8 patients)
#' evidence <- list(
#'   survival::Surv(time = c(5, 10, 15, 20, 25, 30, 35, 40), 
#'   event = c(1, 1, 0, 1, 0, 1, 1, 0)),  # Training
#'   survival::Surv(time = c(12, 18, 25, 32), 
#'   event = c(1, 0, 1, 0))  # Testing
#' )
#' # Predicted risk scores
#' predicted <- list(
#'   c(0.5142118, 0.3902035, 0.9057381, 0.4469696, 
#'   0.8360043, 0.7375956, 0.8110551, 0.3881083),  # Training predictions
#'   c(0.685169729, 0.003948339, 0.832916080, 0.007334147)  # Testing predictions
#' )
#' beggCIndexMetric(evidence, predicted)
#' @importFrom survAUC BeggC
#' @export
beggCIndexMetric <- function(evidence, predicted) {
  assertSuggestAvail("survAUC")

  survAUC::BeggC(
    evidence[[1]], evidence[[2]], predicted[[1]], predicted[[2]]
  )
}

#' Uno's C-Index Metric
#'
#' @description Computes Uno's C-Index for survival analysis.
#' @param evidence The true survival times and event indicators.
#' @param predicted The predicted survival times.
#' @return Uno's C-Index.
#' @examples
#' # More realistic training dataset (8 patients)
#' evidence <- list(
#'   survival::Surv(time = c(5, 10, 15, 20, 25, 30, 35, 40), 
#'   event = c(1, 1, 0, 1, 0, 1, 1, 0)),  # Training
#'   survival::Surv(time = c(12, 18, 25, 32),
#'   event = c(1, 0, 1, 0))  # Testing
#' )
#' # Predicted risk scores
#' predicted <- list(
#'   c(0.5142118, 0.3902035, 0.9057381, 0.4469696, 
#'   0.8360043, 0.7375956, 0.8110551, 0.3881083),  # Training predictions
#'   c(0.685169729, 0.003948339, 0.832916080, 0.007334147)  # Testing predictions
#' )
#' unoCIndexMetric(evidence, predicted)
#' @importFrom survAUC UnoC
#' @export
unoCIndexMetric <- function(evidence, predicted) {
  assertSuggestAvail("survAUC")

  survAUC::UnoC(Surv.rsp = evidence[[2]], Surv.rsp.new = evidence[[2]], 
                lpnew = predicted[[2]])
}

#' GH C-Index Metric
#'
#' @description Computes the GH C-Index for survival analysis.
#' @param evidence The true survival times and event indicators.
#' @param predicted The predicted survival times.
#' @return The GH C-Index.
#' @examples
#' # More realistic training dataset (8 patients)
#' evidence <- list(
#'   survival::Surv(time = c(5, 10, 15, 20, 25, 30, 35, 40), 
#'   event = c(1, 1, 0, 1, 0, 1, 1, 0)),  # Training
#'   survival::Surv(time = c(12, 18, 25, 32), 
#'   event = c(1, 0, 1, 0))  # Testing
#' )
#' # Predicted risk scores
#' predicted <- list(
#'   c(0.5142118, 0.3902035, 0.9057381, 0.4469696, 
#'   0.8360043, 0.7375956, 0.8110551, 0.3881083),  # Training predictions
#'   c(0.685169729, 0.003948339, 0.832916080, 0.007334147)  # Testing predictions
#' )
#' ghCIndexMetric(evidence, predicted)
#' @importFrom survAUC GHCI
#' @export
ghCIndexMetric <- function(evidence, predicted) {
  assertSuggestAvail("survAUC")

  survAUC::GHCI(predicted[[2]])
}

#' Brier Score Metric
#'
#' @description Computes the Brier score for survival analysis.
#' @param evidence The true survival times and event indicators.
#' @param predicted The predicted survival times.
#' @return The Brier score.
#' @examples
#' # More realistic training dataset (8 patients)
#' evidence <- list(
#'   survival::Surv(time = c(5, 10, 15, 20, 25, 30, 35, 40), 
#'   event = c(1, 1, 0, 1, 0, 1, 1, 0)),  # Training
#'   survival::Surv(time = c(12, 18, 25, 32),
#'   event = c(1, 0, 1, 0))  # Testing
#' )
#' # Predicted risk scores
#' predicted <- list(
#'   c(0.5142118, 0.3902035, 0.9057381, 0.4469696, 
#'   0.8360043, 0.7375956, 0.8110551, 0.3881083),  # Training predictions
#'   c(0.685169729, 0.003948339, 0.832916080, 0.007334147)  # Testing predictions
#' )
#' brierScoreMetric(evidence, predicted)
#' @importFrom survAUC predErr
#' @export
brierScoreMetric <- function(evidence, predicted) {
  assertSuggestAvail("survAUC")

  time <- evidence[[1]][, "time"]
  survAUC::predErr(
    evidence[[1]], evidence[[2]], predicted[[1]], predicted[[2]],
    times = time, type = "brier", int.type = "unweighted"
  )$error
}

#' Time-Dependent AUC Metric
#'
#' @description Computes the time-dependent AUC for survival analysis.
#' @param evidence The true survival times and event indicators.
#' @param predicted The predicted survival times.
#' @return The time-dependent AUC.
#' @examples
#' # More realistic training dataset (8 patients)
#' evidence <- list(
#'   survival::Surv(time = c(5, 10, 15, 20, 25, 30, 35, 40), 
#'   event = c(1, 1, 0, 1, 0, 1, 1, 0)),  # Training
#'   survival::Surv(time = c(12, 18, 25, 32),
#'   event = c(1, 0, 1, 0))  # Testing
#' )
#'
#' # Predicted risk scores
#' predicted <- list(
#'   c(0.5142118, 0.3902035, 0.9057381, 0.4469696, 
#'   0.8360043, 0.7375956, 0.8110551, 0.3881083),  # Training predictions
#'   c(0.685169729, 0.003948339, 0.832916080, 0.007334147)  # Testing predictions
#' )
#' timeDependentAUCMetric(evidence, predicted)
#' @importFrom survAUC AUC.uno
#' @export
timeDependentAUCMetric <- function(evidence, predicted) {
  assertSuggestAvail("survAUC")

  time <- evidence[[1]][, "time"]
  AUC_CD <- survAUC::AUC.uno(evidence[[1]], evidence[[2]], predicted[[2]], time)$auc
  return(AUC_CD)
}
