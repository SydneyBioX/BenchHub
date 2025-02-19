balAccMetric <- function(auxData, predicted)
{
  confusionMatrix <- table(auxData, predicted)
  classSizes <- rowSums(confusionMatrix)
  mean(diag(confusionMatrix) / classSizes, na.rm = TRUE)
}

balErrMetric <- function(auxData, predicted)
{
  confusionMatrix <- table(auxData, predicted)
  classSizes <- rowSums(confusionMatrix)
  classErrors <- classSizes - diag(confusionMatrix)
  mean(classErrors / classSizes, na.rm = TRUE)
}

positivesNegatives <- function(auxData, predicted)
{
  confusionMatrix <- table(auxData, predicted)
  truePositives <- diag(confusionMatrix)
  falsePositives <- colSums(confusionMatrix) - truePositives
  falseNegatives <- rowSums(confusionMatrix) - truePositives
  trueNegatives <- sum(truePositives) - truePositives
  list(TP = truePositives, FP = falsePositives,
       FN = falseNegatives, TN = trueNegatives)
}

microPrecMetric <- function(auxData, predicted)
{
  PN <- positivesNegatives(auxData, predicted)
  sum(PN[["TP"]]) / sum(PN[["TP"]] + PN[["FP"]])
}

microRecMetric <- function(auxData, predicted)
{
  PN <- positivesNegatives(auxData, predicted)
  sum(PN[["TP"]]) / sum(PN[["TP"]] + PN[["FN"]])
}

microF1Metric <- function(auxData, predicted)
{
  2 * microPrec(auxData, predicted) * microRec(auxData, predicted) /
     (microPrec(auxData, predicted) + microRec(auxData, predicted))
}


macroPrecMetric <- function(auxData, predicted)
{
  PN <- positivesNegatives(auxData, predicted)
  sum(PN[["TP"]] / (PN[["TP"]] + PN[["FP"]])) / length(levels(auxData))
}

macroRecMetric <- function(auxData, predicted)
{
  PN <- positivesNegatives(auxData, predicted)
  sum(PN[["TP"]] / (PN[["TP"]] + PN[["FN"]])) / length(levels(auxData))
  sum(truePositives / (truePositives + falseNegatives)) / length(levels(auxData))
}

macroF1Metric <- function(auxData, predicted)
{
  2 * macroPrec(auxData, predicted) * macroRec(auxData, predicted) /
      (macroPrec(auxData, predicted) + macroRec(auxData, predicted))
}

MCCmetric <- function(auxData, predicted)
{
  if(length(levels(auxData)) > 2)
    stop("Error: Matthews Correlation Coefficient calculation ran, but data set has more than two classes.")
  PN <- positivesNegatives(auxData, predicted)
  (PN[["TP"]][2] * PN[["TN"]][2] - PN[["FP"]][2] * PN[["FN"]][2]) /
    sqrt((PN[["TP"]][2] + PN[["FP"]][2]) * (PN[["TP"]][2] + PN[["FN"]][2]) * (PN[["TN"]][2] + PN[["FP"]][2]) * (PN[["TN"]][2] + PN[["FN"]][2]))
}

# One for numeric auxiliary data.
MSEmetric <- function(auxData, predicted)
{
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
  harrelC1 <- Hmisc::rcorr.cens(-predicted[[2]], auxData[[2]])
  return(harrelC1["C Index"])
}

#' @import survAUC
beggCIndexMetric <- function(auxData, predicted) {
  # cli::cli_abort(c(
  #   "Begg's c-index is not supported yet! :("
  # ))
  # auxData:list(surv_object[ train_index], surv_object[ test_index])
  # predicted: list(predictors[ train_index], predictors[test_index])
  begg_cindex <- survAUC::BeggC(auxData[[1]], auxData[[2]],predicted[[1]], predicted[[2]])
  return(begg_cindex)
}

unoCIndexMetric <- function(auxData, predicted) {
  # cli::cli_abort(c(
  #   "Uno's c-index is not supported yet! :("
  # ))
  
  uno_cindex <- survAUC::UnoC(auxData[[1]], auxData[[2]], predicted)
  return(uno_cindex)
  
}

ghCIndexMetric <- function(auxData, predicted) {
  # cli::cli_abort(c(
  #   "GH's c-index is not supported yet! :("
  # ))
  gh_cindex <- survAUC::GHCI(predicted)
  return(gh_cindex)
}

brierScoreMetric <- function(auxData, predicted) {
  # cli::cli_abort(c(
  #   "Brier's score is not supported yet! :("
  # ))
  time <- auxData[[1]][,"time"]
  brier_score <- survAUC::predErr(auxData[[1]], auxData[[2]],predicted[[1]], predicted[[2]],times=time, type = "brier", int.type = "unweighted")$error
  return(brier_score)
}

timeDependentAUCMetric <- function(auxData, predicted) {
  # cli::cli_abort(c(
  #   "Time dependent AUC is not supported yet! :("
  # ))
  time <- auxData[[1]][,"time"]
  AUC_CD <- survAUC::AUC.uno(auxData[[1]], auxData[[2]], predicted[[2]], time)
  return(AUC_CD)
}