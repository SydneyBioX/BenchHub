% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics.R
\name{ghCIndexMetric}
\alias{ghCIndexMetric}
\title{GH C-Index Metric}
\usage{
ghCIndexMetric(auxData, predicted)
}
\arguments{
\item{auxData}{The true survival times and event indicators.}

\item{predicted}{The predicted survival times.}
}
\value{
The GH C-Index.
}
\description{
Computes the GH C-Index for survival analysis.
}
\examples{
# More realistic training dataset (8 patients)
auxData <- list(
  survival::Surv(time = c(5, 10, 15, 20, 25, 30, 35, 40), 
  event = c(1, 1, 0, 1, 0, 1, 1, 0)),  # Training
  survival::Surv(time = c(12, 18, 25, 32), 
  event = c(1, 0, 1, 0))  # Testing
)
# Predicted risk scores
predicted <- list(
  c(0.5142118, 0.3902035, 0.9057381, 0.4469696, 
  0.8360043, 0.7375956, 0.8110551, 0.3881083),  # Training predictions
  c(0.685169729, 0.003948339, 0.832916080, 0.007334147)  # Testing predictions
)
ghCIndexMetric(auxData, predicted)
}
