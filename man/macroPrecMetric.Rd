% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics.R
\name{macroPrecMetric}
\alias{macroPrecMetric}
\title{Macro Precision Metric}
\usage{
macroPrecMetric(auxData, predicted)
}
\arguments{
\item{auxData}{The true labels.}

\item{predicted}{The predicted labels.}
}
\value{
The macro precision.
}
\description{
Computes the macro precision of the predictions.
}
\examples{
auxData <- factor(c("A", "B", "A", "B"))
predicted <- factor(c("A", "A", "A", "B"))
macroPrecMetric(auxData, predicted)
}
