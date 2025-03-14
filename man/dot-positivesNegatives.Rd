% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics.R
\name{.positivesNegatives}
\alias{.positivesNegatives}
\title{Compute Positives and Negatives}
\usage{
.positivesNegatives(auxData, predicted)
}
\arguments{
\item{auxData}{The true labels.}

\item{predicted}{The predicted labels.}
}
\value{
A list containing the true positives, false positives, false negatives, and true negatives.
}
\description{
Computes the true positives, false positives, false negatives, and true negatives.
}
\examples{
auxData <- factor(c("A", "B", "A", "B"))
predicted <- factor(c("A", "A", "A", "B"))
.positivesNegatives(auxData, predicted)
}
\keyword{internal}
