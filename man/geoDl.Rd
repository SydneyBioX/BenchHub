% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/downloaders.R
\name{geoDl}
\alias{geoDl}
\title{Download files from geo}
\usage{
geoDl(ID, cachePath)
}
\arguments{
\item{ID}{The ID, formatted either "GSEXXXXXX" for the main file or
"GSEXXXXXX/SupFile.tar.gz" or a supplementary file.}

\item{cachePath}{The path to store the downloaded file.}
}
\value{
The path to the downloaded file.
}
\description{
Download main or supplementary files from GEO.
}
\keyword{internal}
