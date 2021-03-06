% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Format-DGVMData.R
\name{getField_DGVMData}
\alias{getField_DGVMData}
\title{Get a Field for DGVMData}
\usage{
getField_DGVMData(source, quant, target.STAInfo, verbose = FALSE)
}
\arguments{
\item{source}{A \code{Source} containing the meta-data about the DGVMData source}

\item{quant}{A Quantity object to specify what quantity should be opened.}

\item{verbose}{A logical, set to true to give progress/debug information}

\item{target.sta.info}{An STAInfo object defining the spatial-temporal-annual extent over which we want the data}

\item{last.year}{The last year (as a numeric) of the data to be returned}
}
\value{
A list containing firstly the data.tablle containing the data, and secondly the STAInfo for the data that we have
}
\description{
An internal function that reads data from an DGVMData .nc file.
}
\author{
Matthew Forrest \email{matthew.forrest@senckenberg.de}
}
\keyword{internal}
