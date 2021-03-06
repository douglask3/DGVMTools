% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/show-methods.R
\docType{methods}
\name{show}
\alias{show}
\alias{show,PFT-method}
\alias{show,Quantity-method}
\alias{show,Period-method}
\alias{show,Source-method}
\alias{show,STAInfo-method}
\alias{show,Field-method}
\alias{show,Comparison-method}
\alias{show,BiomeScheme-method}
\alias{show,Format-method}
\title{Show a DGVMTools object}
\usage{
\S4method{show}{PFT}(object)

\S4method{show}{Quantity}(object)

\S4method{show}{Period}(object)

\S4method{show}{Source}(object)

\S4method{show}{STAInfo}(object)

\S4method{show}{Field}(object)

\S4method{show}{Comparison}(object)

\S4method{show}{BiomeScheme}(object)

\S4method{show}{Format}(object)
}
\arguments{
\item{object}{Any DGVMTools object to be shown (printed)}

\item{...}{other arguments to be passed on to a further show functions, not currently used.}
}
\description{
Show a DGVMTools object in a reasonably nice and space efficient way.
}
\details{
Simple stuff, implemented as both S3 and S4 methods.  The reason for both is that appears S3 needs to be implemented for printing inside lists
or other objects. Further tweaking of the formatting may be in order, feedback from users welcome.
}
\author{
Matthew Forrest \email{matthew.forrest@senckenberg.de}
}
