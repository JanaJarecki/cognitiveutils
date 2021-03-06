% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meansd.R
\name{print.meansd}
\alias{print.meansd}
\title{Print method for objects of class \code{"meansd"}}
\usage{
\method{print}{meansd}(x, digits = 2, na.rm = TRUE, label = FALSE)
}
\arguments{
\item{x}{A numeric vector}

\item{digits}{The number of digits to print.}

\item{na.rm}{Logical, whether to remove \code{NA} values in \code{x}.}

\item{label}{(default \code{TRUE}) Logical, \code{TRUE} adds "M" and "SD" as labels.}
}
\value{
A string holding the mean m und standard deviation sd as \code{"m (sd)"} or \code{"M = m, SD = sd"}
}
\description{
Print method for objects of class \code{"meansd"}
}
