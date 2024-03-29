% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/substituteValues.R
\name{substituteValues}
\alias{substituteValues}
\title{Substitute final values for original values in a vector}
\usage{
substituteValues(v, orig, finl)
}
\arguments{
\item{v}{\itemize{
\item vector of values on which to perform substitution
}}

\item{orig}{\itemize{
\item vector of original values, matching those in \code{finl} by position
}}

\item{finl}{\itemize{
\item vector of final values, matching those in \code{orig} by position
}}
}
\value{
a vector of the same length as v with the substituted values replacing the original values.
}
\description{
Function to substitute final values for original values in a vector.
}
\details{
original and final values are matched by position in the respective areas (i.e.
finl[j] is substituted for orig[j] in v wherever orig[j] occurs). Values in \code{v} that
are not in \code{orig} are not changed in the result.
}
