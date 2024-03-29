% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rootMedianSquare.R
\name{rootMedianSquare}
\alias{rootMedianSquare}
\title{Compute the root median square deviation (rmsd) of a vector}
\usage{
rootMedianSquare(y, na.rm = TRUE)
}
\arguments{
\item{y}{\itemize{
\item vector to compute rmsd for
}}

\item{na.rm}{\itemize{
\item flag to remove NAs before calculating
}}
}
\value{
the rmsd, or NA_real_ if y is all NAs
}
\description{
Function to compute the root median square deviation (rmsd) of a vector.
}
\details{
Uses package \code{stats}.
}
