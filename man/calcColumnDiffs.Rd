% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcColumnDiffs.R
\name{calcColumnDiffs}
\alias{calcColumnDiffs}
\title{Calculate differences between dataframe columns by row}
\usage{
calcColumnDiffs(
  dfr,
  col,
  cols = NULL,
  adjacent = FALSE,
  dropCols = TRUE,
  dropBase = FALSE
)
}
\arguments{
\item{dfr}{\itemize{
\item dataframe
}}

\item{col}{\itemize{
\item column name for base
}}

\item{cols}{\itemize{
\item vector of column names to compute by-row differences for (or NULL)
}}

\item{adjacent}{\itemize{
\item flag to compute differences between "adjacent" columns (default=FALSE)
}}

\item{dropCols}{\itemize{
\item flag to drop original columns involved in differences (default=TRUE)
}}

\item{dropBase}{\itemize{
\item flag to drop base column involved in differences (default=FALSE)
}}
}
\value{
copy of input dataframe, with columns inv with extra columns representing column differences
}
\description{
Function to calculate differences between dataframe columns by row.
}
\details{
If \code{cols} is NULL, then differences will be computed as if \code{cols} was
the vector of names of columns to the "right" of \code{col}. If \code{adjacent=FALSE},
then all differences are relative to \code{col} as the base. Otherwise, after the
first difference, the base column is sequentially moved through those in \code{cols}.
}
