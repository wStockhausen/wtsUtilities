\name{sortBy}
\alias{sortBy}
\title{sort a dataframe by row according to columns.}
\usage{
sortBy(df, factors, na.last = TRUE, ascending = TRUE, debug = FALSE)
}
\arguments{
  \item{df}{- input dataframe (or object that can be cast
  to a dataframe)}

  \item{factors}{- vector of desired column names from df
  to sort by}

  \item{na.last}{- flag (T/F) to order NAs last/first (=NA
  removes any NAs)}

  \item{ascending}{- flag (T/F) to sort in
  ascending/descending order}

  \item{debug}{- flag (T/F) to print debugging info}
}
\value{
sorted version of df
}
\description{
Returns a dataframe sorted by row according to the
combination of values in the columns specified by
"factors".
}
\details{

}

