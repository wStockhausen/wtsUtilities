\name{sumBy}
\alias{sumBy}
\title{Sum variables in a dataframe by a set of factor levels}
\usage{
sumBy(df, factors, vars, sortBy = NULL, ascending = TRUE)
}
\arguments{
  \item{df}{- dataframe (or object that can be coerced to a
  dataframe) with variables to be summed and factor levels
  to be summed by}

  \item{factors}{- vector of names of columns in df to
  serve as factors in the summations}

  \item{vars}{- vector of names of column in df that should
  be summed}

  \item{sortBy}{- vector of factor column names to sort
  results by}

  \item{ascending}{- sort in ascending order}
}
\value{
a dataframe of results with columns corresponding to
factors and vars
}
\description{
Sums variables in a dataframe by a set of factor levels.
Uses \code{getFactorLevels} to identify the unique factor
combinations in columns identified as factors.
}
\details{

}

