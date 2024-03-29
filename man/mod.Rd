% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod.R
\name{mod}
\alias{mod}
\title{Calculate the modulus of two numbers}
\usage{
mod(x, y)
}
\arguments{
\item{x}{\itemize{
\item a number or vector
}}

\item{y}{\itemize{
\item a number or vector
}}
}
\value{
the result x modulo y
}
\description{
Calculates the modulus of two numbers using the equation
z = x-y*floor(x/y)
}
