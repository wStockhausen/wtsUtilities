% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createRectGrid.R
\name{createRectGrid}
\alias{createRectGrid}
\title{Create a \pkg{tibble} tibble describing a rectangular grid}
\usage{
createRectGrid(xctrs, yctrs, dx = 1, dy = 1)
}
\arguments{
\item{xctrs}{- vector of x-axis locations of grid cell centers}

\item{yctrs}{- vector of y-axis locations of grid cell centers}

\item{dx}{- rectangle width (length along x-axis)}

\item{dy}{- rectangle height (length along y-axis)}
}
\value{
a \code{\link[tibble]{tibble}}. See Details.
}
\description{
Function to create a \pkg{tibble} tibble describing a rectangular grid.
}
\details{
The returned tibble will have the following columns: 
\itemize{
 \item{xc - rectangle center location along x-axis}
 \item{yc - rectangle center location along y-axis}
 \item{id - id (character) for rectangle with center xc,yc [="(xc,yc)"]}
 \item{x - rectangle corner location along x-axis}
 \item{y - rectangle corner location along y-axis}
}
}