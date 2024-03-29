% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gg_ShowPlotDims.R
\name{gg_ShowPlotDims}
\alias{gg_ShowPlotDims}
\title{Determine and show dimensions of a ggplot2 object or derived gtable}
\usage{
gg_ShowPlotDims(
  p,
  unit = "in",
  null = 3,
  null_x = NULL,
  null_y = NULL,
  xtick = 0.5,
  ytick = xtick,
  showPlot = TRUE,
  verbose = 0
)
}
\arguments{
\item{p}{\itemize{
\item ggplot2 or derived gtable object
}}

\item{unit}{\itemize{
\item plot units ("in", "cm", etc.)
}}

\item{null}{\itemize{
\item size of (x and y) null (resizeable) units (in plot units; overridden if null_x and null_y are given)
}}

\item{null_x}{\itemize{
\item size of horizontal null (resizeable) units (in plot units; must also specify null_y)
}}

\item{null_y}{\itemize{
\item size of vertical (resizeable) units (in plot units; must also specify null_x)
}}

\item{xtick}{\itemize{
\item x-axis tick separation
}}

\item{ytick}{\itemize{
\item y-axis tick separation
}}

\item{showPlot}{\itemize{
\item flag (T/F) to print plots
}}

\item{verbose}{\itemize{
\item flag (0,1,2) to print intermediate information (0=off)
}}
}
\value{
invisibly returns a list with elements
plot - plot of object components with axes reflecting inferred sizes
dims - vector with plot width and height
lyt - layout table (if verbose>1)
tbl_rows - row dimensions (if verbose>1)
tbl_cols - column dimensions (if verbose>1)
tbl_matp -tibble with intermediate information (if verbose>1)
tbl_matpp - tibble with intermediate information (if verbose>1)
}
\description{
Function to determine and show dimensions of a ggplot2 object or derived gtable.
}
\details{
This function converts "null" (relative) scales in a ggplot2 object
(or derived gtable) to absolute scales
and creates a rectangle plot of the resulting components with axes reflecting
the absolute scales.

If either null_x or null_y is NULL, then the value of \code{null} is used for both. If
both null_x and null_y are not NULL, these values are used for nulls along the correpsonding
axes.
}
\author{
William Stockhausen (william.stockhausen@noaa.gov)
}
