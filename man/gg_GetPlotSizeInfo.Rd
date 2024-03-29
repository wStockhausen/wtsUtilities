% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gg_GetPlotSizeInfo.R
\name{gg_GetPlotSizeInfo}
\alias{gg_GetPlotSizeInfo}
\title{Get info on grob sizes of a ggplot2 object or derived gtable}
\usage{
gg_GetPlotSizeInfo(p, unit = "in", keepAll = FALSE, verbose = 0)
}
\arguments{
\item{p}{\itemize{
\item ggplot or gtable object
}}

\item{unit}{\itemize{
\item plot units ("in", "cm", etc.)
}}

\item{keepAll}{\itemize{
\item flag (T/F) to keep all grobs (including grobs with physical size 0)
}}

\item{verbose}{\itemize{
\item flag (0,1,2) to print intermediate information (0=off)
}}
}
\value{
invisibly returns a list with elements
lyt - vector with plot width and height
tbl_rows - table with row sizes
sum_rows - table with summed height of rows
tbl_cols - table with column sizes
sum_cols - table with summed width of columns
}
\description{
Function to get info on grob sizes  of a ggplot2 object or derived gtable.
}
\details{
This function identifies scales of components in a ggplot2 object. The
row/column sizes (and sums)  may involve null units reflecting expandable grobs.
}
\author{
William Stockhausen (william.stockhausen@noaa.gov)
}
