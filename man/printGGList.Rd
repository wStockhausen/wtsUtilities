% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/printGGList.R
\name{printGGList}
\alias{printGGList}
\title{Print a ggplot2 object, or list of ggplot2 objects}
\usage{
printGGList(plots, figno = NULL, cap = NULL, show = TRUE)
}
\arguments{
\item{plots}{\itemize{
\item a ggplot2 object or a list of ggplot2 objects
}}

\item{figno}{\itemize{
\item figure number (or NULL)
}}

\item{cap}{\itemize{
\item plot caption
}}

\item{show}{\itemize{
\item flag (T/F) to actually print plots
}}
}
\value{
list with elements:
\itemize{
\item{figno - figure number for next plot, if figno was not NULL}
\item{plotlist - non-nested list of plots, with numbered captions as names}
}
}
\description{
Function to print a ggplot2 object or a list of ggplot2 objects.
}
\details{
Captions for each figure are taken from the name of the list element associated with the plot.
\code{figno} is substituted for '&&fno' or '&&figno' in the caption and incremented for the next plot.
}
