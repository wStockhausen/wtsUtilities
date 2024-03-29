% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getCSV.R
\name{getCSV}
\alias{getCSV}
\title{Open a csv file as a dataframe.}
\usage{
getCSV(csvfile = NULL, caption = "Select csv file to import")
}
\arguments{
\item{csvfile}{\itemize{
\item name of csv file to open
}}

\item{caption}{\itemize{
\item caption for file dialog (if file name not provided)
}}
}
\value{
dataframe based on reading csv file. Returns NULL if no csv file
was provided and the user canceled selection using the file dialog.
}
\description{
Opens a csv file and returns it as a dataframe.\cr
If a file name is not provided, the function displays a (tcltk)
file chooser dialog.\cr
Uses package "tcltk".
}
\details{
Uses packages \code{utils}, \pkg{tcltk}.
}
