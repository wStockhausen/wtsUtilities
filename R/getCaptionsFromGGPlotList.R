#'
#' @title Extract captions from list of plots in printGGList format
#' 
#' @description Function to extract captions from list of plots in printGGList format.
#' 
#' @param plts - list of plots in printGGList format, with captions as list names
#' 
#' @details Uses \code{stringr::str_trim} to trim leading and trailing whitespace.
#' 
#' @return character vector with plot captions as elements
#' 
#' @export
#' 
getCaptionsFromGGPlotList<-function(plts){
    x<-names(plts);
    x<-gsub("Figure &&figno. ","",x,fixed=TRUE);
    x<-gsub("Figure &&fno. ","",x,fixed=TRUE);
    x<-gsub("\n","",x,fixed=TRUE);
    x<-stringr::str_trim(x,side="both")
    return(x);
}
