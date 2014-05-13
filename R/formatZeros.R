#'
#' @title formatZeros: replace blanks with 0's in printed numbers
#' 
#' @description Format input vector, replacing leading or trailing blanks with zeros.
#'       
#' @param x - numeric vector
#' @param width - width of formatted numbers
#' @param format - c-type format string (see "format" description in base R)
#' @param ...    - additional inputs to "format"
#' 
#' @return character vector with blanks replaced by zeros
#'
#'@export
#'
formatZeros<-function(x,width=2,format="d",...){
    xp<-as.vector(as.numeric(x));
    cxp<-format(xp,width=width,format=format,...);
    res<-gsub(" ","0",cxp,fixed=TRUE)
    return(res)
}
