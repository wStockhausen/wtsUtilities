#'
#' @title formatZeros: replace blanks with 0's in printed numbers
#' 
#' @description Format input vector, replacing leading or trailing blanks with zeros.
#'       
#' @param x - object that can be coerced to a numeric vector using as.numeric(as.vector(x))
#' @param width - width of formatted numbers
#' @param format - c-type format string (see "format" description in base R)
#' @param ...    - additional inputs to "format"
#' 
#' @return character vector with blanks replaced by zeros
#'
#'@details None.
#'
#'@export
#'
formatZeros<-function(x,width=2,format="d",...){
    xp<-as.numeric(as.vector(x));
    cxp<-format(xp,width=width,format=format,...);
    res<-gsub(" ","0",cxp,fixed=TRUE)
    return(res)
}
