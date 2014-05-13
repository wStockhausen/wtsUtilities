#' @title Returns count of x (ignoring NAs)
#' 
#' @description Returns count of x (ignoring NAs)
#' 
#' @param x - a vector of numbers (or object coerceable to a vector)
#' 
#' @return number of non-NAs in x
#'
#'@export
#'
count<-function(x){
    x<-as.vector(x);
    res<-sum(!is.na(x));
    return(res);
}