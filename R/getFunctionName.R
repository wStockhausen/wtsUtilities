#' @title Get the name of a function
#' 
#' @description Returns the name of the input function as a character string
#' 
#' @param fcn - function name as an expression
#' 
#' @return function name as character string
#'
#'@export
#'
getFunctionName<-function(fcn) {
    str<-deparse(substitute(fcn,sys.frame(sys.parent())));
    return(str);
}
