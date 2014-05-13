#' @title Calculate the modulus of two numbers
#' 
#' @description Calculates the modulus of two numbers using the equation
#' z = x-y*floor(x/y)
#' 
#' @param x - a number or vector
#' @param y - a number or vector
#' 
#' @return the result x modulo y
#'
#'@export
#'
mod<-function(x,y) {
    z<-x-y*floor(x/y);
    return(z);
}
