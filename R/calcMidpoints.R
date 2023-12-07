#' 
#' @title Calculate midpoints from a vector ofcuutpoints 
#' @description Funciton to calculate midpoints from a vector ofcuutpoints 
#' @param x - numeric vector of cutpoints 
#' @return vector of midpoints (length = length(x)-1)
#' @details calculates the arithmetic midpoints of the successive intervals 
#' defined by x.
#' @export
calcMidpoints<-function(x){
    y = x[1:(length(x)-1)] + base::diff(x,1)/2;
    return(y);
}
