#'
#'@title Sum with NAs removed
#'
#'@description Convenience function to sum with NAs removed.
#'
#'@param x - object to be summed
#'
#'@details  \code{Sum(x)} is equivalent to \code{sum(x,na.rm=TRUE)}
#'
#'@export
#'
Sum<-function(x){return(sum(x,na.rm=TRUE));}