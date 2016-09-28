#'
#'@title Sum array by specified dimensions
#'
#'@description Function to sum array by specified dimensions.
#'
#'@param n - array to sum
#'@param dims - vector of indices for dimension to sum by
#'
#'@return array with dimensions used for summing
#'
#'@details Uses the \code{apply} function to sum over the requested dimension.
#'
#'@export
#'
sumByDims<-function(n,dims=1){
    sm<-apply(n,dims,sum);
    return(sm);
}