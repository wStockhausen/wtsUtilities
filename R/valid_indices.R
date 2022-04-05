#' 
#' @title Determine valid indices
#' 
#' @description Function to determine valid indices in a candidate vector
#' 
#' @param idx - vector of "candidate" indices
#' @param n - max valid index
#' 
#' @return vector of valid indices, in order they occur in \code{idx}
#' 
#' @details Valid indices into another vector are assumed to run from 1 to \code{n}. The
#' function returns the vector of valid indices, given \code{n}, in the input vector \code{idx}.
#' 
#' @export
#' 
valid_indices<-function(idx,n){
  return(idx[(0<idx)&(idx<=n)]);
}
