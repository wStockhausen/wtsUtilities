#'
#' @title Substitute final values for original values in a vector
#' 
#' @description Function to substitute final values for original values in a vector.
#' 
#' @param v - vector of values on which to perform substition
#' @param orig - vector of original values, matching those in \code{finl} by position 
#' @param finl - vector of final values, matching those in \code{orig} by position
#' 
#' @return a vector of the same length as v with the substituted values replacing the original values.
#' 
#' @details original and final values are matched by position in the respective areas (i.e.
#' finl[j] is substituted for orig[j] in v wherever orig[j] occurs). Values in \code{v} that 
#' are not in \code{orig} are not changed in the result.
#' 
#' @export
#' 
substituteValues<-function(v,orig,finl){
    nO<-length(orig);
    nF<-length(finl);
    for (i in 1:nO){
        v[v==orig[i]]<-finl[i];
    }
    return(v);
}
