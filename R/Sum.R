#'
#'@title Sum with NAs removed
#'
#'@description Convenience function to sum with NAs removed.
#'
#'@param x - object to be summed
#'@param verbose - flg to print diagnostic info
#'
#'@details  If x is numeric, then \code{Sum(x)} is equivalent to \code{sum(x,na.rm=TRUE)}. If x is a character vector,
#'then \code{Sum(x)} returns x\[1\]. If x is logical, then \code{Sum(x)} is equivalent to \code{any(x,na.rm=TRUE)}
#'
#'@export
#'
Sum<-function(x,verbose=FALSE){
    if (length(x)==0) {
        return(0.0);
    } else if (is.null(x)) {
        return(0.0);
    } else if (is.numeric(x)){
        if (verbose) cat("x is numeric\n");
        res<-(sum(x,na.rm=TRUE));
    } else if (is.logical(x)){
        if (verbose) cat("x is logical\n");
        res<-sum(as.numeric(x),na.rm=TRUE);
    } else if (is.character(x)){
        if (verbose) cat("x is character\n");
        res<-(x[!is.na(x)])[1];
    } else if (is.factor(x)){
        if (verbose) cat("x is factor\n");
        res<-as.character(x)[1];
    }
    return(res);
}