#'
#'@title Calculate geometric mean with NAs removed
#'
#'@description Function to calculate geometric mean with NAs removed.
#'
#'@param x - vector
#'@param verbose - flag to print diagnostic info
#'
#'@return a numeric value.
#'
#'@details  If x has any 0's, then 0 is returned. Otherwise exp((mean(log(x),na.rm=TRUE))).
#'
#'@export
#'
GeomMean<-function(x,verbose=FALSE){
    if (length(x)==0) {
        return(0.0);
    } else if (is.null(x)) {
        return(0.0);
    } else if (is.numeric(x)){
        if (verbose) cat("x is numeric\n");
        if (any(x==0)) return (0.0);
        res<-exp((mean(log(x),na.rm=TRUE)));
    }     
    return(res);
}

#'
#'@title Calculate harmonic mean with NAs removed
#'
#'@description Function to calculate harmonic mean with NAs removed.
#'
#'@param x - vector
#'@param verbose - flag to print diagnostic info
#'
#'@return a numeric value.
#'
#'@details  If x has any 0's, then 0 is returned. Otherwise 1/(mean(1/x,na.rm=TRUE)).
#'
#'@export
#'
HarmMean<-function(x,verbose=FALSE){
    if (length(x)==0) {
        return(0.0);
    } else if (is.null(x)) {
        return(0.0);
    } else if (is.numeric(x)){
        if (verbose) cat("x is numeric\n");
        if (any(x==0)) return (0.0);
        res<-1/(mean(1/x,na.rm=TRUE));
    }     
    return(res);
}

