#'
#'@title Standardize a vector by converting to z-scores.
#'
#'@description Function to standardize a vector by converting to z-scores.
#'
#'@param x - vector to standardize
#'
#'@return standardized vector as z-scores (same size as x)
#'
#'@export
#'
standardize<-function(x){
    mn<-mean(x,na.rm=TRUE);
    sd<-sd(x,na.rm=TRUE);
    if(sd>0){
        y<-(x-mn)/sd;
    } else {
        y<-0*x;
    }
    return(y)
}