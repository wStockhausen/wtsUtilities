#'
#'@title Calculate the cv (or std dev, if mean=0) of a vector.
#'
#'@description Function to calculate the cv (or std dev, if mean=0) of a vector.
#'
#'@param x - vector 
#'
#'@return cv (or std dev, if mean=0).
#'
#'@details None.
#'
#'@export
#'
cvp<-function(x){
    y<-0;
    if (mean(y,na.rm=TRUE)==0){
        y<-sqrt(var(x,na.rm=TRUE));
    } else {
        y<-sqrt(var(x,na.rm=TRUE))/mean(y,na.rm=TRUE);
    }
        
    return(y);
}

