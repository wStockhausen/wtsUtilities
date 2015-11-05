#'
#'@title Reclassify values in a vector.
#'
#'@description Function to reclassify values in a vector
#'
#'@param x - vector with values to reclassify
#'@param rcl - list of lists specifying reclassifications
#'
#'@return vector of length(x)
#'
#'@details "rcl" is a list of lists. Each sublist is of the form list(new=new, old=old),
#'where the value "new" is the new classification assigned to each element in "x" 
#'that is found in the vector "old".
#'
#'@export
#'
reclassify<-function(x,rcl){
  y<-x;
  n<-length(rcl);
  for (i in 1:n){
    idx<-(x %in% rcl[[i]]$old);
    y[idx]<-rcl[[i]]$new;
  }
  return(y);
}
