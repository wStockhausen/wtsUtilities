#'
#'@title Compute the root median square of a vector 
#'
#'@description Function to compute the root median square of a vector
#'
#'@param y - vector to compute rmds for
#'
#'@return the rmds
#'
#'@export
#'
rootMedianSquare<-function(y){
  x<-y-median(y);
  return(sqrt(sum(x*x)/length(x)));
}