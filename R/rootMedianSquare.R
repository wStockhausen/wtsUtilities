#'
#'@title Compute the root median square deviation (rmsd) of a vector 
#'
#'@description Function to compute the root median square deviation (rmsd) of a vector
#'
#'@param y - vector to compute rmsd for
#'@param na.rm - flag to remove NAs before calculating
#'
#'@return the rmsd, or NA_real_ if y is all NAs 
#'
#'@details None.
#'
#'@export
#'
rootMedianSquare<-function(y,na.rm=TRUE){
  x<-y-median(y,na.rm=na.rm);
  if (length(x)>0) return(sqrt(sum(x*x)/length(x)));
  return(NA_real_);
}