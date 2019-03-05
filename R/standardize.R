#'
#'@title Standardize a vector or columns in a dataframe by converting to z-scores.
#'
#'@description Function to standardize a vector or columns in a dataframe by converting to z-scores.
#'
#'@param x - vector or dataframe to standardize
#'@param cols - vector of column names to standardize (if x is a dataframe)
#'
#'@return standardized vector as z-scores (same size as x), or dataframe
#'
#'@details None.
#'
#'@export
#'
standardize<-function(x,cols=NULL){
  xp<-x;
  if (is.data.frame(xp)) {
      for (col in cols) xp[[col]]<-standardize.vector(xp[[col]]);
  } else {
      return(standardize.vector(xp));
  }
  return(xp);
}
#'
#'@title Standardize a vector by converting to z-scores.
#'
#'@description Function to standardize a vectorby converting to z-scores.
#'
#'@param x - vector to standardize
#'
#'@return standardized vector as z-scores (same size as x)
#'
#'@details If sd(x) is 0, then 0*x is returned. If sd(x) is NA, then NA+x is returned.
#'
#'not exported
#'
standardize.vector<-function(x){
    mn<-mean(x,na.rm=TRUE);
    sd<-sd(x,na.rm=TRUE);
    if(is.na(sd)){
        y<-x+NA;##all NA's
    } else if (sd==0){
        y<-0*x;##all 0's
    } else {
      y<-(x-mn)/sd;
  }
  return(y);
}