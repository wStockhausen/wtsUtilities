#'
#' @title Convert a character vector to numeric values
#' 
#' @description Function to convert a character vector to numeric values.
#' 
#' @param x - vector of values on which to perform conversion
#' 
#' @return a vector of the same length as \code{x} converted to numeric
#' 
#' @details Values with commas are converted after stripping out commas. 
#' Values with percent signs are converted after removing percent signs and 
#' converting to fractions (i.e., dividing by 100). Otherwise, this works as \code{as.numeric(v)} would.
#' 
#' @export
#' 
convertToNumeric<-function(x){
    xp<-x;
    idx1<-grep(",",xp,fixed=TRUE);
    xp[idx1]<-gsub(",","",xp[idx1],fixed=TRUE);
    idx2<-grep("%",xp,fixed=TRUE);
    xp[idx2]<-gsub("%","",xp[idx2],fixed=TRUE);
    v<-as.numeric(xp);
    v[idx2]<-v[idx2]/100.;#convert %-ages to fractions
    return(v);
}
