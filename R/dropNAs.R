#' @title Removes all rows from a dataframe based on columns that contain NAs.
#' 
#' @description  Removes all rows from a dataframe where the value in any column specified in "factors"
#'       contains an NA.
#' 
#' @param df        - input dataframe (or object that can be cast to a dataframe)
#' @param factors   - vector of desired column names from df to sort by
#' 
#' @return dataframe without NAs in factor columns
#'
#'@export
#'
dropNAs<-function(df,factors) {
    #coerce df to dataframe, if necessary
    if (!is.data.frame(df)) df<-as.data.frame(df);

    #get subset of df limited to factor columns
    z<-df[,factors];
    
    #find indices of each row without an NA in it
    ans <- sapply(z, is.na)
    if (is.matrix(ans)) {
        ok<-!apply(ans, 1, any);
    } else {
        ok<-!any(ans);
    }
    
    #get the subset of rows without NAs in factors
    dfp<-df[ok,];
    
    return(dfp);
 }