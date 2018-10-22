#'
#' @title Calculate percent non-zero occurrences from a set of dataframe columns, possibly
#'by factor levels
#'
#' @description Function to calculate percent non-zero occurrences from a set of dataframe columns, possibly
#'by factor levels.
#'
#' @param dfr - dataframe with data in columns
#' @param vars - vector of columns (names) on which to calculate percentages 
#' @param factors - vector of columns to use as factors
#'
#' @return a dataframe
#'
#' @details
#'
#' @export
#'
#source(file.path("../Utilities/sumBy.R"),chdir=TRUE);
calcPercentNonzero<-function(dfr,
                             vars=NULL,
                             factors=NULL){
  #convert non-zero values to 1's
  for (var in vars){
    dfr[[var]]<-dfr[[var]]!=0;
  }  
  dfr1<-sumBy(dfr,factors=factors,vars=vars);#numerator
  
  #convert all values to 1's
  for (var in vars){
    dfr[[var]]<-1;
  }  
  dfr2<-sumBy(dfr,factors=factors,vars=vars);#denominator
  
  dfr.r<-dfr1;
  for (var in vars){
    dfr.r[[var]]<-dfr1[[var]]/dfr2[[var]];
  }
  
  return(dfr.r)
}

#dfr.r<-calcPercentNonzero(dfrp,vars='values',factors='ind')