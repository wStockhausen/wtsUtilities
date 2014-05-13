#' @title computeStats: Calculate count, mean, stdev, and cv of a vector or rows/columns of a matrix, or dataframe
#' @description Returns count, mean, stdev, and cv of vector, matrix or dataframe
#' 
#' @param x - vector, matrix or dataframe
#' @param byCol - compute stats by column (if TRUE)
#' @param cols.factors - vector identifying columns to use as factors
#' @param cols.vars - vector identifying columns to use as variables
#' 
#' @return 
#' - for x a vector, returns vector with count,mean, stdev and cv as elements \cr
#' - for x a matrix, returns matrix w/ rows with elements count, mean, stdev and cv\cr
#' Rownames correspond to column (or row) names of original matrix.
#' - for x a dataframe, returns NULL (not yet implemented)
#'
#'@export
#'
computeStats<-function(x,byCol=TRUE,cols.factors=1,cols.vars=2) {
    if(is.vector(x)) {return(computeStats.vector(x))} else 
    if(is.matrix(x)) {return(computeStats.matrix(x,byCol=byCol))} else 
    if(is.data.frame(x)){return(computeStats.dataframe(x,cols.factors=cols.factors))}
    
    return(NULL);
}
