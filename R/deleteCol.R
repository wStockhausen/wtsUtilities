#' @title Delete column in a dataframe or matrix
#' 
#' @param tbl - dataframe or matrix from which to delete column
#' @param col - name of column to delete
#' 
#' @return dataframe or matrix with column removed
#' 
#'@export
#'
deleteCol<-function(tbl,col){
    tblp<-tbl[,(names(tbl)!=col)];
    return(tblp);
}