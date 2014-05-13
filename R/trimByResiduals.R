#' @title Trim rows from a dataframe based on a vector of residuals.
#' 
#' @description Trims rows from a dataframe based on a corresponding vector of "residuals".
#' 
#' @param tbl    - dataframe with original data. row names should be 1:nrows BEFORE residuals are calculated
#' @param resids - vector of residuals to fit
#' @param n      - number of rows to drop
#' @param debug  - flag to print diagnostics
#' 
#' @return the trimmed dataframe
#'
#'@export
#'
trimByResiduals<-function(tbl,resids,n=5,debug=TRUE){
    res<-sort(abs(resids),decreasing=TRUE,index.return=TRUE);
    idxLrg<-res$ix[1:n];                  #indices to largest residuals
    idxTrm<-res$ix[(n+1):length(res$ix)]; #indices to remaining rows
    trmLrg<-data.frame(cbind(tbl[idxLrg,],resids[idxLrg]));   #dataframe of dropped data
    trmTbl<-tbl[idxTrm,]; #trimmed dataframe
    if (debug) {
        cat('\nOriginal number of rows = ',nrow(tbl),'Number of rows trimmed = ',n,'. Number kept = ',nrow(trmTbl),'.\n',sep='');
        print(trmLrg);
        cat('\nOriginal number of rows = ',nrow(tbl),'Number of rows trimmed = ',n,'. Number kept = ',nrow(trmTbl),'.\n',sep='');
    }
    return(trmTbl);
}