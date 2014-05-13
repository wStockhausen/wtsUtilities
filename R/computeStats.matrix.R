#' @title computeStats.matrix: Calculate count, mean, stdev, and cv of the rows/columns of a matrix
#' @description Returns count, mean, stdev, and cv of the rows or columns of a matrix
#' 
#' @param x - matrix
#' @param byCol - compute stats by column (if TRUE)
#' 
#' @return 
#' - for x a matrix, returns matrix w/ rows with elements count, mean, stdev and cv.
#'   Row names correspond to column (or row) names of original matrix.
#'
#'@export
#'
computeStats.matrix<-function(x,byCol=TRUE) {
    if (byCol) x<-t(x);#transpose matrix to process by rows
    nr<-nrow(x);
    res<-matrix(nrow=nr,ncol=4);
    colnames(res)<-c("n","mean","stdev","cv");
    rownames(res)<-rownames(x);
    
    for (r in 1:nr){
        res.rw<-computeStats.vector(x[r,])
        res[r,]<-res.rw;
    }
    return(res)
}
