#'
#' @title Determine bins by applying a vector of cutpoints
#'
#' @description Function to determine bins by applying a vector of cutpoints.
#'
#' @param zs - the vector to apply cutpoints to
#' @param cutpts - vector of cutpoints
#' @param truncate.low - flag to truncate below first cutpoint (i.e., cutpt[1]<-0)
#' @param truncate.high - flag to truncate above last cutpoint (i.e., cutpt[last]<-Inf)
#'
#' @return a vector with values aligned to the left cutpoints
#'
#' @details None.
#'
#' @export
#'
applyCutPts<-function(zs,
                      cutpts=seq(from=25,to=185,by=5),
                      truncate.low=TRUE,
                      truncate.high=FALSE){
    #expand cutpts to truncate or not
    nCtPts<-length(cutpts);
    ctpts.tmp<-cutpts;
    if (!truncate.low ) ctpts.tmp[1]<-0;
    if (!truncate.high) ctpts.tmp[nCtPts]<-Inf;
    #apply cutpts to sizes
    cuts<-cut(zs,ctpts.tmp,right=FALSE,labels=FALSE)
    return(cutpts[cuts]);
}
