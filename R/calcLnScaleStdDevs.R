#'
#'@title Compute ln-scale standard deviations from cv's
#'
#'@description Function to compute ln-scale standard deviations from cv's.
#'
#'@param cvs - vector of cv's
#'
#'@return vector of ln-scale standard deviations
#'
#'@details Conversion from `cv` to ln-scale standard deviation `lnSD` is
#'
#' lnSD = sqrt(log(1+cv^2))
#'
#'@export
#'
calcLnScaleStdDevs<-function(cvs){
    lnSDs = sqrt(log(1+cvs^2));
    return(lnSDs);
}
