#' @title Returns count, mean, stdev, and cv of vector x
#' 
#' @description Returns count, mean, stdev, and cv of vector x
#' 
#' @param x - a vector of numbers (or object coerceable to a vector)
#' 
#' @return - vector with count, mean, stdev and cv as named elements
#'
#'@export
#'
computeStats.vector<-function(x) {
    #coerce x to a vector, if necessary
    if (!is.vector(x)) x<-as.vector(x);
    
    #compute stats
    n<-count(x);
    mn<-mean(x,na.rm=TRUE);
    sd<-sd(x,na.rm=TRUE);
    cv<-sd/mn;
    
    #form output vector
    res<-c(n,mn,sd,cv);
    names(res)<-c("n","mean","stdev","cv");
    return(res)
}
