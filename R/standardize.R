#'
#'@
standardize<-function(x){
    mn<-mean(x,na.rm=TRUE);
    sd<-sd(x,na.rm=TRUE);
    if(sd>0){
        y<-(x-mn)/sd;
    } else {
        y<-0*x;
    }
    return(y)
}