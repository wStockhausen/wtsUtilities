#'
#' @title "Boxify" x,y vectors for a "boxy" xy plot
#' 
#' @description Function to "boxify" x,y vectors for a "boxy" xy plot.
#' 
#' @param x - vector to expand
#' @param y - vector to expand
#' @param dx - increment along x-axis 
#' 
#' @return dataframe with expanded elements x,y
#' 
#' @details This function expands each x,y pair to x+c(-1,0,0,1)*dx/2 and
#' y*c(0,1,1,0), which provides a "box". 
#' 
#' @export
#' 
boxify<-function(x,y,dx=NULL){
    if (is.null(dx)) dx <- x[2]-x[1];
    nx<-length(x);
    dip<-c(-3,-2,-1,0);
    dxp<-c(-1,-1,1,1)*dx/2;
    dyp<-c(0,1,1,0);
    xp<-0*rep_len(x,length.out=4*nx);
    yp<-0*rep_len(x,length.out=4*nx);
    for (i in 1:nx){
        xp[4*i+dip]<-x[i]+dxp;
        yp[4*i+dip]<-y[i]*dyp;
    }
    return(data.frame(x=xp,y=yp))
}

