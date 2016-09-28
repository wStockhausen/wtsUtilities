#' 
#'@title Get interval limits (cut points) from a vector of midpoints
#' 
#'@description Function to return a vector of interval limits from a vector of midpoints.
#'  
#'@param xs - the vector of interval midpoints
#'
#'@return the vector of interval limits (cutpoints)
#'
#'@details Assumes interval limits are midway between coordinates in xs. First
#'and last coordinates assume same widths as first and last cells. Resulting vector
#'has nx+1 values, if xs has nx values.
#'
#'@export
#'
getIntervalLimits<-function(xs){
    nx<-length(xs);
    xp<-vector(mode='numeric',length=nx+1);#extend beyond grid to left
    xp[1]<-xs[1]-0.5*(xs[2]-xs[1]);
    for (x in 2:nx){
        xp[x]<-0.5*c(xs[x-1]+xs[x]);
    }
    xp[nx+1]<-xs[nx]+0.5*(xs[nx]-xs[nx-1]);#extend beyond grid to right
    return(invisible(xp));
}