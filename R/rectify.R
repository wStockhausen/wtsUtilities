#'
#' @title "Rectify" x,y vectors for a ggplot2::geom_rect xy plot
#' 
#' @description Function to "rectxify" x,y vectors for a ggplot2::geom_rect xy plot.
#' 
#' @param x - vector to expand
#' @param y - vector to expand
#' @param dx - increment along x-axis 
#' 
#' @return dataframe with expanded elements xmin,xmax,ymin,ymax
#' 
#' @details None 
#' 
#' @export
#' 
rectify<-function(x,y,dx=NULL){
    if (is.null(dx)) dx<-x[2]-x[1];
    dfr<-data.frame(x=x,y=y,xmin=x-dx/2,xmax=x+dx/2,ymin=0,ymax=y);
    return(dfr)
}

