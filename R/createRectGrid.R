#'
#' @title Create a \pkg{tibble} tibble describing a rectangular grid
#'
#' @description  Function to create a \pkg{tibble} tibble describing a rectangular grid.
#' 
#' @param xctrs - vector of x-axis locations of grid cell centers 
#' @param yctrs - vector of y-axis locations of grid cell centers 
#' @param dx - rectangle width (length along x-axis)
#' @param dy - rectangle height (length along y-axis)
#'       
#'@return a \code{\link[tibble]{tibble}}. See Details.
#'
#'@details The returned tibble will have the following columns: 
#'\itemize{
#'  \item{xc - rectangle center location along x-axis}
#'  \item{yc - rectangle center location along y-axis}
#'  \item{id - id (character) for rectangle with center xc,yc [="(xc,yc)"]}
#'  \item{x - rectangle corner location along x-axis}
#'  \item{y - rectangle corner location along y-axis}
#'}
#'
#'@importFrom tibble tibble
#'
#'@export
#'
createRectGrid<-function(xctrs,yctrs,dx=1,dy=1){
    rects = NULL;
    for (xc in xctrs){
        for (yc in yctrs){
            y = xc+(dx/2)*c(-1,1,1,-1);
            x = yc+(dy/2)*c(-1,-1,1,1);
            rect = tibble::tibble(xc=xc,yc=yc,id=paste0("(",xc,",",yc,")"),x=x,y=y);
            rects = rbind(rects,rect); rm(rect);
        }
    }
    return(rects)
}

