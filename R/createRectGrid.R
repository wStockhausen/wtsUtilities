#'
#' @title Add cumulative sums (standard and normalized) of a column to a dataframe by an index and set of factors
#'
#' @description  Function to add cumulative sums (standard and normalized) of a column to a 
#' dataframe by an index column and set of factor columns.
#' 
#' @param xctrs - vector of x-axis locations of grid cell centers 
#' @param yctrs - vector of y-axis locations of grid cell centers 
#' @param dx - rectangle width (length along x-axis)
#' @param dy - rectangle height (length along y-axis)
#'       
#'@return a \code{\link[tibble]{tibble}}. See @details
#'
#'@details The returned tibble will have the following columns: 
#'\itemize{
#'  \item{xc - cell center location along x-axis}
#'  \item{yc - cell center location along y-axis}
#'  \item{id - id (character) for rectangle with center xc,yc [="(xc,yc)"]}
#'  \item{x - cell corner location along x-axis}
#'  \item{y - cell corner location along y-axis}
#'}
#'
#'@importFrom tibble tibble
#'
#'@export
#'
createRectGrid<-function(xctrs,yctrs,dx=0.5,dy=0.5){
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

