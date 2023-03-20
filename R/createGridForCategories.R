#'
#' @title Create a \pkg{tibble} tibble describing a rectangular grid defined by two sets of categories
#'
#' @description  Function to create a \pkg{tibble} tibble describing a rectangular grid defined by two sets of categories.
#' 
#' @param xcats - vector of categories for x-axis 
#' @param ycats - vector of categories for y-axis
#'       
#'@return a \code{\link[tibble]{tibble}}. See Details.
#'
#'@details The returned tibble will have the following columns: 
#'\itemize{
#'  \item{xc - x-axis category labels (factor levels)}
#'  \item{yc - y-axis category labels (factor levels)}
#'  \item{id - id (character) for rectangle with center xc,yc [="(xc,yc)"]}
#'  \item{x - rectangle corner location (numeric) along x-axis}
#'  \item{y - rectangle corner location (numeric) along y-axis}
#'}
#'
#'@importFrom tibble tibble
#'
#'@export
#'
createGridForCategories<-function(xcats,ycats){
    if (!is.factor(xcats)) xcats = as.factor(xcats);
    if (!is.factor(ycats)) ycats = as.factor(ycats);
    nx = length(xcats); dx = 1;
    ny = length(ycats); dy = 1;
    rects = NULL;
    for (xc in 1:nx){
        for (yc in 1:ny){
            x = xc+(dx/2)*c(-1,1,1,-1);
            y = yc+(dy/2)*c(-1,-1,1,1);
            rect = tibble::tibble(xc=xcats[xc],yc=ycats[yc],id=paste0("(",xcats[xc],",",ycats[yc],")"),x=x,y=y);
            rects = rbind(rects,rect); rm(rect);
        }
    }
    return(rects)
}

