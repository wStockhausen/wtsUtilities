#' 
#' @title Get the layout dataframe from a ggplot2 or gtable object
#' 
#' @description Function to get the layout dataframe from a ggplot2 or gtable object.
#' 
#' @param p - ggplot2 or gtable object
#' @param keepAll - flag (T/F) to keep all grobs (including grobs with physical size 0)
#' @param verbose - flag (T/F) to print intermediate information
#' 
#' @return returns a dataframe with columns reflecting the row/column locations and extents
#' of the input ggplot2 or gtable object.
#' 
#' @details This function adds information to a layout regarding how grobs
#' are assigned to rows and columns in a ggplot2 (gtable) layout. It adds coordinates to
#' allow plotting grobs as rectangles by row/column coordinates.
#' 
#' @import dplyr
#' @import magrittr
#' 
#' @author William Stockhausen (william.stockhausen@noaa.gov)
#' 
#' @export
#' 
gg_GetPlotLayout<-function(p,keepAll=TRUE,verbose=0){
	if(inherits(p, "ggplot")){
	    if (verbose) cat("gg_GetPlotLayout: object is a ggplot object\n");
		grob = ggplot2::ggplotGrob(p)
	}else if (inherits(p, "gtable")){
	    if (verbose) cat("gg_GetPlotLayout: object is a gtable object\n");
		grob = p
	}else{
		stop("gg_GetPlotLayout: Don't know how to get sizes for object of class ",
			deparse(class(p)))
	}
  ncols = length(grob$widths);
  nrows = length(grob$heights);
  non0Grbs = which(sapply(grob$grobs,function(x){return(class(x)[1]);},simplify=TRUE)!="zeroGrob");
  lyt   = grob$layout %>% 
            dplyr::mutate(grob=row_number(),
                          id  = paste(grob,name),
                          id=factor(id,levels=id)) %>%
            dplyr::arrange(z) %>%
            dplyr::mutate(xmin = l-0.5,
                          xmax = r+0.5,
                          x    = (xmax+xmin)/2,
                          ymin = nrows-b+0.5,
                          ymax = nrows-t+1.5,
                          y    = (ymax+ymin)/2);
  if (!keepAll) lyt %<>% dplyr::filter(grob %in% non0Grbs);
  return(lyt);
}
