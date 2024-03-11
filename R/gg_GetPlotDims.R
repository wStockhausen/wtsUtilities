#' 
#' @title Get dimensions of a ggplot2 object or derived gtable
#' 
#' @description Function to get dimensions of a ggplot2 object or derived gtable.
#' 
#' @param p - ggplot2 or derived gtable object
#' @param unit - plot units ("in", "cm", etc.)
#' @param null - size of (x and y) null (resizeable) units (in plot units; overridden if null_x and null_y are given)
#' @param null_x - size of horizontal null (resizeable) units (in plot units; must also specify null_y)
#' @param null_y - size of vertical (resizeable) units (in plot units; must also specify null_x)
#' @param returnAll - flag to return all information (not just dimensions)
#' @param verbose - flag (0,1,2) to print intermediate information (0=off)
#' 
#' @return if returnAll is FALSE, returns a vector with plot width and height. Otherwise, it
#' invisibly returns a list with elements
#' dims - vector with plot width and height
#' lyt - layout table
#' tbl_rows - row dimensions
#' tbl_cols - column dimensions
#' tbl_matp -tibble with intermediate information
#' tbl_matpp - tibble with intermediate information
#' 
#' @details This function converts "null" (relative) scales in a ggplot2 object 
#' (or derived gtable) to absolute scales.
#' 
#' If either null_x or null_y is NULL, then the value of \code{null} is used for both. If
#' both null_x and null_y are not NULL, these values are used for nulls along the corresponding
#' axes.
#' 
#' @note This does not work (yet) for composite figures consisting of multiple ggplot2 objects.
#' 
#' @import dplyr
#' @import magrittr
#' @import ggplot2 
#' @importFrom utils View
#' 
#' @author William Stockhausen (william.stockhausen@noaa.gov)
#' 
#' @export
#' 
gg_GetPlotDims<-function(p,
                          unit="in",
                          null=3,
                          null_x=NULL,
                          null_y=NULL,
                          returnAll=FALSE,
                          verbose=0){
  if (verbose) cat("--starting gg_GetPlotDims\n");
  if (is.null(null_x)|is.null(null_y)) {null_x=null_y=null;}
	if(inherits(p, "ggplot")){
	    if (verbose) cat("gg_GetPlotDims: p is a ggplot object\n");
		grob = ggplot2::ggplotGrob(p)
	}else if (inherits(p, "gtable")){
	    if (verbose) cat("gg_GetPlotDims: p is a gtable object\n");
		grob = p
	}else{
		stop("gg_GetPlotDims: Don't know how to get sizes for object of class ",
			deparse(class(p)))
	}
  lst = gg_GetPlotSizeInfo(p,unit=unit,keepAll=FALSE,verbose=FALSE);
  lyt = lst$layout;
  tbl_rows = lst$tbl_rows;
  tbl_cols = lst$tbl_cols;
  
  tbl_rowsp = tbl_rows %>% 
    dplyr::mutate(val=ifelse(unit=="null",null_y*val,val),
                  row=dplyr::row_number()) %>%
    dplyr::arrange(dplyr::desc(row)) %>%
    dplyr::mutate(cum_val=cumsum(val))
  if (verbose) {cat("gg_GetPlotDims: tbl_rowsp:\n");  print(tbl_rowsp);}
  
  tbl_colsp = tbl_cols %>% 
    dplyr::mutate(val=ifelse(unit=="null",null_x*val,val),
                  col=dplyr::row_number()) %>%
    dplyr::arrange(col) %>%
    dplyr::mutate(cum_val=cumsum(val))
  if (verbose) {cat("gg_GetPlotDims: tbl_colsp:\n"); print(tbl_colsp);}
  
  tbl_mat = dplyr::full_join(tbl_rowsp %>% dplyr::select(!c(type,unit)),
                             tbl_colsp %>% dplyr::select(!c(type,unit)),
                             by=character(),suffix=c("_row","_col"));
  
  tbl_matp = tbl_mat %>% dplyr::filter((val_row!=0)&(val_col!=0)) %>%
               dplyr::select(col,row,val_col,val_row,cum_val_col,cum_val_row,label_col,label_row) %>%
               dplyr::mutate(xmin=cum_val_col-val_col,
                             xmax=cum_val_col,
                             ymin=cum_val_row-val_row,
                             ymax=cum_val_row,
                             xl=cum_val_col-val_col/2,
                             yl=cum_val_row-val_row/2,
                             label=paste0(col,",",row));
  if (verbose>1) View(tbl_matp);
  
  xmax = max(tbl_matp$xmax);
  ymax = max(tbl_matp$ymax);
  if (verbose) cat("gg_GetPlotDims: width =",xmax,"height =",ymax,"units:",unit,"\n")

  tbl_matpp = tbl_matp %>% dplyr::mutate(grob=0);
  for (r in 1:nrow(lyt)){
    #--testing: r=1;
    rw = lyt[r,];
    grb = rw$grob
    top = rw$t;
    lft = rw$l;
    btm = rw$b;
    rgt = rw$r;
    nam = rw$name;
    for (i in lft:rgt){
      for (j in btm:top){
        #--testing: i=1; j=1;
        idx = (tbl_matpp$col==i)&(tbl_matpp$row==j);
        tbl_matpp$grob[idx] = grb;
      }#--j
    }#--i
  }#--r
  tbl_matpp$grob = factor(tbl_matpp$grob,levels=as.character(lyt$grob),labels=lyt$id)
  if (verbose>1) print(tbl_matpp);
  
  dims=c(width=xmax,height=ymax);
  if (returnAll) {
    lst = list(dims=dims,
               lyt=lyt,
               tbl_rows = tbl_rows,
               tbl_cols = tbl_cols,
               tbl_matp=tbl_matp,
               tbl_matpp=tbl_matpp);
    if (verbose) cat("--finished gg_GetPlotDims\n");
    return(invisible(lst));
  }
  if (verbose) cat("--finished gg_GetPlotDims\n");
  return(dims);
}

