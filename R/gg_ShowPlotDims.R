#' 
#' @title Determine and show dimensions of a ggplot2 object or derived gtable
#' 
#' @description Function to determine and show dimensions of a ggplot2 object or derived gtable.
#' 
#' @param p - ggplot2 or derived gtable object
#' @param unit - plot units ("in", "cm", etc.)
#' @param null - size of (x and y) null (resizeable) units (in plot units; overridden if null_x and null_y are given)
#' @param null_x - size of horizontal null (resizeable) units (in plot units; must also specify null_y)
#' @param null_y - size of vertical (resizeable) units (in plot units; must also specify null_x)
#' @param xtick - x-axis tick separation
#' @param ytick - y-axis tick separation
#' @param showPlot - flag (T/F) to print plots
#' @param verbose - flag (0,1,2) to print intermediate information (0=off)
#' 
#' @return invisibly returns a list with elements
#' plot - plot of object components with axes reflecting inferred sizes
#' dims - vector with plot width and height
#' lyt - layout table (if verbose>1)
#' tbl_rows - row dimensions (if verbose>1)
#' tbl_cols - column dimensions (if verbose>1)
#' tbl_matp -tibble with intermediate information (if verbose>1)
#' tbl_matpp - tibble with intermediate information (if verbose>1)
#' 
#' @details This function converts "null" (relative) scales in a ggplot2 object 
#' (or derived gtable) to absolute scales
#' and creates a rectangle plot of the resulting components with axes reflecting
#' the absolute scales.
#' 
#' If either null_x or null_y is NULL, then the value of \code{null} is used for both. If
#' both null_x and null_y are not NULL, these values are used for nulls along the correpsonding
#' axes.
#' 
#' @import dplyr
#' @import magrittr
#' @import ggplot2 
#' 
#' @author William Stockhausen (william.stockhausen@noaa.gov)
#' 
#' @export
#' 
gg_ShowPlotDims<-function(p,
                          unit="in",
                          null=3,
                          null_x=NULL,
                          null_y=NULL,
                          xtick=0.5,
                          ytick=xtick,
                          showPlot=TRUE,
                          verbose=0){
  if (is.null(null_x)|is.null(null_y)) {null_x=null_y=null;}
	if(inherits(p, "ggplot")){
	    if (verbose) cat("gg_ShowPlotDims: p is a ggplot object\n");
		grob = ggplot2::ggplotGrob(p)
	}else if (inherits(p, "gtable")){
	    if (verbose) cat("gg_ShowPlotDims: p is a gtable object\n");
		grob = p
	}else{
		stop("gg_ShowPlotDims: Don't know how to get sizes for object of class ",
			deparse(class(p)))
	}
  lst = gg_GetPlotDims(p,unit=unit,null=null,null_x=null_x,null_y=null_y,returnAll=TRUE,verbose=verbose);
  
  # lst = gg_ShowPlotGrobs(p,unit=unit,showAll=FALSE,showPlot=FALSE,verbose=FALSE);
  # lyt = lst$layout;
  # tbl_rows = lst$tbl_rows;
  # tbl_cols = lst$tbl_cols;
  # 
  # tbl_rowsp = tbl_rows %>% 
  #   dplyr::mutate(val=ifelse(unit=="null",null_y*val,val),
  #                 row=dplyr::row_number()) %>%
  #   dplyr::arrange(dplyr::desc(row)) %>%
  #   dplyr::mutate(cum_val=cumsum(val))
  # tbl_rowsp;  
  # 
  # tbl_colsp = tbl_cols %>% 
  #   dplyr::mutate(val=ifelse(unit=="null",null_x*val,val),
  #                 col=dplyr::row_number()) %>%
  #   dplyr::arrange(col) %>%
  #   dplyr::mutate(cum_val=cumsum(val))
  # tbl_colsp;  
  # 
  # tbl_mat = dplyr::full_join(tbl_rowsp %>% dplyr::select(!c(type,unit,x)),
  #                            tbl_colsp %>% dplyr::select(!c(type,unit,y)),
  #                            by=character(),suffix=c("_row","_col"));
  # 
  # tbl_matp = tbl_mat %>% dplyr::filter((val_row!=0)&(val_col!=0)) %>%
  #              dplyr::select(x,y,col,row,val_col,val_row,cum_val_col,cum_val_row,label_col,label_row) %>%
  #              dplyr::mutate(xmin=cum_val_col-val_col,
  #                            xmax=cum_val_col,
  #                            ymin=cum_val_row-val_row,
  #                            ymax=cum_val_row,
  #                            xl=cum_val_col-val_col/2,
  #                            yl=cum_val_row-val_row/2,
  #                            label=paste0(col,",",row));
  # if (verbose>1) View(tbl_matp);
  # 
  # xmax = max(tbl_matp$xmax);
  # ymax = max(tbl_matp$ymax);
  # if (verbose) cat("gg_ShowPlotDims: width =",xmax,"height =",ymax,"units:",unit,"\n")
  
  # tbl_matpp = tbl_matp %>% dplyr::mutate(grob=0);
  # for (r in 1:nrow(lyt)){
  #   #--testing: r=1;
  #   rw = lyt[r,];
  #   grb = rw$grob
  #   top = rw$t;
  #   lft = rw$l;
  #   btm = rw$b;
  #   rgt = rw$r;
  #   nam = rw$name;
  #   for (i in lft:rgt){
  #     for (j in btm:top){
  #       #--testing: i=1; j=1;
  #       idx = (tbl_matpp$col==i)&(tbl_matpp$row==j);
  #       tbl_matpp$grob[idx] = grb;
  #     }#--j
  #   }#--i
  # }#--r
  # tbl_matpp$grob = factor(tbl_matpp$grob,levels=as.character(lyt$grob),labels=lyt$id)
  # if (verbose>1) View(tbl_matpp);
  
  xmax = lst$dims["width"];
  ymax = lst$dims["height"];
  p1 = ggplot(lst$tbl_matp,aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,x=xl,y=yl,label=label)) +
        geom_rect(fill="light grey",colour="black")+
        geom_text()+
        coord_fixed(expand=FALSE)+
        scale_x_continuous(breaks=seq(0,xmax,xtick)) +
        scale_y_continuous(breaks=seq(0,ymax,ytick)) +
        labs(y=paste0("dimensions in ",unit)) +
        theme(axis.title.x=element_blank());
  if (showPlot&(verbose>1)) print(p1);
  
  p2 = p1 + 
         geom_rect(data=lst$tbl_matpp,
                   mapping=aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=grob,colour=grob));
  if (showPlot) print(p2);

  lst1 = list(plot=p2,dims=lst$dims);
  if (verbose>1) {
    lst1$lyt       = lst$lyt;
    lst1$tbl_rows  = lst$tbl_rows;
    lst1$tbl_cols  = lst$tbl_cols;
    lst1$tbl_matp  = lst$tbl_matp;
    lst1$tbl_matpp = lst$tbl_matpp;
  }
  return(invisible(lst1));
}

