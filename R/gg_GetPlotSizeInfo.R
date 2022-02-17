#' 
#' @title Get info on grob sizes of a ggplot2 object or derived gtable
#' 
#' @description Function to get info on grob sizes  of a ggplot2 object or derived gtable.
#' 
#' @param p - ggplot or gtable object
#' @param unit - plot units ("in", "cm", etc.)
#' @param keepAll - flag (T/F) to keep all grobs (including grobs with physical size 0)
#' @param verbose - flag (0,1,2) to print intermediate information (0=off)
#' 
#' @return invisibly returns a list with elements
#' lyt - vector with plot width and height
#' tbl_rows - table with row sizes
#' sum_rows - table with summed height of rows
#' tbl_cols - table with column sizes
#' sum_cols - table with summed width of columns
#' 
#' @details This function identifies scales of components in a ggplot2 object. The
#' row/column sizes (and sums)  may involve null units reflecting expandable grobs.
#' 
#' @import dplyr
#' @import magrittr
#' @import grid
#' @import tibble
#' 
#' @author William Stockhausen (william.stockhausen@noaa.gov)
#' 
#' @export
#' 
gg_GetPlotSizeInfo<-function(p,
                             unit="in",
                             keepAll=FALSE,
                             verbose=0){
	if (inherits(p, "ggplot")){
	    if (verbose) cat("gg_ShowPlotGrobs: ggobj is a ggplot object\n");
		grob = ggplot2::ggplotGrob(p)
	} else if (inherits(p, "gtable")){
	    if (verbose) cat("gg_ShowPlotGrobs: ggobj is a gtable object\n");
		grob = p
	} else{
		stop("gg_ShowPlotGrobs: Don't know how to get sizes for object of class ",
			deparse(class(p)))
	}

  lyt = gg_GetPlotLayout(grob,keepAll=keepAll);  
  
  row_hts = as.numeric(grid::convertHeight(grob$heights,unitTo=unit,valueOnly=TRUE));
  col_wds = as.numeric(grid::convertHeight(grob$widths,unitTo=unit,valueOnly=TRUE));
  
  nrows   = length(row_hts);
  ncols   = length(col_wds);
  
  idx_rows=which(grid::unitType(grob$heights)=="null");
  tbl_rows = tibble::tibble(type="row",val=row_hts,unit=unit);
  null_rows = as.numeric(grob$heights[idx_rows]);
  tbl_rows$val[idx_rows]  = null_rows;
  tbl_rows$unit[idx_rows] = "null";
  tbl_rows %<>% dplyr::mutate(label = paste0(round(100*val)/100,unit));
  sum_rows = tbl_rows %>% 
               dplyr::group_by(type,unit) %>% 
               dplyr::summarize(val=sum(val)) %>%
               dplyr::ungroup() %>%
               dplyr::mutate(label = paste0(round(100*val)/100,unit)) %>%
               dplyr::group_by(type) %>%
               dplyr::summarize(label=paste(label,collapse=" + ")) %>%
               dplyr::ungroup() %>%
               dplyr::mutate(unit="sum",val=NA);
  
  idx_cols=which(grid::unitType(grob$widths) =="null");
  tbl_cols = tibble::tibble(type="col",val=col_wds,unit=unit);
  null_cols = as.numeric(grob$widths[idx_cols]);
  tbl_cols$val[idx_cols]  = null_cols;
  tbl_cols$unit[idx_cols] = "null";
  tbl_cols %<>% dplyr::mutate(label = paste0(round(100*val)/100,unit));
  sum_cols = tbl_cols %>% 
               dplyr::group_by(type,unit) %>% 
               dplyr::summarize(val=sum(val)) %>%
               dplyr::ungroup() %>%
               dplyr::mutate(label = paste0(round(100*val)/100,unit)) %>%
               dplyr::group_by(type) %>%
               dplyr::summarize(label=paste(label,collapse=" + ")) %>%
               dplyr::ungroup() %>%
               dplyr::mutate(unit="sum",val=NA);
  lst = list(layout=lyt,tbl_rows=tbl_rows,sum_rows=sum_rows,tbl_cols=tbl_cols,sum_cols=sum_cols);
  return(lst);
}

