#' 
#' @title Show component "grobs" of a ggplot2 object (and get info on grob sizes)
#' 
#' @description Function to show component "grobs" of a ggplot2 object (and get info on grob sizes).
#' 
#' @param p - ggplot or gtable object
#' @param unit - plot units ("in", "cm", etc.)
#' @param xOffset - x-axis offset for row size labels
#' @param yOffset - y-axis offset for column size labels
#' @param showAll - flag (T/F) to show all grobs (including grobs with physical size 0)
#' @param showPlot - flag (T/F) to print grob plot
#' @param verbose - flag (0,1,2) to print intermediate information (0=off)
#' 
#' @return invisibly returns a list with elements
#' plot - plot of object components with axes reflecting inferred sizes
#' lyt - vector with plot width and height
#' tbl_rows - 
#' sum_rows - 
#' tbl_cols - 
#' sum_cols - 
#' 
#' @details This function identifies scales of components in a ggplot2 object 
#' and creates an unfilled rectangle plot of the resulting components with axes reflecting
#' the rows and columns in the gtable derived from the ggplot2 object.
#' 
#' See also [gg_ShowPlotDims], which is similar but provides absolute scales.
#' 
#' @import dplyr
#' @import magrittr
#' @import ggplot2 
#' @import grid
#' @import tibble
#' 
#' @author William Stockhausen (william.stockhausen@noaa.gov)
#' 
#' @export
#' 
gg_ShowPlotGrobs<-function(p,
                           unit="in",
                           xOffset=-3,
                           yOffset=-3,
                           showAll=FALSE,
                           showPlot=TRUE,
                           verbose=0){
  lst = gg_GetPlotSizeInfo(p,keepAll=showAll);
  lyt = lst$layout;
  tbl_rows = lst$tbl_rows;
  sum_rows = lst$sum_rows;
  tbl_cols = lst$tbl_cols;
  sum_cols = lst$sum_cols;

  nrows   = nrow(tbl_rows);
  ncols   = nrow(tbl_cols);
  
  tbl_rows %<>% dplyr::mutate(x=xOffset,
                              y=nrows-dplyr::row_number()+1);
  sum_rows %<>% dplyr::mutate(x = ncols+1,
                              y = nrows/2);
  tbl_cols %<>% dplyr::mutate(x=dplyr::row_number(),
                             y=yOffset);
  sum_cols %<>% dplyr::mutate(x = ncols/2,
                              y = nrows+1);


  # lyt = gg_GetPlotLayout(grob,showAll=showAll);  
  # 
  # row_hts = as.numeric(grid::convertHeight(grob$heights,unitTo=unit,valueOnly=TRUE));
  # col_wds = as.numeric(grid::convertHeight(grob$widths,unitTo=unit,valueOnly=TRUE));
  # 
  # nrows   = length(row_hts);
  # ncols   = length(col_wds);
  # 
  # idx_rows=which(grid::unitType(grob$heights)=="null");
  # tbl_rows = tibble::tibble(type="row",val=row_hts,unit=unit) %>%
  #              dplyr::mutate(x=xOffset,
  #                            y=nrows-dplyr::row_number()+1);
  # null_rows = as.numeric(grob$heights[idx_rows]);
  # tbl_rows$val[idx_rows]  = null_rows;
  # tbl_rows$unit[idx_rows] = "null";
  # tbl_rows %<>% dplyr::mutate(label = paste0(round(100*val)/100,unit));
  # sum_rows = tbl_rows %>% 
  #              dplyr::group_by(type,unit) %>% 
  #              dplyr::summarize(val=sum(val)) %>%
  #              dplyr::ungroup() %>%
  #              dplyr::mutate(x = ncols+1,
  #                            y = nrows/2,
  #                            label = paste0(round(100*val)/100,unit)) %>%
  #              dplyr::group_by(type,x,y) %>%
  #              dplyr::summarize(label=paste(label,collapse=" + ")) %>%
  #              dplyr::ungroup() %>%
  #              dplyr::mutate(unit="sum",val=NA);
  # 
  # idx_cols=which(grid::unitType(grob$widths) =="null");
  # tbl_cols = tibble::tibble(type="col",val=col_wds,unit=unit) %>%
  #              dplyr::mutate(x=dplyr::row_number(),
  #                            y=yOffset);
  # null_cols = as.numeric(grob$widths[idx_cols]);
  # tbl_cols$val[idx_cols]  = null_cols;
  # tbl_cols$unit[idx_cols] = "null";
  # tbl_cols %<>% dplyr::mutate(label = paste0(round(100*val)/100,unit));
  # sum_cols = tbl_cols %>% 
  #              dplyr::group_by(type,unit) %>% 
  #              dplyr::summarize(val=sum(val)) %>%
  #              dplyr::ungroup() %>%
  #              dplyr::mutate(x = ncols/2,
  #                            y = nrows+1,
  #                            label = paste0(round(100*val)/100,unit)) %>%
  #              dplyr::group_by(type,x,y) %>%
  #              dplyr::summarize(label=paste(label,collapse=" + ")) %>%
  #              dplyr::ungroup() %>%
  #              dplyr::mutate(unit="sum",val=NA);
  
  p1 = ggplot(lyt)+
        geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,colour=id),
                  fill=NA,size=2) +
        geom_label(aes(label=grob,x=x,y=y,colour=id),show.legend=FALSE)+
        scale_x_continuous(breaks=1:ncols) +
        scale_y_continuous(breaks=1:nrows) +
        scale_colour_viridis_d() +
        theme(panel.ontop=TRUE,
              panel.grid.major=element_line(colour="light grey",linetype=2,size=0.5),
              panel.grid.minor=element_blank(),
              panel.background=element_rect(fill=NA,colour="black"),
              axis.title=element_blank()) +
       geom_text(data=tbl_rows,mapping=aes(x=x,y=y,label=label),hjust="left") +
       geom_text(data=sum_rows,mapping=aes(x=x,y=y,label=label),angle=270) +
       geom_text(data=tbl_cols,mapping=aes(x=x,y=y,label=label),hjust="left",angle=90) +
       geom_text(data=sum_cols,mapping=aes(x=x,y=y,label=label));
  if (showPlot) print(p1);
  lst = list(plot=p1,layout=lyt,tbl_rows=tbl_rows,sum_rows=sum_rows,tbl_cols=tbl_cols,sum_cols=sum_cols);
  return(invisible(lst));
}

