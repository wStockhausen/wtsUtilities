#'
#' @title Save a ggplot2 plot to a file, specifying the size of one dimension
#' 
#' @description Function to save a ggplot2 plot to a file, 
#' specifying the size of one dimension.
#' 
#' @param filename - file name for output
#' @param plot - ggplot2 plot object to save (defaults to last plot)
#' @param fxdDim - value for the fixed dimension
#' @param fitWidth - flag indicating fixed dimension is width (default is TRUE)
#' @param units - character string with units for the fixed dimension
#' @param device - graphics device to print to
#' @param dpi - resolution (default=200 dpi)
#' 
#' @details Uses [gg_FixOnePlotDim] to determine the value of the missing plot dimension.
#' 
#' @importFrom ggplot2 ggsave
#' 
#' @export
#' 
gg_SavePlotWithOneFixedDim<-function(filename,
                                     plot=last_plot(),
                                     fxdDim,
                                     fitWidth=TRUE,
                                     units="in",
                                     device=NULL,
                                     dpi=200){
  dims = gg_FixOnePlotDim(plot,fxdDim,fitWidth,units);
  ggplot2::ggsave(filename=filename,plot=plot,device=device,
                  width=dims$width,height=dims$height,
                  units=units,dpi=dpi);
}