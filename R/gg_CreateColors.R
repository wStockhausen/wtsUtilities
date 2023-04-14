#' 
#' @title Create a vector of colors based on a color scale
#' 
#' @description Function to create a vector of colors based on a color scale.
#' 
#' @param n - number of colours to create 
#' @param scale - name of scale to use (see Details)
#' @param ... - passed on to the scale 
#' @param show - show colours using [scales::show_col()]
#' 
#' @return a vector of colors.
#' 
#' @details
#' Available color scales (and default parameters) are\cr
#' \itemize{
#'  \item{"hue" - ...: none (see [scales::hue_pal()])}
#'  \item{"brewer" - ...: type="seq", palette=1, direction=1 (see [scales::brewer_pal()])}
#'  \item{"viridis" - ...: alpha=1,begin=0,end=0,direction=1,option='D' (see [scales::viridis_pal()])}
#' }
#' 
#' @examples
#' gg_CreateColors(3,show=TRUE);
#' gg_CreateColors(3,"brewer",show=TRUE);
#' gg_CreateColors(3,"viridis",show=TRUE); 
#' 
#' @seealso [scales::hue_pal()], [scales::brewer_pal()],[scales::viridis_pal()]
#'  
#' @import scales 
#' 
#' @export
#' 
gg_CreateColors<-function(n,
                          scale="hue",
                          ...,
                          show=FALSE){
    if (scale=="hue") 
        colors = scales::hue_pal()(n);
    if (scale=="brewer")
        colors = scales::brewer_pal(...)(n);
    if (scale=="viridis")
        colors = scales::viridis_pal(...)(n);
    if (show) scales::show_col(colors);
    return(colors);
}

#--testing
#  gg_CreateColors(3,show=TRUE);
#  gg_CreateColors(3,"brewer",show=TRUE);
#  gg_CreateColors(3,"viridis",show=TRUE);
