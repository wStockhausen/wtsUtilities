#'
#'@title Add transparency to a vector of colors
#'
#'@description Function to add transparency to a vector of colors
#'
#'@param clrs - vector of colors
#'@param alpha - level of transparency to add (0-1)
#'
#'@return corresponding vector of colors with transparency added
#'
#'@details None.
#'
#'@importFrom grDevices col2rgb
#'@importFrom grDevices rgb
#'
#'@export
#'
addTransparency<-function(clrs,alpha=1.0){
    clrs.rgb<-col2rgb(clrs);
    clrs.col<-rgb(clrs.rgb[1,],clrs.rgb[2,],clrs.rgb[3,],255*alpha,maxColorValue=255);
    return(clrs.col)
}
