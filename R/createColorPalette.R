#'
#'@title Create a color palette.
#'
#'@description Creates a color palette (or returns a pre-defined one).
#'
#'@param name - name of base palette to use (pre-defined: 'rainbow','heat','topo', 'cm', and 'jet'; 'ramp' for new palette)
#'@param n - number of colors to include in palette
#'@param alpha - transparency (0-1)
#'@param start - value associated with 1st color in palette
#'@param end - value associated with last color in palette
#'@param s - parameter passed to "rainbow" function
#'@param v - parameter passed to "rainbow" function
#'@param bias - ??
#'@param space - colorspace ('rgb' or 'Lab')
#'@param interpolate - interpolation scheme ('linear' or 'spline')
#'@param clrs - vector of colors defining base palette
#'@param showAsWheel - flag (T/F) to show the final palette as a colorwheel
#'@param showAsBar - flag (T/F) to show final palette as a colorbar
#'
#'@details New (interpolated) palettes can be created based on the following pre-defined palettes: \cr
#''rainbow', 'heat', 'topo', 'cm', and 'jet'\cr
#'Specifying a pre-defined palette eliminates the need to specify the 'clrs' color vector.\cr 
#'Palettes can created based on arbitrary base palettes by specifying name as 'ramp' and providing a vector
#'of colors to use as the base palette (via clrs). This appraoch uses the grpahics function "colorRampPalette" 
#'to create the palette.
#'
#'@return a color palette
#'
#'@export
#'@import graphics
#'
#require(graphics);
#----------------------------------------------------------
createColorPalette<-function(name,
                             n=1,
                             alpha=1,
                             start=0,
                             end=max(1,n-1)/n,
                             gamma=1,
                             s=1,
                             v=1,
                             bias=1,
                             space=c("rgb","Lab"),
                             interpolate=c("linear","spline"),
                             clrs=c("#00007F","blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                             showAsWheel=FALSE,
                             showAsBar=FALSE) {
    if (name=="rainbow"){
        pal<-rainbow(n,s=s,v=v,start=start,end=end,gamma=gamma,alpha=alpha);
    } else if (name=="heat") {
        pal<-heat.colors(n,alpha=alpha);
    } else if (name=="terrain") {
        pal<-terrain.colors(n,alpha=alpha);
    } else if (name=="topo") {
        pal<-topo.colors(n,alpha=alpha);
    } else if (name=="cm") {
        pal<-cm.colors(n,alpha=alpha);
    } else if (name=="jet") {
        clrs<-c("#00007F","blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000");
        crp<-colorRampPalette(clrs,bias=bias,space=space,interpolate=interpolate);
        pal<-crp(n);
    } else if (name=="ramp") {
        crp<-colorRampPalette(clrs,bias=bias,space=space,interpolate=interpolate);
        pal<-crp(n);
    }

    if (showAsWheel) {
        border<-NULL;#use par("fg") setting for slice borders
        if (n>20) border = NA;#don't show borders
        pie(rep(1,n),col=pal,clockwise=TRUE,init.angle=90,border=border);
    }

    if (showAsBar) {
        border<-NULL;#use par("fg") setting for slice borders
        if (n>20) border = NA;#don't show borders
        plot(c(0,1),c(0,n),type="n");
        for (i in 1:n) {
            rect(0,i-1,1,i,col=pal[i],border=border);
        }
    }

    return(pal)
}
