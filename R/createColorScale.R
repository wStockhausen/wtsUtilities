#'
#'@title Create a function defining a color scale.
#'
#'@description Creates a function defining a color scale. The resulting function (e.g., 'scale') is used as \cr 
#'\code{clr <- scale(x,mn=sclMin,mx=sclMax,alpha=alpha)} \cr 
#' to return a color based on the color associated with value x (scaled into the interval sclMin to sclMax) 
#' in the defined color scale, with transparency 'alpha'.
#'
#'@param name - name of base color scale (pre-defined: 'jet', 'coldhot', 'cold', and 'hot'; NULL for new scale)
#'@param clrs - vector of colors defining the base scale (ignored if 'name' is given)
#'@param bias - ??
#'@param space - colorspace ('rgb' or 'Lab')
#'@param interpolate - interpolation scheme ('linear' or 'spline')
#'@param showAsWheel - flag (T/F) to show the final palette as a colorwheel
#'@param showAsBar - flag (T/F) to show final palette as a colorbar
#'
#'@details New color scales can be created based on pre-defined color vectors using the following names: \cr 
#''jet', 'coldhot', 'cold', and 'hot' \cr 
#'Specifying a pre-defined name eliminates the need to specify the 'clrs' color vector. \cr 
#'Arbitrary scales can created by specifying NULL for 'name' (the default) and providing a vector
#'of colors to use to define the scale function. This uses the graphics function "colorRamp" to define
#'the underlying color ramp used for the scale.
#'
#'@return a function reflecting a color scale.
#'
#'@export
#'@import graphics
#'
#require(graphics);
#source("../Plots/createColorPalette.R",chdir=TRUE);
#----------------------------------------------------------
createColorScale<-function(clrs=c("#00007F","blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                          name=NULL,
                          bias=1,
                          space=c("rgb","Lab"),
                          interpolate=c("linear","spline"),
                          showAsWheel=FALSE,
                          showAsBar=FALSE) {
    if (!is.null(name)) {
        if (name=="jet") {
            clrs<-c("#00007F","blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000");
        } else if (name=="coldhot"){
          reds<-c(0.164,0.15,0.25,0.45,0.67,0.88,1,1,1,0.97,0.85,0.65);
          grns<-c(0.043,0.306,	0.63,	0.853,	0.973,	1,	1,	0.88,	0.679,	0.43,	0.15,	0);
          blus<-c(0.85,  1,	1,	1,	1,	1,	0.75,	0.6,	0.45,	0.37,	0.196,	0.13);
          clrs<-rgb(reds,grns,blus)
        } else if (name=="hot"){
          reds<-c(0.97,	1,	1,	1,	1,	1,	1,	1);
          grns<-c(0.98,	1,	1,	1,	0.8,	0.6,	0.4,	0);
          blus<-c(1,	0.8,	0.6,	0,	0,	0,	0,	0);
          clrs<-rgb(reds,grns,blus)
        } else if (name=="cold"){
          reds<-c(0.03,  0.2,	0.35,	0.55,	0.75,	0.9,	0.97);
          grns<-c(0.353,  0.467,	0.567,	0.7,	0.833,	0.933,	0.98);
          blus<-c(1,  1,	1,	1,	1,	1,	1);
          clrs<-rgb(reds,grns,blus)
        }
    }
    rmp<-colorRamp(clrs,bias=bias,space=space,interpolate=interpolate);
    ramp<-function(x,mn=NULL,mx=NULL,alpha=1){
#         print(x);
        xrng<-range(x,na.rm=TRUE,finite=TRUE);
        if (is.null(mn)) mn<-xrng[1];
        if (is.null(mx)) mx<-xrng[2];
        if (mx==mn) {
            xp<-0*x;
        } else {
            xp<-(x-mn)/(mx-mn);#map into [0,1] based on mx,mn
            xp[xp<0]<-0;#set min color for x<mn
            xp[xp>1]<-1;#set max color for x>mx
        }
        vals<-rmp(xp)/255;#scale rgb triplets to [0,1]
        res<-rgb(vals[,1],vals[,2],vals[,3],alpha,maxColorValue=1);#change from rgb to R colors
        return(res);
    }

    if (showAsWheel) {
        n<-100;
        pal<-ramp((1:n)/n);#create a palette for convenience
        pie(rep(1,n),col=pal,clockwise=TRUE,init.angle=90,border=NA);
    }

    if (showAsBar) {
        n<-100;
        plot(c(0,1),c(0,n),type="n",xlab="",ylab="Scale (units)",ylim=c(0,n),yaxs="i",xaxs="i",xaxt="n");
        for (i in 1:n) {
            rect(0,i-1,1,i,col=ramp(i,mx=n,mn=0),border=NA);
        }
    }

    return(ramp)
}
