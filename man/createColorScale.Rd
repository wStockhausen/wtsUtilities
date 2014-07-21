\name{createColorScale}
\alias{createColorScale}
\title{Create a function defining a color scale.}
\usage{
createColorScale(clrs = c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F",
  "yellow", "#FF7F00", "red", "#7F0000"), name = NULL, bias = 1,
  space = c("rgb", "Lab"), interpolate = c("linear", "spline"),
  showAsWheel = FALSE, showAsBar = FALSE)
}
\arguments{
  \item{name}{- name of base color scale (pre-defined:
  'jet', 'coldhot', 'cold', and 'hot'; NULL for new scale)}

  \item{clrs}{- vector of colors defining the base scale
  (ignored if 'name' is given)}

  \item{bias}{- ??}

  \item{space}{- colorspace ('rgb' or 'Lab')}

  \item{interpolate}{- interpolation scheme ('linear' or
  'spline')}

  \item{showAsWheel}{- flag (T/F) to show the final palette
  as a colorwheel}

  \item{showAsBar}{- flag (T/F) to show final palette as a
  colorbar}
}
\value{
a function reflecting a color scale.
}
\description{
Creates a function defining a color scale. The resulting
function (e.g., 'scale') is used as \cr \code{clr <-
scale(x,mn=sclMin,mx=sclMax,alpha=alpha)} \cr to return a
color based on the color associated with value x (scaled
into the interval sclMin to sclMax) in the defined color
scale, with transparency 'alpha'.
}
\details{


New color scales can be created based on pre-defined color
vectors using the following names: \cr 'jet', 'coldhot',
'cold', and 'hot' \cr Specifying a pre-defined name
eliminates the need to specify the 'clrs' color vector. \cr
Arbitrary scales can created by specifying NULL for 'name'
(the default) and providing a vector of colors to use to
define the scale function. This uses the graphics function
"colorRamp" to define the underlying color ramp used for
the scale.
}

