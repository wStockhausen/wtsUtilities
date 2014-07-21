\name{createColorPalette}
\alias{createColorPalette}
\title{Create a color palette.}
\usage{
createColorPalette(name, n = 1, alpha = 1, start = 0, end = max(1, n -
  1)/n, gamma = 1, s = 1, v = 1, bias = 1, space = c("rgb", "Lab"),
  interpolate = c("linear", "spline"), clrs = c("#00007F", "blue",
  "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
  showAsWheel = FALSE, showAsBar = FALSE)
}
\arguments{
  \item{name}{- name of base palette to use (pre-defined:
  'rainbow','heat','topo', 'cm', and 'jet'; 'ramp' for new
  palette)}

  \item{n}{- number of colors to include in palette}

  \item{alpha}{- transparency (0-1)}

  \item{start}{- value associated with 1st color in
  palette}

  \item{end}{- value associated with last color in palette}

  \item{s}{- parameter passed to "rainbow" function}

  \item{v}{- parameter passed to "rainbow" function}

  \item{bias}{- ??}

  \item{space}{- colorspace ('rgb' or 'Lab')}

  \item{interpolate}{- interpolation scheme ('linear' or
  'spline')}

  \item{clrs}{- vector of colors defining base palette}

  \item{showAsWheel}{- flag (T/F) to show the final palette
  as a colorwheel}

  \item{showAsBar}{- flag (T/F) to show final palette as a
  colorbar}
}
\value{
a color palette
}
\description{
Creates a color palette (or returns a pre-defined one).
}
\details{


New (interpolated) palettes can be created based on the
following pre-defined palettes: \cr 'rainbow', 'heat',
'topo', 'cm', and 'jet'\cr Specifying a pre-defined palette
eliminates the need to specify the 'clrs' color vector.\cr
Palettes can created based on arbitrary base palettes by
specifying name as 'ramp' and providing a vector of colors
to use as the base palette (via clrs). This appraoch uses
the grpahics function "colorRampPalette" to create the
palette.
}

