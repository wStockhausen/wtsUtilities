#'
#' @title Calculate the missing dimension of a ggplot2 object given the other
#' 
#' @description Function to calculate the missing dimension of a 
#' ggplot2 object given the other dimension.
#' 
#' @param ggobj  - a ggplot2 object (or possibly a gtable)
#' @param fxdDim - value for the fixed dimension
#' @param fitWidth - flag indicating fixed dimension is width
#' @param units - character string with units for the fixed ddimension
#' @param verbose - flag to print intermediate information
#' @param ... - not used
#' 
#' @return a 2-element list (height, width) with the plot dimensions
#' 
#' @details Based on code from the DeLuciatoR::get_dims function. 
#' 
#' This function
#' calculates the total height/width based on setting the total width/height (the 
#' latter determines the size of a "null" unit for expandable grobs). 
#' Another approach is taken in [gg_GetPlotDims], which fixes the size of null units,
#' then calculates the resulting height and width.
#' 
#' @import grid
#' @import ggplot2
#' @importFrom utils packageVersion
#'
#' @export
#' 
gg_FixOnePlotDim = function(ggobj, fxdDim, fitWidth=TRUE, units="in", verbose=FALSE, ...){

	if (verbose) cat("--starting gg_FixOnePlotDim\n");
	# This approach relies on the quirk that
	# grid::convertUnit treats null units as 0.
	# If this changes, it will be rewrite time.
	stopifnot(
		grid::convertUnit(grid::unit(1, "null"), "in", "x", valueOnly = TRUE) == 0
	)
	# Open a temporary plotting device to do the calculations inside.
	# This is for two reasons: Ultimately because unit conversions
	# are device-dependent, but proximally because otherwise Grid will
	# open one for us and leave it open for the user to deal with.
	tmpf = tempfile(pattern="dispos-a-plot", fileext= ".png")
    h = ifelse(!fitWidth,fxdDim,100*fxdDim);
    w = ifelse( fitWidth,fxdDim,100*fxdDim);
	png(
		filename=tmpf,
		height=h,
		width =w,
		units=units,
		res=120,
		...)
	on.exit({dev.off(); unlink(tmpf)})
	if (verbose){
	    cat("device height = ",h,"\n")
	    cat("device width  = ",w,"\n")
	}
	rm(h,w);

	# Supported plot types: Supposedly handles anything ggplot can produce.
	# Probably works on other gtables, e.g. gridExtra::arrangeGrob,
	# but these are less tested. TODO: Lattice output?
	if(inherits(ggobj, "ggplot")){
	    if (verbose) cat("ggobj is a ggplot object\n");
		g = ggplot2::ggplotGrob(ggobj)
	}else if (inherits(ggobj, "gtable")){
	    if (verbose) cat("ggobj is a gtable object\n");
		g = ggobj
	}else{
		stop("Don't know how to get sizes for object of class ",
			deparse(class(ggobj)))
	}
	if (verbose){
	    cat("num. rows:",length(g$heights),"\n")
	    cat("row heights:\n");
	    print(g$heights);
	    cat("num. cols:",length(g$widths),"\n")
	    cat("colwidths:\n");
	    print(g$widths);
	}

	# This sum gives the dimensions filled by *fixed-size* grobs.
	# We'll divide the remaining available space between rows/columns
	# in proportion to their size in null units.
	known_ht = sum(grid::convertHeight(g$heights, units, valueOnly=TRUE))
	known_wd = sum(grid::convertWidth( g$widths,  units, valueOnly=TRUE))
	if (verbose){
	    cat("known_ht = ",known_ht,"\n");
	    cat("known_wd = ",known_wd,"\n");
	}

	# Find rows & columns specified in null units.
	if (packageVersion("grid") >= "4.0.0") {
		# Grid units changed in 4.0.0 and broke old method,
		# but fortunately also provided a much easier new way
		null_rowhts <- as.numeric(g$heights[grid::unitType(g$heights) == "null"])
		null_colwds <- as.numeric(g$widths[ grid::unitType(g$widths)  == "null"])
		panel_asps <- (
			matrix(null_rowhts, ncol = 1)
			%*% matrix(1 / null_colwds, nrow = 1))
	} else {
		# This is a convoluted process in grid < 4.0 because unit names are
		# potentially many layers deep in unit.arithmetic or unit.list objects.
		# Rather than access them directly, we compute fixed dimensions twice,
		# once as normal and once after replacing all null units with inches.
		# Then the difference between these, even though reported as inches,
		# is the dimension in nulls.
		all_null_rowhts <- (
			grid::convertHeight(.null_as_if_inch(g$heights), "in", valueOnly = TRUE)
			- grid::convertHeight(g$heights, "in", valueOnly = TRUE))
		all_null_colwds <- (
			grid::convertWidth(.null_as_if_inch(g$widths), "in", valueOnly = TRUE)
			- grid::convertWidth(g$widths, "in", valueOnly = TRUE))
		null_rowhts <- all_null_rowhts[all_null_rowhts > 0]
		null_colwds <- all_null_colwds[all_null_colwds > 0]

		panel_asps <- (
			matrix(null_rowhts, ncol = 1) %*% matrix(1 / null_colwds, nrow = 1))
	}
	if (verbose){
	    cat("null_rowhts =",null_rowhts,"\n");
	    cat("null_colwds =",null_colwds,"\n");
	    cat("panel_asps  =",panel_asps,"\n");
	}

	if (fitWidth) {
      	width  = fxdDim;
      	colwds = (fxdDim-known_wd) * null_colwds / sum(null_colwds);#sum(colwds) = fxdDim-known_wd
      	rowhts = colwds[1] * panel_asps[,1];
      	height = known_ht + sum(rowhts);
      	if (verbose){
      	    cat("colwds      =",colwds,"\n");
      	    cat("rowhts      =",rowhts,"\n");
      	    cat("sum(rowhts) =",sum(rowhts),"\n")
      	}
	} else {
      	height = fxdDim;
      	rowhts = (fxdDim-known_ht) * null_rowhts / sum(null_rowhts);#sum(rowhts) = fxdDim-known_ht
      	colwds = rowhts[1] / panel_asps[1,];
      	width  = known_wd + sum(colwds);
      	if (verbose){
      	    cat("colwds      =",colwds,"\n");
      	    cat("rowhts      =",rowhts,"\n");
      	    cat("sum(colwds) =",sum(colwds),"\n")
      	}
	}
# 
# 	rowhts_if_maxwd = max_colwds[1] * panel_asps[,1]
# 	colwds_if_maxht = max_rowhts[1] / panel_asps[1,]
# 	# print(c("rowhts_if_maxwd: ", rowhts_if_maxwd, " colwds_if_maxht: ", colwds_if_maxht))
# 
# 	height = min(maxheight, known_ht + sum(rowhts_if_maxwd))
# 	width = min(maxwidth, known_wd + sum(colwds_if_maxht))

	if (verbose) cat("--finished gg_FixOnePlotDim\n");
	return(list(height=height, width=width))
}


	