#' 
#' @title Calculate differences between dataframe columns by row
#' @description
#' Function to calculate differences between dataframe columns by row.
#' 
#' @param dfr - dataframe 
#' @param col - column name for base 
#' @param cols - vector of column names to compute by-row differences for (or NULL)
#' @param adjacent - flag to compute differences between "adjacent" columns (default=FALSE)
#' 
#' @return copy of input dataframe with extra columns representing column differences
#' 
#' @details
#' If \code{cols} is NULL, then differences will be commputed as if \code{cols} was 
#' the vector of names of columns to the "right" of \code{col}. If \code{adjacent=FALSE}, 
#' then all differences are relative to \code{col} as the base. Otherwise, after the 
#' first difference, the base column is sequentially moves through those in \code{cols}.
#' 
#' @export
#' 
calcColumnDiffs<-function(dfr,col,cols=NULL,adjacent=FALSE){
  nms = names(dfr);
  idcol = which(nms==col)
  if (is.null(cols)){
    idcols = (idcol+1):length(nms);
  } else {
    idcols = which(nms %in% cols)
  }
  tmp = dfr;
  #--calculate diffs between id column and base column
  base = idcol;
  for (id in idcols){
    colnm = paste(nms[id],"-",nms[base]);
    tmp[[colnm]] = tmp[[id]]-tmp[[base]];
    if (adjacent) base = id;#--set base to current column
  }
  return(tmp);
}
