#'
#' @title Get the extension from a filename
#'
#' @description Function to get the extension from a filename
#'
#' @param fn - the filename 
#' 
#' @return - the extension (without the "."), or NA if no extension
#' 
#' @details If the filename has no extension, the functions returns NA. If the filename ends
#' with ".", then "" is returned.
#' 
#' @importFrom stringr str_extract
#' 
#' @export
#' 
getFileExtension<-function(fn){
  bn = basename(fn);
  ext = stringr::str_extract(bn,"(?<=\\.)[^.]+$"); #--could also use (\\.[^.]+)$
  if (is.na(ext)) ext = stringr::str_extract(bn,"(?<=\\.)$"); #--check if filename ends in "."
  return(ext);
}