#'
#' @title Get file structure
#' 
#' @param path - path to toplevel folder
#' @param all.files - flag to include all files
#' @param full.names - flag to return fullnames
#' @param recursive - flag to determine structure recursively
#' @param include.dirs - flag to include folders 
#' 
#' @return text vector
#' 
#' @details Uses \code{[base]{list.files}} to recursively determine file structure.
#' 
#' @export
#' 
getFileStructure<-function(path=".",
                           all.files=TRUE,
                           full.names=TRUE,
                           recursive=TRUE,
                           include.dirs=FALSE){
    txt = list.files(path=path,
                     all.files=all.files,
                     full.names=full.names,
                     recursive=recursive,
                     ignore.case=FALSE,
                     include.dirs=include.dirs);
    return(txt);
}

#'
#' @title Get folder structure
#' 
#' @param path - path to toplevel folder
#' @param full.names - flag to return fullnames
#' @param recursive - flag to determine structure recursively
#' 
#' @return text vector
#' 
#' @details Uses \code{[base]{list.dirs}} to determine folder structure.
#' 
#' @export
#' 
getFolderStructure<-function(path=".",full.names=TRUE,recursive=TRUE){
    txt = list.dirs(path=path,
                     full.names=full.names,
                     recursive=recursive);
    return(txt);
}