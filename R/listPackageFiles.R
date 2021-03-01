#'
#' @title Get a vector of filenames associated with a package
#'
#' @description  Function to get a vector of filenames associated with a package.
#' 
#' @param pkg - name of package
#' @param sub - (optional) name of subfolder to check
#'       
#'@return a character vector of file paths relative to the package folder or sub-folder. 
#'
#'@details If \code{sub} is NULL, the names of all folders/files included in the package are returned.
#'Note, though, that this does NOT return file names from the package source, but from the compiled 
#'package.
#'
#'@export
#'
listPackageFiles<-function(pkg,
                           sub=NULL){
    if (is.null(sub)){
        lst = list.files(system.file(package=pkg),all.files=TRUE,include.dirs = TRUE,recursive=TRUE);
    } else {
        lst = list.files(system.file(sub,package=pkg),all.files=TRUE,include.dirs = TRUE,recursive=TRUE);
    }
    return(lst);
}
#listPackageFiles("wtsROMS")
#listPackageFiles("wtsROMS",sub="extdata")
