#' 
#' @title Save an R object to an RData file with name 'obj'
#' 
#' @description Function to save an R object to an RData file with name 'obj'.
#' 
#' @param obj - object to save
#' @param fn - filename to which to save the R object 
#' 
#' @details The original R object is saved with the name 'obj', so it can easily
#' be returned using the function \code{\link{getObj}}.
#' 
#' @export
#' 
saveObj<-function(obj,fn){
  save(obj,file=fn);
}

#' 
#' @title Get saved object from an RData file
#' 
#' @description Function to get a saved object ('obj') from an RData file.
#' 
#' @param fn - filename from which to extract R object 
#' 
#' @return the object
#' 
#' @details The original R object must have been saved with the name 'obj' (see
#' \code{\link{saveObj}} for a function to do so).
#' 
#' @export
#' 
getObj<-function(fn){
    if (!file.exists(fn)){
        stop(paste0("File '",fn,"' does not exist!"))
    }
    load(file=fn,envir = environment());
    return(obj);
}
