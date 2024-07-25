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
#' @title Get a saved object from an RData file
#' 
#' @description Function to get a saved object from an RData file.
#' 
#' @param fn - filename from which to extract R object 
#' @param obj_name - optional name (as 1-element character vector) of object to get
#' 
#' @return an R object
#' 
#' @details The RData file may contain multiple R objects, but the function will return 
#' only a single object. The RData file is "loaded" into a new environment. If `obj_name` is 
#' not NULL, then the object with that name is returned. Otherwise, a vector of 
#' object names is obtained using ls() and the object corresponding to the first name 
#' in the list is returned. 
#' 
#' @export
#' 
getObj<-function(fn,obj_name=NULL){
    if (!file.exists(fn)){
        stop(paste0("File '",fn,"' does not exist!"))
    }
    
    load(file=fn,envir=environment());#--load file
    obj_names = ls();                 #--get object names
    #--determine object name (if not give)
    if (!is.null(obj_name)) {
        #--return object associated with obj_name
        obj_name = obj_name[1];#--can only return one object
        if (!(obj_name %in% obj_names)) {
            warning(paste0("getObj: Requested object `",obj_name,"` not found. Returning NULL."));
            return(NULL);
        }
    } else {
        obj_name = obj_names[1];
    }
    #--make copy of object as `obj` to return
    eval(str2expression(text=paste0("obj=",obj_name)));
    return(obj);
}


#' 
#' @title Get the names of objects saved in an RData file
#' 
#' @description Function to get the names of objects saved in an RData file.
#' 
#' @param fn - filename from which to extract the names  
#' 
#' @return a character vector with the object names
#' 
#' @details The RData file is "loaded" in the function environment to determine 
#' the names of the associated objects. 
#' 
#' @export
#' 
getObjNames<-function(fn){
    if (!file.exists(fn)){
        stop(paste0("File '",fn,"' does not exist!"))
    }
    
    obj_names = load(file=fn,envir=environment());#--load file
    return(obj_names);
}
