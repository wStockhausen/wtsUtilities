#' @title Select a file
#'  
#' @description Function allows the user to select a file using a gui interface.
#' 
#' @param ext - extension for files to choose from
#' @param caption - caption for file dialog (if file name not provided)
#' @param path - path to folder (defaults to [rstudioapi::getActiveProject()])
#' 
#' @return Selected file name. Returns NULL if the user canceled selection using the file dialog.
#' 
#' @details Uses [rstudioapi::selectFile()] to select file.
#' 
#' @importFrom rstudioapi getActiveProject selectFile
#' 
#' @export
#' 
selectFile<-function(ext='*',
                     caption=paste0("Select .",ext," file to import",),
                     path=rstudioapi::getActiveProject()){
    if (ext[1] %in% c('',"*")){
        #this does NOT seem to work for files w/out extensions
        file<-rstudioapi::selectFile(caption=caption,label="Select",filter="All Files (*)",existing=TRUE,path=path);
    } else {
        Filters<-addFilter(ext,paste(ext,"files (*.",ext,")",sep=''),paste("*.",ext,sep=''));
        file<-rstudioapi::selectFile(caption=caption,label="Select",filter=Filters[ext,],existing=TRUE,path=path);
    }
    if (length(file)==0) return(NULL);
    return(file)
}

#file<-selectFile();