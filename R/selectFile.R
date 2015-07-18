#' @title Select a file.
#'  
#' @description Function allows the user to select a file using a gui interface.
#' 
#' @param ext - extension for files to choose from
#' @param caption - caption for file dialog (if file name not provided)
#' @param multi - flag (T/F) to allow multiple file selection
#' 
#' @return Selected file name(s). Returns NULL if the user canceled selection using the file dialog.
#' 
#' @importFrom tcltk tk_choose.files
#' 
#' @export
#' 
selectFile<-function(ext='*',caption=paste("Select .",ext," file(s) to import",sep=''),multi=FALSE){
    if (ext==''){
        #this does NOT seem to work for files w/out extensions
        file<-tcltk::tk_choose.files(caption=caption,multi=multi,
                                     filters=matrix(c("executables",""),1,2,byrow=TRUE));
    } else if (ext==' '){
        #this does NOT seem to work for files w/out extensions
        file<-tcltk::tk_choose.files(caption=caption,multi=multi,
                                     filters=matrix(c("executables"," "),1,2,byrow=TRUE));
    } else if (ext=='*'){
        file<-tcltk::tk_choose.files(caption=caption,multi=multi,
                                     filters=matrix(c("All","*"),1,2,byrow=TRUE));
    } else {
        Filters<-addFilter(ext,paste(ext,"files (*.",ext,")",sep=''),paste("*.",ext,sep=''));
        file<-tcltk::tk_choose.files(caption=caption,multi=multi,
                                     filters=matrix(Filters[ext,],1,2,byrow=TRUE));
    }
    if (length(file)==0) return(NULL);
    return(file)
}

#file<-selectFile();