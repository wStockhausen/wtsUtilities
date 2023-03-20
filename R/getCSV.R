#' @title Open a csv file as a dataframe.
#'  
#' @description Opens a csv file and returns it as a dataframe.\cr 
#' If a file name is not provided, the function displays a (tcltk)
#' file chooser dialog.\cr 
#' Uses package "tcltk".
#' 
#' @param csvfile - name of csv file to open
#' @param caption - caption for file dialog (if file name not provided)
#' 
#' @return dataframe based on reading csv file. Returns NULL if no csv file
#' was provided and the user canceled selection using the file dialog.
#' 
#' @details Uses packages \code{utils}, \pkg{tcltk}.
#' 
#' @importFrom tcltk tk_choose.files
#' 
#' @export
#' 
getCSV<-function(csvfile=NULL,caption="Select csv file to import"){
    if (is.null(csvfile)) {
        csvfile<-selectFile(ext='csv',caption=caption);
        if (length(csvfile)==0) return(NULL);
    }  
    
    dfr<-utils::read.csv(file=csvfile,stringsAsFactors=FALSE);
    return(dfr)
}

#dfr<-getCSV();