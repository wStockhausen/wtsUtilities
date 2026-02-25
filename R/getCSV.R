#' @title Open a csv file as a dataframe.
#'  
#' @description Opens a csv file and returns it as a dataframe.\cr 
#' If a file name is not provided, the function displays a 
#' file chooser dialog.\cr 
#' 
#' @param csvfile - path to csv file to open (or NULL to invoke a dialog)
#' @param caption - caption for file dialog (if file name not provided)
#' 
#' @return A tibble based on reading csv file. Returns NULL if no csv file
#' was provided and the user canceled selection using the file dialog.
#' 
#' @details The returned object is a [tibble::tibble()], basically an enhanced dataframe.
#' 
#' @importFrom readr read_csv
#' @importFrom rstudioapi selectFile
#' @export
#' 
getCSV<-function(csvfile=NULL,caption="Select csv file to import"){
    if (is.null(csvfile)) {
        csvfile<-selectFile(ext="csv",caption=caption);
        if (length(csvfile)==0) return(NULL);
    }  
    
    dfr<-readr::read_csv(file=csvfile);
    return(dfr)
}

#dfr<-getCSV();