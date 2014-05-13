#' @title Create a menu of items
#' 
#' @description Creates a menu of items from a vector of values and a vector of prompts
#' 
#' @param vals - vector of values
#' @param prompts - ??
#' 
#'@export
#'
doMenu<-function(vals=NULL,prompts=NULL) {
    i<-1;
    for (i in 1:length(vals)) {
        prompts[i]<-paste(names(vals)[i]," = ",paste(vals,collapse=","));
    }
    while (i>0) {
        i<-menu(prompts,graphics=TRUE,title="Select variable to change: ");
        if (i>0) {
            nm<-names(vals)[i];
            cat(paste(nm," = ",paste(vals[[i]],collapse=","),"\n",sep=""));
            str<-paste("Enter value(s) for ",nm,"> ",sep="");
            res<-readline(prompt=str)
            vals[[i]]<-res;
        }
    }
    return(vals);
}