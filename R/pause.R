#' @title Pause R for a defined time              
#'                             
#' @description Runs a loop that causes the system to pause operation for 't' seconds   
#' 
#' @param t - number of seconds to pause         
#' @param debug - flag (T/F) to print debugging info
#'          
#'@export
#'
pause<-function(t,debug=FALSE){
    if (debug) cat("pausing ",t," seconds.\n",sep="");
    t0<-Sys.time()+t;
    while (Sys.time()<t0) {
    }
    if (debug) cat("done!\n");
}