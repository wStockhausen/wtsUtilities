#'
#' @title Get paths to all terminal list elements as a character vector
#'
#' @description Function to get paths to all terminal list elements as a character vector.
#'
#' @param lst - list of interest
#' @param top - dummy for recursion
#' @param  level - dummy for recursion
#' @param  verbose - flag (T/F) to print diagnostic output
#' 
#' @return character vector with paths to each non-NULL terminal element of the list, in unix format
#' 
#' @details None
#' 
#' @export
#' 
getListElementPaths<-function(lst,top=".",level="",verbose=TRUE){
    ctr<-0;
    pths<-list();
    if (is.list(lst)){
        nms<-names(lst);
        for (nm in nms){
            if (verbose) cat(level,"getSubListPaths(): checking '",paste0(top,"/",nm),"' for sublists.\n",sep='');
            if (nm!=''){
                subnms<-getSubListPaths(lst[[nm]],top=paste0(top,"/",nm),level=paste0(level,"--"),verbose=verbose);
                if (is.list(subnms)) {
                    if (verbose) cat(paste0(level,"-"),nm," has elements: ",paste0("'",unlist(subnms),"'",collapse=", "),".\n",sep='');
                    for (subnm in subnms) {ctr<-ctr+1; pths[[ctr]]<-subnm;}
                } else {
                    ctr<-ctr+1; pths[[ctr]]<-subnms;
                }
            }
        }#--i
        if (top!=".") return(pths);
        pths<-unlist(pths);
        pths<-gsub('./','',pths,fixed=TRUE);
        return(pths);
    } else {
        if (verbose) cat(paste0(level,"-"),top,"is a terminal element.\n",sep=' ');
        return(top);
    }
}
