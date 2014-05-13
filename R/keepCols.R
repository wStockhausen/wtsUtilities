#'
#' @title keepCols: keep columns from a dataframe
#' 
#'  @description
#'       Create dataframe with subset of columns of input dataframe "tbl" by
#'       keeping only columns matching the specified names.
#' 
#' @param  tbl  - dataframe with columns to keep or discard
#' @param cols - vector of column names to keep
#' @param debug - flag (T/F) to print debugging info
#'       
#' @return a dataframe with columns to be kept, or NULL if no column names matched.
#'
#'@export
#'
keepCols<-function(tbl,cols,debug=FALSE){
    if (length(cols)>0) {
        #loop through columns to keep and find matches with table names
        nms<-names(tbl);
        if (debug) {
            cat('keepCols:\ninput table names ',nms,'\n');
            cat('columns to keep: (',paste(cols,collapse=","),')\n');
        }
        res<-rep(FALSE,length(nms));
        for (c in 1:length(cols)) {
            resp<-(nms==cols[c]);
            res<-res|resp;
            if (debug) cat('col = "',cols[c],'"\n',paste('resp =',resp,sep=' '),'\n',sep='');
        }
        if (debug) cat('Selection: \n',res,'\n');
        #keep only columns that match names to be kept
        if (debug) cat('Selected columns = ',nms[res],'\n');
        tblp<-as.data.frame(tbl[,res]);
        names(tblp)<-nms[res];
        if (debug) cat('output table names: \n',names(tblp),'\nfinished keepCols\n');
        return(tblp);
    } else {
        return(NULL);#no columns to be kept
    }
}