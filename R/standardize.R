#'
#'@title Standardize a vector or columns in a dataframe by converting to z-scores.
#'
#'@description Function to standardize a vector or columns in a dataframe by converting to z-scores.
#'
#'@param x - vector or dataframe to standardize
#'@param cols - vector of column names to standardize (if x is a dataframe)
#'@param factors - vector of column names used as factors to standardize by (if x is a dataframe), or NULL
#'@param idxCols - vector of column names used as indices for standardized values, or NULL
#' @param verbose - flag (T/F) to print diagnostic information
#'
#'@return standardized vector as z-scores (same size as x), or dataframe
#'
#'@details idxCols are just copied over from the relevant section of each combination of factor levels.
#'
#'@export
#'
standardize<-function(x,cols=NULL,factors=NULL,idxCols=NULL,verbose=FALSE){
    xp<-x;
    if (is.vector(xp)) {
      return(standardize.vector(xp));
    } else if (is.data.frame(xp)&is.null(factors)){
      for (col in cols) xp[[col]]<-standardize.vector(xp[[col]]);
      return(xp);
    }
  
    dfr<-xp;
    uFacs<-unique(dfr[,factors]);
    colStr<-paste0("d.",cols,collapse=", ");
    facStr<-paste0("d.",factors,collapse=", ");
    whrStr<-paste0(paste0("d.",factors,"=u.",factors),collapse=" AND \n");
    idxStr<-"";
    if (!is.null(idxCols)) idxStr<-paste0(paste0("d.",idxCols,collapse=", "),",");
    #--loop over unique factor combinations and add standardized values
    dfr1<-NULL;
    for (rw in 1:nrow(uFacs)){
        #--select
        uFacs1<-uFacs[rw,];
        qry<-"select 
                &&facStr,
                &&idxStr
                &&colStr
              from dfr d, uFacs1 u
              where &&whrStr
              order by &&facStr;";
        qry<-gsub("&&facStr",facStr,qry,fixed=TRUE);
        qry<-gsub("&&idxStr",idxStr,qry,fixed=TRUE);
        qry<-gsub("&&colStr",colStr,qry,fixed=TRUE);
        qry<-gsub("&&whrStr",whrStr,qry,fixed=TRUE);
        if (verbose) cat("qry:\n",qry,"\n");
        dfr2<-sqldf::sqldf(qry);
        if (verbose){
            print(head(dfr2))
        }
        for (col in cols){
            if (verbose) cat(col,":\n",sep="");
            z<-standardize.vector(dfr2[[col]]);
            if (verbose) print(z);
            dfr2[[paste0("std_",col)]]<-z;
        }
        dfr1<-rbind(dfr1,dfr2);
    }
    return(dfr1);
}

#'
#'@title Standardize a vector by converting to z-scores.
#'
#'@description Function to standardize a vectorby converting to z-scores.
#'
#'@param x - vector to standardize
#'
#'@return standardized vector as z-scores (same size as x)
#'
#'@details If sd(x) is 0, then 0*x is returned. If sd(x) is NA, then NA+x is returned.
#'
#'not exported
#'
standardize.vector<-function(x){
    mn<-mean(x,na.rm=TRUE);
    sd<-sd(x,na.rm=TRUE);
    if(is.na(sd)){
        y<-x+NA;##all NA's
    } else if (sd==0){
        y<-0*x;##all 0's
    } else {
      y<-(x-mn)/sd;
  }
  return(y);
}