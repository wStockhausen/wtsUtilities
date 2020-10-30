#'
#' @title Add cumulative sums (standard and normalized) of a column to a dataframe by an index and set of factors
#'
#' @description  Function to add cumulative sums (standard and normalized) of a column to a 
#' dataframe by an index column and set of factor columns.
#' 
#' @param dfr - dataframe (or object that can be coerced to a dataframe)
#'              with values column to be cum-summed, index column, and factor levels
#' @param valCol - name of values column to be cum-summed (as character string; e.g., "num")
#' @param idxCol - name of "index" column to track cum-summing (as character string; e.g. "size", or NULL if none)
#' @param factors - vector of names of columns in dfr to serve as factors in the summations (or NULL)
#' @param verbose - flag (T/F) to print diagnostic information
#'       
#'@return a dataframe of results with columns corresponding to the factors, the index column (if defined), the values column,
#'a "nrmlzd" column, a "cumSum" column, and a "nrmSum" column. The latter three represent the normalized values 
#'(summing to 1), the cumulative sum of the values, and the cumulative sum of the normalized values, where the 
#'normalization and sums are taken over each factor combination and the associated index set. 
#'
#'@details Requires \pkg{sqldf}.
#'
#'@importFrom sqldf sqldf
#'
#'@export
#'
addCumSums<-function(dfr,
                     valCol,
                     idxCol=NULL,
                     factors=NULL,
                     verbose=FALSE){
    if (!is.null(factors)){
        uFacs<-unique(dfr[,factors]);
        facStr<-paste0("d.",factors,collapse=", ");
        whrStr<-paste0(paste0("d.",factors,"=u.",factors),collapse=" AND \n");
    } else {
        facStr<-"";
        whrStr<-"1=1"
    }
    if (verbose) cat("facStr = '",facStr,"'\n",sep="");
    if (verbose) cat("whrStr = '",whrStr,"'\n",sep="");
    
    #--loop over unique factor combinations and add 
    #--normalized values, cumulative sums, and cumulative normalized values
    dfr1<-NULL;
    for (rw in 1:nrow(uFacs)){
        #--select
        uFacs1<-uFacs[rw,];
        qry<-"select 
                &&facStr
                &&idxCol, d.&&valCol
              from dfr d, uFacs1 u
              where &&whrStr
              order by &&facStr &&idxCol;";
        qry<-gsub("&&facStr",facStr,qry,fixed=TRUE);
        if (is.null(idxCol)) {
            qry<-gsub("&&idxCol","",qry,fixed=TRUE);
        } else {
            qry<-gsub("&&idxCol",paste0(",d.",idxCol),qry,fixed=TRUE);
        }
        qry<-gsub("&&valCol",valCol,qry,fixed=TRUE);
        qry<-gsub("&&whrStr",whrStr,qry,fixed=TRUE);
        if (verbose) cat("qry:\n",qry,"\n");
        dfr2<-sqldf::sqldf(qry);
        dfr2[["nrmlzd"]]<-dfr2[[valCol]]/sum(dfr2[[valCol]]);
        dfr2[["cumSum"]]<-cumsum(dfr2[[valCol]]);
        dfr2[["nrmSum"]]<-cumsum(dfr2[["nrmlzd"]]);
        dfr1<-rbind(dfr1,dfr2);
    }
    return(dfr1);
}

