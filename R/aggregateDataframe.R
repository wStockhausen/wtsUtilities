#'
#'@title Aggregate a dataframe according to "factor" levels.
#'
#'@description Function to aggregate a dataframe according to "factor" levels.
#'
#'@param dfr - dataframe
#'@param factors - factors to aggregate by
#'@param vars - variable(s) to aggregate
#'
#'@return dataframe with vars aggregated by levels of "factor" columns
#'
#'@import sqldf
#'
#'@export
#'
aggregateDataframe<-function(dfr,
                                factors=NULL,
                                vars=NULL){
    if (is.null(factors)){
        stop('Must supply a vector of column names to aggregate by.\nAborting...\n');
    }
    if (is.null(vars)){
        stop('Must supply a vector of column names to aggregate.\nAborting...\n');
        
    }
    dfrN<-dfr;
    facs1<-paste(factors,collapse=',');
    vars1<-paste("sum(",vars,") as ",vars,collapse=', ',sep='')
    query<-"select
                &&facs,
                &&vars
            from
                dfrN
            group by
                &&facs;"
    query<-gsub('&&facs',facs1,query,fixed=TRUE);
    query<-gsub('&&vars',vars1,query,fixed=TRUE);
    agg<-sqldf(query);
    return(agg);
}
    