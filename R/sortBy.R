#'
#' @title sort a dataframe by row according to columns. 
#' 
#' @description Returns a dataframe sorted by row according to the combination of values in
#'       the columns specified by "factors".
#'       
#' @param df        - input dataframe (or object that can be cast to a dataframe)
#' @param factors   - vector of desired column names from df to sort by
#' @param na.last   - flag (T/F) to order NAs last/first (=NA removes any NAs)
#' @param ascending - flag (T/F) to sort in ascending/descending order
#' @param debug     - flag (T/F) to print debugging info
#' 
#' @return sorted version of df
#' 
#' @details None.
#'
#'@export
#'
sortBy<-function(df,
                 factors,
                 na.last=TRUE,
                 ascending=TRUE,
                 debug=FALSE){
    #coerce df to dataframe, if necessary
    if (!is.data.frame(df)) {
        if (debug) cat("sortBy: coercing df to data frame\n")
        df<-as.data.frame(df);
        names(df)<-factors;
    }

    #compute permutation to sort dataframe df by factors using R function "order"
    expr<-"order(&&f na.last=&&na,decreasing=!&&asc)";
    str<-paste(factors,",",sep="",collapse="");
    expr<-gsub("&&f"  ,str,expr); #replace &&f with str in expr
    expr<-gsub("&&na" ,paste(na.last),expr); #replace &&na with na.last in expr
    expr<-gsub("&&asc",paste(ascending),expr); #replace &&asc with ascending in expr

    expr<-as.expression(parse(text=expr));
    if (debug) print(expr);
    perm<-eval(expr,df);

    #now do the actual sorting
    res<-df[perm,];

    #make sure res is a dataframe (will not be if df has a single column)
    if (!is.data.frame(res)) res<-as.data.frame(res);

    #make sure column names of res are same as df
    names(res)<-names(df);

    return(res)
}