#'
#' @title Sum variables in a dataframe by a set of factor levels
#'
#' @description  Sums variables in a dataframe by a set of factor levels. 
#' Uses \code{getFactorLevels} to identify the unique factor combinations in columns
#' identified as factors.
#' 
#' @param df - dataframe (or object that can be coerced to a dataframe)
#'            with variables to be summed and factor levels to be summed by
#' @param factors - vector of names of columns in df
#'                 to serve as factors in the summations
#' @param vars    - vector of names of column in df
#'                 that should be summed
#' @param sortBy  - vector of factor column names to sort results by
#' @param ascending - sort in ascending order
#'       
#'@return a dataframe of results with columns corresponding to factors and vars
#'
#'@export
#'
#source("../Utilities/getFactorLevels.R",chdir=TRUE);
sumBy<-function(df,factors,vars,sortBy=NULL,ascending=TRUE) {
    #coerce df to dataframe, if necessary
    if (!is.data.frame(df)) df<-as.data.frame(df);

    #get factor levels
    u<-getFactorLevels(df,factors,sortBy=sortBy,ascending=ascending);

    #loop over unique factor combinations and get sum results
    nu<-nrow(u);
    subset<-(df[,names(u)[1]]==u[1,1]);
    if (ncol(u)>1) {
        for (c in 2:ncol(u)) {
            subset<-subset & (df[,names(u)[c]]==u[1,c]);
        }
    }
    dfp<-subset(df,subset,vars);
    sum<-lapply(dfp,sum);
    res<-data.frame(u[1,],sum);
    names(res)<-c(names(u),vars);
    if (nu>1) {
        for (r in 2:nu) {
            subset<-(df[,names(u)[1]]==u[r,1]);
            if (ncol(u)>1) {
                for (c in 2:ncol(u)) {
                    subset<-subset & (df[,names(u)[c]]==u[r,c]);
                }
            }
            dfp<-subset(df,subset,vars);
            sum<-lapply(dfp,sum);
            resp<-data.frame(u[r,],sum);
            names(resp)<-c(names(u),vars);
            res<-rbind(res,resp);
        }
    }

    return(res);
}