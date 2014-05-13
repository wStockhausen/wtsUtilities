#' @title Compute results using an arbitrary function on columns in a dataframe, 
#' by a set of factor levels
#' @description Function to compute results from an arbitrary function (fcn)
#'       on colunms in a dataframe, by a set of factor levels
#' 
#' @param df - dataframe (or object that can be cast to a dataframe) with variables and factors
#' @param fcn     - function name AS CHARACTER STRING
#'                 to apply to each var in vars by each
#'                 unique factor level combination
#' @param factors - vector of names of columns in df
#'                 to serve as factors in the function applications
#' @param vars    - vector of names of column in df
#'                  that the function should be applied to
#' @param sortBy  - vector of factor column names to sort results by
#' @param ascending - flag (T/F) to sort in ascending/descending order
#' @param debug - flag (T/F) to print debugging info
#' @return
#'       dataframe of results with columns corresponding to factors and vars
#'
#'@export
#'
#source("../Utilities/getFactorLevels.R",chdir=TRUE);
#source("../Utilities/getFunctionName.R",chdir=TRUE);
computeFunctionByFactors<-function(df,
                                   fcn="sum",
                                   factors=NULL,
                                   vars=NULL,
                                   sortBy=NULL,
                                   ascending=TRUE,
                                   debug=FALSE){
    #coerce df to dataframe, if necessary
    if (!is.data.frame(df)) df<-as.data.frame(df);

    #get function corresponding to fcn
    nfcn<-fcn;
    fcn<-match.fun(fcn); #fcn is now mode of "function"
#    if (debug) print(getFunctionName(fcn));

    #get unique factor combinations
    u<-getFactorLevels(df,factors,sortBy=sortBy,ascending=ascending,debug=FALSE);
    if (debug) {
      cat("Unique factors:\n");
      print(u);
    }

    #compute function by looping over factor combinations
    if (debug) print("got here 1");
    nu<-nrow(u);
    r<-1;
    subset<-(df[,names(u)[1]]==u[r,1]);
    if (ncol(u)>1) {
        for (c in 2:ncol(u)) {
            subset<-subset & (df[,names(u)[c]]==u[r,c]);
        }
    }
    dfp<-subset(df,subset,vars);
    resfcn<-lapply(dfp,fcn); #this is a list of result vectors, with list names = vars
    uresfcn<-unlist(resfcn); #unlist to get results as vector
    unames<-lapply(strsplit(names(uresfcn),".",fixed=TRUE),paste,collapse="_"); #replace "." with "_" in row names
    mresfcn<-matrix(uresfcn,nrow=1,ncol=length(uresfcn),dimnames=list("1",unames));
    res<-data.frame(u[r,],mresfcn);
    if (debug) {
      cat("dfp:\n");
      print(dfp);
      cat("resfcn:\n");
      print(resfcn);
      cat("uresfcn:\n");
      print(uresfcn);
      cat("unames:\n");
      print(unames);
      cat("mresfcn:\n");
      print(mresfcn);
      cat("res:\n");
      print(res);
    }

    if (debug) print("got here 2");
    if (nu>1) {
        for (r in 2:nu) {
            subset<-(df[,names(u)[1]]==u[r,1]);
            if (ncol(u)>1) {
                for (c in 2:ncol(u)) {
                    subset<-subset & (df[,names(u)[c]]==u[r,c]);
                }
            }
            dfp<-subset(df,subset,vars);
            resfcn<-lapply(dfp,fcn); #this is a list of result vectors, with list names = vars
            uresfcn<-unlist(resfcn); #unlist to get results as vector
            unames<-lapply(strsplit(names(uresfcn),".",fixed=TRUE),paste,collapse="_"); #replace "." with "_" in row names
            mresfcn<-matrix(uresfcn,nrow=1,ncol=length(uresfcn),dimnames=list("1",unames));
            resp<-data.frame(u[r,],mresfcn);
            if (debug) {
              cat("dfp:\n");
              print(dfp);
              cat("resfcn:\n");
              print(resfcn);
              cat("uresfcn:\n");
              print(uresfcn);
              cat("unames:\n");
              print(unames);
              cat("mresfcn:\n");
              print(mresfcn);
              cat("res:\n");
              print(resp);
            }
            res<-rbind(res,resp);
        }
    }
    names(res)<-c(factors,vars);
    if (debug) {
      cat("res:\n");
      print(res);
    }

    return(res)
}

