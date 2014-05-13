#'
#' @title Unstack columns in a dataframe by a set of factor levels 
#' 
#' @description Creates a new dataframe by "unstack"ing columns in the
#'   input dataframe by factor level combinations in a set of other columns. Uses
#'   \code{getFactorLevels} to find the unique factor level combinations from
#'   the set of dataaframe columns identified as factors.
#' 
#' @param dfr - the dataframe
#' @param unstackCols - the names of the columns to unstack
#' @param factorCols - a vector of names of columns to use as factors
#' @param dropCols - a vector of names of columns to drop from the result
#' @param debug - flag (T/F) to print out debugging info
#' 
#' @return dataframe with unstacked columns and factor levels
#'
#'@export
#'
#source('../Utilities/getFactorLevels.R',chdir=TRUE);
unstackDataframe<-function(dfr,
                           unstackCols=NULL,
                           factorCols=NULL,
                           dropCols=NULL,
                           debug=TRUE){
    #get factor levels by which to unstack dfr
    faclevs<-getFactorLevels(dfr,factorCols,sortBy=factorCols);
    nf<-nrow(faclevs);
    if (debug) {
        cat("factor levels =\n");
        print(faclevs);
        cat("Number of factor level combinations = ",nf,"\n",sep="");
        readline(prompt="Press enter to continue...");
    }

    #extract names of columns to retain
    allcolnms<-names(dfr);
    colnms<-NULL;
    for (nm in allcolnms) {
      if (sum(nm==c(factorCols,dropCols,unstackCols))==0) colnms<-c(colnms,nm);
    }
    colnms<-colnms[!is.null(colnms)];#get rid of NULL
    if (debug) print(colnms);

    #extract data for factor level combination 1
    if (debug) print("Unstacking data for factor level combination 1");
    i<-1;
    mn<-paste(names(faclevs)[1],"=",faclevs[i,1],sep="");
    subst<-(dfr[,names(faclevs)[1]]==faclevs[i,1]);
    if (ncol(faclevs)>1) {
        nc<-ncol(faclevs);
        for (c in 2:nc) {
            mn<-paste(mn,"_",names(faclevs)[c],"=",faclevs[i,c],sep="");
            subst<-subst & (dfr[,names(faclevs)[c]]==faclevs[i,c]);
        }
    }
    if (length(colnms)>0){
      #extract data for retained columns
      if (debug) print("Extracting retained columns");
      dfp<-subset(dfr,subst,colnms);#subset dataframe for current factors
      if (debug){
        print(mn);
        cat("dfp ",i," has nrows = ",nrow(dfp),"\n",sep="");
        print(dfp);
        readline(prompt="Press enter to continue...");
      }
    }
    #extract data from unstackCols
    if (debug) print("Unstacking data from unstackCols");
    dfpr<-subset(dfr,subst,unstackCols);#subset dataframe for current factors
    nmn<-paste(unstackCols,mn,sep="_");#name for column
    if (debug) print(nmn);
    names(dfpr)<-nmn;
    if (debug) print(dfpr);
    #create dfp or add columns to dfp
    if (length(colnms)>0) {
      #dfp created above, so add columns
      dfp<-data.frame(dfp,dfpr,stringsAsFactors=FALSE);
    } else {
      #no retained but unstacked columns, so dfp not defined above
      dfp<-dfpr;
    }
    if (debug){
      print(mn);
      cat("dfp ",i," has nrows = ",nrow(dfp),"\n",sep="");
      print(dfp);
      print(names(dfp));
      readline(prompt="Press enter to continue...");
    }

    #extract data for remaining factor combination levels from unstackCols
    nfc<-nrow(faclevs);
    if (nfc>1) {
      for (i in 2:nfc) {
        if (debug) cat("Unstacking data for fator level combination ",i,"\n",sep="");
        mn<-paste(names(faclevs)[1],"=",faclevs[i,1],sep="");
        subst<-(dfr[,names(faclevs)[1]]==faclevs[i,1]);
        if (ncol(faclevs)>1) {
            nc<-ncol(faclevs);
            for (c in 2:nc) {
                mn<-paste(mn,"_",names(faclevs)[c],"=",faclevs[i,c],sep="");
                subst<-subst & (dfr[,names(faclevs)[c]]==faclevs[i,c]);
            }
        }
        dfpr<-subset(dfr,subst,unstackCols);#subset dataframe for current factors
        nmn<-paste(unstackCols,mn,sep="_");#name for column
        names(dfpr)<-nmn;
        if (debug) print(dfpr);
        #append column to dfp
        dfp<-data.frame(dfp,dfpr,stringsAsFactors=FALSE);
        if (debug){
          print(mn);
          cat("dfp ",i," has nrows = ",nrow(dfp),"\n",sep="");
          print(dfp);
          print(names(dfp));
          readline(prompt="Press enter to continue...");
        }
     }
    }
    return(dfp);
}