#' @title Find the unique combination of levels of a set of "factors" in a dataframe
#' 
#' @description  Returns the unique combination of levels
#'   of the specified "factors" in a dataframe.
#'   
#' @param dfr        - input dataframe (or object that can be cast to a dataframe)
#' @param factors    - vector of desired factor names from dfr
#' @param sortBy     - vector of factor names to sort by (in order of precedence)
#' @param ascending  - flag (T/F) to sort in ascending/descending order
#' @param orthogonal - flag (T/F) to form orthogonal combination of individual factors
#' @param debug      - flag (T/F) to print debugging info
#' 
#' @return a dataframe containing each unique combination of factor levels in dfr
#'
#'@export
#'
getFactorLevels<-function(dfr,factors,sortBy=NULL,ascending=TRUE,orthogonal=FALSE,debug=FALSE){
    if (debug) cat("Entered getFactorLevels\n")
    #coerce dfr to dataframe, if necessary
    if (is.vector(dfr)) {
        if (debug) cat("coercing input vector to dataframe\n");
        expr<-parse(text=paste("dfr<-as.data.frame(list(",factors[1],"=dfr));"));
        if (debug) print(expr);
        eval(expr);
    }
    if (!is.data.frame(dfr)) dfr<-as.data.frame(dfr);

    if (debug) {
        print(dfr)
        print(factors)
        print(dfr[,factors])
    }

    u<-NULL;
    if (orthogonal) {
        #find unique levels for each factor, then cross them
        nf<-length(factors);
        nu<-vector("integer",length=length(factors));#vector of number of unique levels for each factor
        uf<-list();                                #list of unique levels for each factor
        for (f in 1:nf) {
            uf[[factors[f]]]<-unique(dfr[,factors[f]]);#unique levels of fth factor
            nu[f]<-length(uf[[factors[f]]]);#number of unique levels of fth factor
        }
        tnu<-prod(nu);#total number of orthogonal factor combinations
        u<-vector("list",length=nf);
        names(u)<-factors;
        for (f in 1:nf) {
            vf<-uf[[factors[f]]];
            if (f<nf){
                #"expand" vector
                nsp<-prod(nu[(f+1):nf]);
                vf<-rep(vf,,,nsp);
            }
            if (f>1){
                #"stack" vector
                nsp<-prod(nu[1:(f-1)]);
                vf<-rep(vf,nsp);
            }
            u[[factors[f]]]<-vf;
        }
        u<-as.data.frame(u);
        if (debug) {
            cat("orthogonal factor combinations:\n");
            print(u);
        }
    } else {
        #find unique factor level combinations in dfr
        if (debug) cat("Extracted factors form a vector. Coercing vector to dataframe\n");
        u<-unique(dfr[,factors]);
        if (!is.data.frame(u)) {
            expr<-parse(text=paste("u<-as.data.frame(list(",factors[1],"=u));"));
            if (debug)  print(expr);
            eval(expr);
        }
    }

    if (!is.data.frame(u)) {
        cat("factor level combinations (u) for factors:",factors,"is invalid!!\n");
        print(mode(u));
        print(u);
        stop("Exiting getFactorLevels!!!\n")
    }

    #sort levels, if required
    if (!is.null(sortBy)) u<-sortBy(u,sortBy,ascending=ascending,debug=debug);
    if (debug) {
        print(mode(u));
        print(u);
    }

    #reassign row names
    row.names(u)<-seq(dim(u)[1]);#reassign row names to 1:nrows

    if (debug) cat("Exiting getFactorLevels\n")
    return(u)
}