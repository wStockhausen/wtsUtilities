#'
#'@title Compute confidence intervals from cv's or standard deviations
#'
#'@description Function to compute confidence intervals.
#'
#'@param vals - vector of values
#'@param cvs - vector of cvs
#'@param sdvs - vector of arithmetic-scale standard deviations
#'@param pdfType - probability distribution for error bars
#'@param ci - confidence interval for error bar plots
#'@param verbose - flag (T/F) to print intermediate output
#'
#'@return list with vectors lci, uci as  named elements
#'
#'@details None.
#'
#'@export
#'
calcCIs<-function(vals,cvs=NULL,sdvs=NULL,
                  pdfType=c('lognormal','normal'),
                  ci=0.95,verbose=FALSE){
    #compute confidence intervals for survey data
    ci<-c((1-ci)/2,1-(1-ci)/2);#confidence intervals
    obs<-vals;
    cv <-cvs;
    if (tolower(pdfType[1])=='normal'){
        if (verbose) cat('using err type = normal\n')
        if (is.null(sdvs)){
            sdv<-cv*obs;
        } else {sdv <- sdvs;}
        lci<-stats::qnorm(ci[1],mean=obs,sd=sdv);
        uci<-stats::qnorm(ci[2],mean=obs,sd=sdv);
    } else if (tolower(pdfType[1])=='lognormal'){
        if (verbose) cat('using err type = lognormal\n')
        if (!is.null(sdvs)){
            cv <- sdvs/vals;
        }
        sdv<-sqrt(log(1+cv^2));#log-scale std dev
        lci<-stats::qlnorm(ci[1],meanlog=log(obs),sdlog=sdv);
        uci<-stats::qlnorm(ci[2],meanlog=log(obs),sdlog=sdv);
    } else {
        cat('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n')
        cat('Error in wtsUtilities::calcCIs.\n')
        cat("pdfType '",pdfType,"' not recognized!!\n")
        cat("Exiting function.\n")
        cat('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n')
        return(NULL)
    }

    return(list(lci=lci,uci=uci));
}
