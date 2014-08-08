#'
#'@title Convert character vector of dates to numeric vector of months.
#'
#'@title Function to convert a character vector of dates to a numeric vector of months.
#'
#'@param x - the character vector to convert
#'@param format - the date format (e.g. 'MM/DD/YYYY','DD-MON-YYYY') 
#'
#'@return numeric vector of months (1-12)
#'
#'@export
#'
parseMonths<-function(x,format='MM/DD/YYYY'){
    r<-vector('numeric',length=length(x));
    if (toupper(format)=='MM/DD/YYYY'){
        sep<-'/';
        xs<-strsplit(x,sep,fixed=TRUE);
        for (i in 1:length(r)) {r[i]<-as.numeric(xs[[i]][1]);}
    } else
    if (any(toupper(format)==c('DD-MON-YYYY','DD-MON-YY'))){
        sep<-'-';
        xs<-strsplit(x,sep,fixed=TRUE);
        MONTH.ABB<-toupper(month.abb);
        for (i in 1:length(r)) {r[i]<-which(toupper(xs[[i]][2])==MONTH.abb)[1];}
    }    
    return(r)
}