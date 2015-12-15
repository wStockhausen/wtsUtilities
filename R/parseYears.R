#'
#'@title Convert character vector of dates to numeric vector of days-of-month.
#'
#'@description Function to convert a character vector of dates to a numeric vector of days-of-month.
#'
#'@param x - the character vector to convert
#'@param format - the date format (e.g. 'MM/DD/YYYY','DD-MON-YYYY) 
#'
#'@return numeric vector of months (1-12)
#'
#'@export
#'
parseYears<-function(x,format='MM/DD/YYYY'){
    add<-FALSE;
    r<-vector('numeric',length=length(x));
    if (toupper(format)=='MM/DD/YYYY'){
        sep<-'/';
        xs<-strsplit(x,sep,fixed=TRUE);
        for (i in 1:length(r)) {r[i]<-as.numeric(xs[[i]][3]);}
    } else
    if (any(toupper(format)==c('DD-MON-YYYY','DD-MON-YY'))){
        sep<-'-';
        fs<-strsplit(toupper(format),sep,fixed=TRUE);
        if (fs[[1]][3]=='YY') add<-TRUE;
        xs<-strsplit(x,sep,fixed=TRUE);
        MONTH.ABB<-toupper(month.abb);
        for (i in 1:length(r)) {r[i]<-r[i]<-as.numeric(xs[[i]][3]);}
    }    
    
    if (add) r<-(1900+r)*(r>=50) + (2000+r)*(r<50);
    
    return(r)
}