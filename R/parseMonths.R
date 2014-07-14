#'
#'@title Convert character vector of dates to numeric vector of months.
#'
#'@title Function to convert a character vector of dates to a numeric vector of months.
#'
#'@param x - the character vector to convert
#'@param format - the date format (e.g. 'MM/DD/YYYY') 
#'@param sep - the date-month-yar divider (e.g. '/')
#'
#'@return numeric vector of months (1-12)
#'
#'@export
#'
parseMonths<-function(x,format='MM/DD/YYYY',sep='/'){
    frmt<-strsplit(toupper(format),sep,fixed=TRUE)
    ctr<-0;
    for (f in frmt[[1]]){
        ctr<-ctr+1;
        if (f=='MM') {break;}
    }
    
    r<-vector('numeric',length=length(x));
    xs<-strsplit(x,sep,fixed=TRUE);
    for (i in 1:length(r)) {r[i]<-as.numeric(xs[[i]][ctr]);}
    
    return(r)
}