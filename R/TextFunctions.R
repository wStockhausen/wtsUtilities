#'
#' @title Strip comments from elements of a character vector
#'
#' @description Function to strip comments from elements of a character vector.
#'
#' @param strv - character vector to strip comments from
#' @param comment - character indicating remaining text is a comment
#'
#' @return character vector with comments removed. 
#'
#' @details The returned character vector will not have the same number of elements
#'as the original if the latter had elements that were only comments or 'blank' elements.
#'Thus, one use of this function could be to remove blank lines, comment-only lines, and
#'commented text from a character vector created by reading a text file using readLines().
#'
#' @export
#'
stripComments<-function(strv,comment='#'){
    iv<-regexpr(paste('[',comment,']',sep=''),strv,fixed=FALSE)
    idx<-!(iv==1);#lines NOT starting with a comment
    ivp<-iv[idx];
    strvp<-strv[idx];
    for (i in 1:length(ivp)){
        if (ivp[i]>1) strvp[i]<-substr(strvp[i],1,ivp[i]-1);
    }
    idx<-!(gsub(' ','',strvp,fixed=TRUE)=='');#find white space-only lines
    return(strvp[idx])
}#stripComments

#'
#'@title Parse numeric expressions from a character vector.
#'
#'@description Function to parse numeric expressions from a character vector.
#'
#'@param str - character vector to parse
#'
#'@return parsed and evaluated numerical expressions as a vector
#'
#'@details None.
#'
#'@export
#'
parseNum<-function(str){
    n<-length(str);
    res<-list();
    for (i in 1:n){
        expr<-parse(text=paste('tmp<-',str[i]))
        eval(expr);
        res[[i]]<-tmp;
    }
    return(unlist(res));
}

#'
#'@title Add quotes to a character string vector.
#'
#'@description Function to add quotes to a character string vector.
#'
#'@param str - character vector to add quotes to
#'@param qt - quote character
#'
#'@return character vector w/ elements in quotes
#'
#'@details None.
#'
#'@export
#'
addQuotes<-function(str,qt="'"){
    return(strp<-paste(qt,str,qt,sep=''));
}

#'
#'@title Strip quotes from a character string vector.
#'
#'@description Function to strip quotes from a character string vector.
#'
#'@param str - character vector to strip quotes from
#'@param qt - quote character
#'
#'@return character vector w/ quotes stripped out
#'
#'@details None.
#'
#'@export
#'
stripQuotes<-function(str,qt="'"){
    return(gsub(qt,'',str,fixed=TRUE));
}
