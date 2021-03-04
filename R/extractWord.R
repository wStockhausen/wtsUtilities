#' 
#' @title Extract a vector of the n-th word by location from a vector of character strings
#' 
#' @description Function to extract a vector of the n-th word by location from a vector of character strings.
#' 
#' @param x - a vector of character strings
#' @param which - integer indicating which word, by position, to extract
#' @param split - character string by which to define words
#' 
#' @return a vector of words extracted bby location from x (of length(x))
#' 
#' @details Uses \code{strsplit(x,split,fixed=TRUE)} to split each character 
#' string in x into a list of vectors of 'words', then extracts the n-th word 
#' from each list element (where n is the input value of \code{which}). If a 
#' list element has less than \code{which} words, the associated value in the 
#' returned vector will be "".
#'
#'@export
#'
extractWord<-function(x,which=1,split=" "){
  lst = strsplit(x,split,fixed=TRUE);
  n = length(lst);
  w = vector("character",length=n);
  for (i in 1:n) w[i] = ifelse(length(lst[[i]]>=which),lst[[i]][which],"");
  return(w);
}
