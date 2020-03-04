#'
#' @title Check a value (type) against a vector of valid types
#'
#' @description Function to check type against vector of valid types.
#'
#' @param type - string to check
#' @param valid_types - vector of valid types
#'
#' @return TRUE if the value of \code{type} is a valid type, FALSE otherwise.
#' 
#' @details If FALSE, a warning message is generated. 
#' It is the responsibility of the caller to determine what an
#' appropriate response is.
#'
#' @export
#'
checkValidType<-function(type,
                         valid_types){
  name <-deparse(substitute(type));
  msg<-paste0(name," is invalid. Got '",type,"' but expected one of: ",paste0("'",valid_types,"'",collapse=","));
  if (any(tolower(type)==valid_types)) return(TRUE);
  warning(msg,"\n")
  return(FALSE);
}