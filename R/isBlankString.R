#'
#' @title Test if character string is only whitespace (i.e., blank)
#'
#' @description Function to test if character string is only whitespace (i.e., blank).
#'
#' @param str - character string to test
#'
#' @return TRUE or FALSE. 
#'
#' @details Returns TRUE if \code{stringr::str_squish(str)} reduces \code{str} to "", otherwise FALSE. 
#' Also returns FALSE if \code{str} is not a length 1 character vector.
#'
#' @export
#'
isBlankString<-function(str){
    return((length(str)==1)&&(stringr::str_squish(str)==""));
}
