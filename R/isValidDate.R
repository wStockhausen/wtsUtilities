#' 
#' @title Test if object is a valid date
#' @description
#' Function to test if an object is a valid date.
#' @param dt - the object to test 
#' @return logical 
#' @details if is.na(as.Date(dt)) does not throw an error and is TRUE, 
#' then the function returns TRUE; otherwise, it returns FALSE.
#' 
#' Based on discussion at https://gist.github.com/micstr/69a64fbd0f5635094a53.
#' 
#' @examples
#' isValidDate("2023-02-28");  #--expect TRUE
#' 
#' isValidDate("2023-02-30"); #--expect FALSE
#' 
#' @export
#' 
isValidDate<-function(dt){
  if (is.na(dt))   return(FALSE);
  if (is.null(dt)) return(FALSE);
  tryCatch(
    !is.na(as.Date(dt)),
    error=function(err){FALSE}
  )
}
