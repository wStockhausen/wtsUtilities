#'
#'@title my version of lapply
#'
#'@description A convenience implementation of lapply
#'
#'@param X - data to apply function to (coerced to list if necessary)
#'@param FUN - function to apply to X
#'@param ... - other inputs to FUN
#'
#'@return result of lapply'ing function to X
#'
#'@export
#'
my.lapply<-function (X, FUN, ...) {
    FUN <- match.fun(FUN)
    if (!is.list(X)) 
        X <- as.list(X)
    rval <- lapply(X, FUN)
    return(rval)
}
