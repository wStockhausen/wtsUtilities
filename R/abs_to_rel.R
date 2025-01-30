#' 
#' @title Convert absolute path to relative one
#' @description Function to convert an absolute path to relative one. 
#' @param path - path to convert to relative to base
#' @param base - base folder for relative path
#' @param mustWork - logical: if TRUE then an error is given if the 
#' result cannot be determined; if NA then a warning.
#' 
#' @return relative path as string
#' 
#' @details See [normalizePath] for more information on \code{mustWork}. The code 
#' for this function is essentially a copy `abs2rel` from https://github.com/ranghetti/sen2r.
#' 
#' @export
#' 
abs_to_rel<-function (path, base, mustWork = NA) {
    . <- NULL
    #path <- expand_path(path, silent = NA) #--function in sen2r package
    path = path.expand(path);
    path <- normalizePath(path, winslash = "/", mustWork = mustWork)
    base <- normalizePath(base, winslash = "/", mustWork = mustWork)
    if (path == base) {
        print_message(type = "warning", "Input (", path, ") and reference (", 
            base, ") point to the same path.")
        return(".")
    }
    com_path <- gsub("/$", "", comsub(c(path, base), sep = "/"))
    if (com_path == "") {
        print_message(type = "warning", "Input (", path, ") and reference (", 
            base, ") paths do not have a common parent directory; ", 
            "an absolute path is returned.")
        return(path)
    }
    diff_ref <- gsub("^/", "", gsub(paste0("^", com_path), "", 
        base))
    diff_in <- gsub("^/", "", gsub(paste0("^", com_path), "", 
        path))
    n_ref <- length(strsplit(diff_ref, "/")[[1]])
    out_prefix <- if (n_ref == 0) {
        "."
    }
    else {
        paste(rep("..", n_ref), collapse = "/")
    }
    rel_path <- file.path(out_prefix, diff_in)
    return(rel_path)
}

#--from sen2r package
comsub<-function (data, sep = "") 
{
    . <- NULL
    data_spl <- strsplit(data, sep)
    data_spl_maxlength <- max(sapply(strsplit(data, sep), length))
    which_max <- if (length(unique(data)) > 1) {
        which.max(apply(do.call(rbind, lapply(data_spl, `length<-`, 
            data_spl_maxlength)), 2, function(i) {
            !length(unique(i)) == 1
        })) - 1
    }
    else {
        length(data_spl[[1]]) - 1
    }
    paste(c(data_spl[[1]][seq_len(which_max)], ""), collapse = sep)
}
