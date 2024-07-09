#' 
#' @title Create a set of formulas from combinations of terms in one
#' @description Function to create a set formulas from combinations of terms.
#' @param frmla - base formula to expand
#' @param keep - NULL, or character vector of terms which must be included in final set
#' @return list of formulas 
#' @details If `keep` is NULL (the default), then the list of returned formulas includes all 
#' possible combinations of terms. If `keep` is not NULL, then the returned list 
#' only includes terms involving those specified in `keep`.
#' @importFrom stringr str_detect
#' @importFrom stringr fixed
#' @export
#' 
createFormulas<-function(frmla,keep=NULL){
  trms = terms(frmla);
  lbls = attr(trms,"term.labels");
  resp = rownames(attr(trms,"factors"))[1];
  n    = length(lbls);
  m = lower.tri(matrix(0,nrow=n,ncol=n),diag=TRUE)
  fs = list(); nf=0; np = 1;
  while (np<=n){
    for (r in 1:n){
      txt = paste0(lbls[m[r,]],collapse="+");
      if ((txt!="")){
        if (is.null(keep)){
          fs[[nf<-nf+1]] = formula(paste(resp,"~",txt));
        } else {
          if (all(stringr::str_detect(lbls[m[r,]],stringr::fixed(keep))))
            fs[[nf<-nf+1]] = formula(paste(resp,"~",txt));
        }
      }
    }
    m[,np] = FALSE;
    np = np+1;
  }
  return(fs);
}
