#'
#' @title Collect values in a dataframe by implicit group
#' @description Function to collect values from a column in a dataframe into a list vector by group.
#' @param dfr0 - dataframe on which to operate
#' @param collect - name(s) of columns from which to collect values
#' @param names_from - column name with names to pivot wider (optional)
#' @param values_from - column name with values to pivot wider (optional)
#' @details This function optionally pivots an input dataframe wider using [tidyr::pivot_wider()]
#' with `names_from` and `values_from`. It then selects only the distinct rows in the resulting dataframe
#' after the column(s) identified by `collect` are dropped, and assigns a `group` number to each distinct row.
#' The values in the `collect` column(s) of the input dataframe are then identified by group and "collected"
#' by group membership into a vector. The list of "collected" vectors is then appended to the "distinct" dataframe.
#'
#' @import dplyr
#' @import rlang
#' @import tidyr
#' @import tidyselect
#' 
#' @export
#'
collectValuesByGroup<-function(dfr0,collect="y",names_from="z",values_from="val"){
  cols = names(dfr0);
  #--pivot wider, if requested
  if (!is.null(names_from)){
    dfr1 = dfr0 |>
             tidyr::pivot_wider(names_from=names_from,values_from=values_from);
  } else {dfr1 = dfr0;}
  dfr2 = dfr1 |> dplyr::distinct(dfr1 |> dplyr::select(!tidyselect::all_of(collect))) |>
           dplyr::group_by(!!!rlang::syms(cols[!(cols %in% c(collect,names_from,values_from))])) |>
           dplyr::mutate(group=dplyr::row_number(),.before=1) |>
           dplyr::ungroup();
  ;
  dfr3 = dfr2 |> dplyr::inner_join(dfr1) |> dplyr::select(tidyselect::any_of(c("group",cols[!(cols %in% c(names_from,values_from))])));
  ex<-function(d,c){vec=list(d[[c]]);
                    dp = d |> dplyr::distinct(!!!rlang::syms(names(d)[!(names(d) %in% c)]));
                    dp[[c]]=vec;
                    dp = dp |> dplyr::distinct(!!!rlang::syms(names(d)));
                    return(dp)}
  dfr4 = dfr3 |> dplyr::group_by(!!!rlang::syms(c("group",cols[!(cols %in% c(collect,names_from,values_from))]))) |>
           dplyr::group_modify(~ex(.x,collect)) |>
           dplyr::ungroup();
  dfr5 = dfr4 |> dplyr::inner_join(dfr2,by=names(dfr4)[!(names(dfr4) %in% collect)]);
  return(dfr5);
}
if (FALSE){
  tmp =  dfrSel |> dplyr::filter(fleet %in% "TCF",type=="retained");
  cols = names(tmp)[names(tmp) %in% c("case","y","x","m","s","z","val")];
  tmp0 = tmp |> dplyr::select(tidyselect::all_of(cols));
  tmp1 = collectValuesByGroup(tmp0,collect="y",names_from="z",values_from="val");
  tmp1 = tmp1 |> dplyr::rowwise() |> dplyr::mutate(ylab=collapseIntegersToString(y),.before=y);
}

#'
#' @title Collapse a vector of integers to a more-compact string representation
#' @description Function to collapse a vector of integers to a more-compact string representation.
#' @param v - vector of integers (or list consisting of a single vector of integers)
#' @return a string representation of the vector
#' @details If `v` is equal to c(x:y,z,...), where `x`, `y`, and `z` are integers, and `...` is constructed of integers,
#' then the returned string from `collapseIntegersToString(v)` would be x:y,z,..., where
#' `x`, `y`, `z` and `...` are now character equivalents to the original values.
#'
#' NOTE: this function is not vectorized. To use it with a tibble list column, use [dplyr::rowwise()].
#'
#' @examples
#' # example code
#' v = c(1:3,10,21:24,5);
#' s = collapseIntegersToString(v);
#'
#' Also, `v` can be recovered by
#' v = eval(parse(text=paste0("c(",s,")")));
#'
#' lv = dplyr::bind_rows(tibble::tibble(v=list(v)), tibble::tibble(v=list(c(v,5))));
#' lv = lv |> dplyr::rowwise() |> dplyr::mutate(s=collapseIntegersToString(v),.before=1);
#'
#' @export
#'
collapseIntegersToString<-function(v){
  if (is.list(v)) v = unlist(v);
  i = 1;
  s = paste(v[i]);
  lv = length(v);
  if (lv>1){
    sp="";
    while (i < (lv-1)){
      if ((v[i+1]-v[i])==1) {
        sp = paste0(":",v[i+1]);
        i = i+1;
      } else {
        s = paste0(s,sp,",",v[i+1]);
        i=i+1;
        sp="";
      }
    }#--while
    if ((v[lv]-v[lv-1])==1) {
      s = paste0(s,":",v[lv]);
    } else {
      s = paste0(s,sp)
      s = paste0(s,",",v[lv]);
    }
  }
  return(s)
}
#if (FALSE){
  # v = c(1:3,10,21:24,5);
  # collapseIntegersToString(v);
  # v = c(1:3,10,22,21:24);
  # collapseIntegersToString(v);
#' }
