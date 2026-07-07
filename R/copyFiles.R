#' @title Copy new or updated files/folders 
#' @description Function to copy new or updated files and folders from one 
#' location to another. 
#' @param dirFrom - path to folder to copy files/folders from
#' @param dirTo - path to folder to copy files/folders to
#' @return tibble of new or updated files/folder paths in the "to" folder
#' @details File/folder lists are recursively compiled into tibbles for both "from" and "to" 
#' folders and the resulting "from" tibble is left-joined to the "to" tibble based on the 
#' folder/file names under the base folders. The file modification times are checked for 
#' files that appear in both file structures and new files or files with with later modification times in the 
#' "from" folders are copied into their respective "to" folders. Folders in "from" but missing 
#' in "to" are created in "to" and any files are copied into the new folders in "to". 
#' 
#' If `dirFrom` or `dirTo` is NULL (the default for both), a browser is opened allowing the user to select 
#' the folder by selecting **one of the files** it contains.
#' 
#' @export
#' 
copyFiles<-function(dirFrom=NULL,dirTo=NULL){
    if (is.null(dirFrom))
      dirFrom = dirname(file.choose());
    if (is.null(dirTo))
      dirTo = dirname(file.choose());
    
    #--get file lists for from folders
    tblFrom = fs::dir_info(dirFrom,recurse=TRUE,fail=FALSE) |> 
                dplyr::select(path,mt=modification_time) |> 
                dplyr::mutate(fp=stringr::str_remove(path,stringr::fixed(dirFrom)));
    tblTo = fs::dir_info(dirTo,recurse=TRUE,fail=FALSE) |> 
                dplyr::select(path,mt=modification_time) |> 
                dplyr::mutate(fp=stringr::str_remove(path,stringr::fixed(dirTo)));
    tbl = tblFrom |> dplyr::left_join(tblTo,by="fp") |> 
                dplyr::filter((mt.x-mt.y>1)|is.na(mt.y));
    
    #--copy files
    for (rw in 1:nrow(tbl)){
      #--rw = 1;
      fp = tbl$fp[rw];
      fpt = paste0(dirTo,fp);
      if (fs::is_file(tbl$path.x[rw])){
        fs::file_copy(tbl$path.x[rw],fpt,TRUE);
        tbl$path.y[rw] = fpt;
      } else if (fs::is_dir(tbl$path.x[rw])&&!fs::dir_exists(fpt)){
        fs::dir_create(fpt);
        tbl$path.y[rw] = fpt;
      }
    }
    return(tbl$path.y); #--updated folders/files
}