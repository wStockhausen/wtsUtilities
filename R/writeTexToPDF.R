#' 
#' @title Create a pdf file from a tex string and tex template.
#' 
#' @description Function to create a pdf file from tex.
#' 
#' @param template_tex_str - character string of tex template
#' @param template_tex_file - name of text file with tex template
#' @param tex_str - the tex string to insert into the template
#' @param tex_file - the name of a tex file with the tex string
#' @param insertion_point - string identifying point at which to insert the tex string into the template
#' @param pdf_name - the desired name for the resulting pdf file
#' @param cleanup - flag to clean up intermediate files
#' 
#' @return Invisibly, the tex string used to create the pdf document.
#' 
#' @details This function inserts a tex string (in tex_str or tex_file) into a string version of a tex template
#' (template_tex_str) or file (template_tex_file) and then converts the resulting tex file to a pdf. The insertion 
#' point in the template must be indicated by the value of \code{insertion_point}.
#' 
#' To use a 'template' without a '&&tex_str' insertion marker, leave tex_str and tex_file as NULLs.
#' 
#' The intermediate files are named "tex_DUMMY_TEX_FILE" with extensions .tex, .aux, .log, and .pdf. The pdf file
#' is copied to the file given by pdf_name (which defaults to the tex_file name with a pdf extension, or 'tex_str.pdf' if
#' tex_file is NULL).
#' 
#' @import stringr
#' 
#' @export
#' 
writeTexToPDF<-function(template_tex_str=NULL,
                        template_tex_file=NULL,
                        tex_str=NULL,
                        tex_file=NULL,
                        insertion_point="&&tex_str",
                        pdf_name=NULL,
                        cleanup=TRUE){
  if (is.null(template_tex_str)){
    #--read file to get template tex string
    if (!file.exists(template_tex_file)) stop(paste0("could not find tex template '",template_tex_file,"'."));
    template_tex_str = paste0(readLines(template_tex_file),collapse="\n");
  }
  if (is.null(tex_str)){
      tex_str = "";
      if (!is.null(tex_file)){
        #--read file to get the table tex string
        if (!file.exists(tex_file)) stop(paste0("could not find tex_file '",tex_file,"'."));
        tex_str = paste0(readLines(tex_file),collapse="\n");
      }
  }
  
  #--insert tex_str into the tex template
  str = gsub(insertion_point,tex_str,template_tex_str,fixed=TRUE)
  #--write to dummy tex file
  cat(str,file="tex_DUMMY_TEX_FILE.tex");
  
  #--create the pdf
  system2("pdflatex",args="tex_DUMMY_TEX_FILE.tex",wait=TRUE,timeout=20);
  
  #--copy tex_DUMMY_TEX_FILE.pdf to pdf_name
  if (is.null(pdf_name)) {
    if (!is.null(tex_file)) {
      pdf_name = tex_file;
      if (stringr::str_ends(tex_file,stringr::fixed(".tex"))) pdf_name = substr(tex_file,1,nchar(tex_file)-4);
      pdf_name = paste0(pdf_name,".pdf");
    } else {
      pdf_name = "tex_str.pdf";
    }
  }
  file.copy("tex_DUMMY_TEX_FILE.pdf",pdf_name,overwrite = TRUE,recursive=FALSE);
  
  #--clean up files
  if (cleanup){
    lst = list.files(pattern=glob2rx("tex_DUMMY_TEX_FILE.*"))
    #lst = lst[!stringr::str_ends(lst,stringr::fixed(".pdf"))];
    file.remove(lst);
  }
  
  return(invisible(str))
}
