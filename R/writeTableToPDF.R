#' 
#' @title Create a pdf file from latex from a tabular object.
#' 
#' @description Function to create a pdf file from latex from a tabular object.
#' 
#' @param tbl_tex - the tex string from tables::toLatex(tabular)
#' @param template_tex_str - character string of tex template
#' @param pdf_name - the desired name for the resulting pdf file
#' @param insertion_point - string identifying point at which to insert the tex string into the template
#' @param pageheight - page height (number, in units given by \code{units})
#' @param  pagewidth - page width (number, in units given by \code{units})
#' @param  margin - margin (number, in units given by \code{units})
#' @param orientation - "portrait" or "landscape"
#' @param units - units for measurements (e.g., "in" or "cm")
#' @param cleanup - flag to clean up intermediate files
#' 
#' @return Invisibly, the tex string for the pdf document.
#' 
#' @details This function inserts the latex string \code{tbl_tex} returned by [tables::toLatex(tbl)] called on
#' a table \code{tbl}into a tex template (\code{tex_template_str}) for a document. It then converts the resulting tex to a pdf using
#' [writeTexToPDF()].
#' 
#' The insertion point in the template is indicated by the value of \code{insertion_point}. A default template is provided in
#' \code{tex/templateForTable.tex}.
#' 
#' The intermediate files are named "tex_DUMMY_TEX_FILE" with extensions .tex, .aux, .log, and .pdf. The pdf file
#' is copied to the file given by pdf_name (which defaults to 'table_str.pdf'). 
#' 
#' @note The use of orientation="landscape" reverses the sense of pageheight and pagewidth in determining the page sie,
#' but does not rotate the table.
#' 
#' @export
#' 
writeTableToPDF<-function(tbl_tex,
                          template_tex=system.file("tex/templateForTable.tex",package="wtsUtilities"),
                          pdf_name="table.pdf",
                          insertion_point="&&tex_str",
                          pageheight=8.5,
                          pagewidth=6.5,
                          margin=0.1,
                          orientation="portrait",
                          units="in",
                          cleanup=FALSE){
  #--insert tbl_tex into template
  tex_str = gsub(insertion_point,tbl_tex,template_tex,fixed=TRUE);
  
  #--adjust paper size, margins, and orientation
  geom_str = paste0("\\geometry{reset,",
                    "papersize={",pagewidth,units,",",pageheight,units,"},",
                    "margin=",margin,units,",",
                    "hcentering=true,",
                    orientation,
                    "}\n\n \\begin{document}\n\n"
                    )
  tex_str = gsub("\\begin{document}",geom_str,tex_str,fixed=TRUE);
  
  #--create pdf
  writeTexToPDF(template_tex_str=tex_str,pdf_name=pdf_name,cleanup=cleanup);
  return(invisible(tex_str));
}