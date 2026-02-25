#'
#'@title Concatenate csv files into one
#'
#'@description Function to concatenate a number of csv files (with identical column structure)
#'into a single csv file.
#'
#'@param caption - caption for file dialog box
#'@param out.csv - filename of output csv file
#'@param out.dir - directory for output csv file
#'@param method - specifies method used for concatenation ('writeLines' or 'write.table')
#'
#'@details None.
#'
#'@export
#'
concatenateCSVs<-function(caption="Select csv files",
                          out.csv='test.csv',
                          out.dir=NULL,
                          method='writeLines'){
    fn = selectFile(ext="csv",caption=caption);
    if (!is.null(fn)){
        pth = dirname(fn);
        fns = fn;
        while(!is.null(fn)){
            fn = selectFile(ext="csv",caption=caption,path=pth);
            if (!is.null(fn)) {
                pth = dirname(fn);
                fns = c(fns,fn);
            }
        }
        in.csvs = fns;
    } else {
        return(invisible(NULL));
    }
    
    if (is.null(out.dir)) {
        out.dir<-dirname(file.path('.'));
        if (!is.null(in.csvs[1])) {out.dir<-dirname(file.path(in.csvs[1]));}
    }
    
    out.csv<-file.path(out.dir,out.csv);#full file path
    
    if (method=='writeLines'){
        inclHdr<-TRUE;
        for (in.csv in in.csvs){
            cat("Processing",in.csv,"\n")
            if (inclHdr){
                out<-file(out.csv,open='w')#write mode
            } else {
                out<-file(out.csv,open='a');#append mode
            }
            conn<-file(in.csv,open='r');
            lines<-readLines(con=conn);
            nl<-length(lines);
            if (inclHdr){
                inclHdr<-FALSE;
                writeLines(lines,con=out);
            } else {
                writeLines(lines[2:nl],con=out);
            }
            close(conn);
            close(out);
        }
    } else {
        #use write.csv method
        inclHdr<-TRUE;
        for (in.csv in in.csvs){
            cat("Processing",in.csv,"\n")
            tbl<-getCSV(csvfile=in.csv);
            if (inclHdr){
                inclHdr<-FALSE;
                utils::write.table(tbl,file=out.csv,sep=",",quote=FALSE,col.names=TRUE,row.names=FALSE,append=FALSE)
            } else {
                utils::write.table(tbl,file=out.csv,sep=",",quote=FALSE,col.names=FALSE,row.names=FALSE,append=TRUE)
            }
        }
    }
}
