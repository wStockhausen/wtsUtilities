#'
#'@title Concatenate csv files into one.
#'
#'@description Function to concatenate a number of csv files (with identical column strucuture)
#'into a single csv file.
#'
#'@param caption - caption for file dialog box
#'@param out.csv - filename of output csv file
#'@param out.dir - directory for output csv file
#'
#'@export
#'
concatenateCSVs<-function(caption="Select csv files",
                          out.csv='test.csv',
                          out.dir=NULL){
    Filters<-addFilter("csv","csv files (*.csv)","*.csv");
    in.csvs<-tk_choose.files(caption=caption,
                             multi=TRUE,filters=matrix(Filters[c("csv"),],1,2,byrow=TRUE));
    if (is.null(out.dir)) {
        out.dir<-dirname(file.path('.'));
        if (!is.null(in.csvs[1])) {out.dir<-dirname(file.path(in.csvs[1]));}
    }
    
    ctr<-0;
    for (in.csv in in.csvs){
        dfrp<-getCSV(csvfile=in.csv);
        if (ctr==0){
            dfr<-dfrp;
        } else {
            dfr<-rbind(dfr,dfrp)
        }
        ctr<-ctr+1;
    }
    write.csv(dfr,file=file.path(out.dir,out.csv),row.names=FALSE,quote=FALSE,na='')
}
