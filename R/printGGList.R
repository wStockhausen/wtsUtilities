#'
#'@title Print a ggplot2 object, or list of ggplot2 objects
#'
#'@description Function to print a ggplot2 object or a list of ggplot2 objects.
#'
#'@param plots - a ggplot2 object or a list of ggplot2 objects
#'@param figno - figure number (or NULL)
#'@param cap - plot caption
#'@param show - flag (T/F) to actually print plots
#'
#'@return list with elements:
#'\itemize{
#'  \item{figno - figure number for next plot, if figno was not NULL}
#'  \item{plotlist - non-nested list of plots, with numbered captions as names}
#'} 
#'
#'@details Captions for each figure are taken from the name of the list element associated with the plot.
#'\code{figno} is substituted for '&&fno' or '&&figno' in the caption and incremented for the next plot.
#'
#'@import ggplot2
#'
#'@export
#
printGGList<-function(plots,figno=NULL,cap=NULL,show=TRUE){
    plotlist<-list();
    if (inherits(plots,"ggplot")){
        #plots is a ggplot object
        if (!is.null(figno)) {
            cap<-gsub("&&fno",figno,cap);
            cap<-gsub("&&figno",figno,cap);
            figno<-figno+1;
        }
        if (show) {
            print(plots);
            cat(cap,"\n",sep=''); 
        }
        plotlist[[cap]]<-plots;
        return(list(figno=figno,plotlist=plotlist));
    } else {
        #plots is a list
        caps<-names(plots);
        for (p in 1:length(plots)){
            plot<-plots[[p]];
            res<-printGGList(plot,figno=figno,cap=caps[p],show=show);
            figno<-res$figno;
            plotlist<-c(plotlist,res$plotlist);
        }
    }
    return(list(figno=figno,plotlist=plotlist));
}
