#'
#'@title Rebin a column in dataframe
#'
#'@description Rebin a column in a dataframe based on a vector of cutpoints
#'
#'@param tbl - dataframe to rebin
#'@param column - name of column to rebin
#'@param cutpts - left cutpts for the rebinning operation
#'@param binType - output type for values ('L','C',or 'R' for left, center or right bin values)
#'
#'@return list with rebinned dataframe ('tbl'), bins ('bins'), and cutpts ('cutpts') as elements
#'
#'@export
#'
rebinColumn<-function(tbl,                     #input dataframe
                      column='LENGTH',         #column to re-bin
                      cutpts=1:10,             #left cutpts for rebinning, 
                      binType=c('L','C','R')   #output bin type
                      ){
  #truncate table
  tbl<-tbl[tbl[[column]]>=cutpts[1],];
  nTbl<-nrow(tbl);
  
  #determine bins
  nBins<-length(cutpts-1);
  if (toupper(binType[1]=='L')){ #left cutpts
    bins<-cutpts[1:nBins];
  } else if (toupper(binType[1]=='C')){ #bin centers
    bins<-0.5*(cutpts[1:nBins]+cutpts[2:(nBins+1)]);
    if (!is.finite(bins[nBins])){bins[nBins]<-cutpts[nBins]+0.5*(cutpts[nBins]-cutpts[nBins-1])}
  } else if (toupper(binType[1]=='R')){ #right cutpts
    bins<-cutpts[(1:nBins)+1];
    if (!is.finite(bins[nBins])){bins[nBins]<-cutpts[nBins]+1.0*(cutpts[nBins]-cutpts[nBins-1])}
  }
  
  #arrange cutpts into a matrix with bin extrema as values
  cut.pts<-c(cutpts[1:nBins],cutpts[1+(1:nBins)]);
  dim(cut.pts)<-c(nBins,2);
  #print(cut.pts)
  
  #rebin the table
  for (j in 1:nTbl) {
      for (i in 1:nBins) {
          if ((tbl[j,column]>=cut.pts[i,1])&&(tbl[j,column]<cut.pts[i,2])) {
              #cat(tbl[j,column],cutpts[i,1],'\n');
              tbl[j,column]<-bins[i];
          }
      }
  }
  return(list(tbl=tbl,bins=bins,cutpts=cutpts));
}
