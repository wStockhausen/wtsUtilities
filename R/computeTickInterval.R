#'
#'@title Calculate nice tick intervals for a scale.
#'
#'@description Function to calculate nice tick intervals for a scale
#'based on a range and the desired number of ticks.
#'
#'@param rng - the nominal range for the scale \[i.e., c(min,max)\]
#'@param nticks - the desired number of ticks
#'
#'@details None.
#'
#'@export
#'
computeTickInterval<-function(rng,nticks=5) {
  #type 1
  td<-floor(rng/nticks);
  t10<-10^(floor(log(td)/log(10)));
  ti<-floor(td/t10)*t10;
  if (ti/t10<1.8) {
    ti<-1*t10;
  } else if (ti/t10<3) {
    ti<-2*t10;
  } else {
    ti<-5*t10
  }
  
  #type 2
  del<-10^(floor(log(rng)/log(10)));
  ti<-floor(rng/del)*del;
  return(ti);
}