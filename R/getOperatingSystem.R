#'
#'@title Get the operating system name.
#'
#'@description Function that returns the operating system name.
#'
#'@return Either 'MacOSX' or 'Windows'
#'
#'@export
#'
getOperatingSystem<-function(){
    #check the operating platform
    MacOSX<-'MacOSX';
    Win<-'Windows';
    plat<-Sys.info()[['sysname']];
#    cat(plat,'\n')
    if (tolower(plat)=='darwin'){
        platform<-MacOSX;
    } else if (tolower(plat)=='windows') {
        platform<-Win;
    }
    return(platform);
}