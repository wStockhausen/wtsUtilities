#'
#'@title Get the operating system name.
#'
#'@description Function that returns the operating system name.
#'
#'@return Either 'OSX' or 'Windows'
#'
#'@details Checks `Sys.info()[['sysname']]` for platform name and returns 'Win' 
#'if lower case platform name is 'windows', 'OSX' if 'darwin'.
#'
#'@export
#'
getOperatingSystem<-function(){
    #check the operating platform
    MacOSX<-'OSX';
    Win<-'Windows';
#    cat(plat,'\n')
    if (isOSX()){
        platform<-MacOSX;
    } else if (isWin) {
        platform<-Win;
    } else {
        stop("Operating system '",Sys.info()[['sysname']],"' not recognized!");
    }
    return(platform);
}

#'
#'@title Test if operating system is OSX.
#'
#'@description Function that returns TRUE if the operating system is OSX, FALSE otherwise.
#'
#'@return TRUE if the operating system is OSX, FALSE otherwise
#'
#'@details Checks `Sys.info()[['sysname']]` for platform name and returns TRUE
#'if the lower case platform name is 'darwin', FALSE otherwise.
#'
#'@export
#'
isOSX<-function(){
    ifelse(tolower(Sys.info()[['sysname']])=="darwin",TRUE,FALSE)
}

#'
#'@title Test if operating system is Windows.
#'
#'@description Function that returns TRUE if the operating system is Windows, FALSE otherwise.
#'
#'@return TRUE if the operating system is Windows, FALSE otherwise
#'
#'@details Checks `Sys.info()[['sysname']]` for platform name and returns TRUE
#'if the lower case platform name is 'windows', FALSE otherwise.
#'
#'@export
#'
isWin<-function(){
    ifelse(tolower(Sys.info()[['sysname']])=="windows",TRUE,FALSE)
}

