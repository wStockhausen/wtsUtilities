#'
#'@title Drop factor levels from a dataframe
#'
#'@description Function to drop factor levels from a dataframe.
#'
#'@param tbl - dataframe to drop levels from
#'@param dropLevels - list by factor name of factor levels to drop
#'
#'@return dataframe
#'
#'@details Distinct levels of each factor can be dropped from the
#'exported dataframe by seting dropLevels to a list with names corresponding to
#'factor columns and values being vectors of factor levels to drop.
#'
#'@export
#'
dropLevels<-function(tbl,
                     dropLevels=NULL){

    #drop requested factor levels
        dfacs<-names(dropLevels);
        for (dfac in dfacs){
            tbl<-tbl[!(tbl[[dfac]] %in% dropLevels[[dfac]]),];
        }


    return(tbl);
}
