# =====================================================================================
# "acsCensusCurrent.R" file                                                           |
#            designate folder locations and load packages                             |
#            load packages                                                            |
#            get census tract population data                                         |
#            link tracts to "community IDs" and to county names                       |
#            save data set                                                            |
#                                                                                     |   
# =====================================================================================

#-- Set Locations Etc----------------------------------------------------------------------

myDrive  <- "E:"  
myPlace  <- paste0(myDrive,"/0.CBD/myCBD")  
upPlace  <- paste0(myDrive,"/0.CBD/myUpstream")  

#-- Load Packages ---------------------------------------------------------------------------------------------------

library(tidycensus)  # gets census and ACS data - get_acs function
library(tigris)      # gets "shape" files  # requires census_api_key("MYKEY") to be run once
library(tidyverse)   # data processing

options(tigris_use_cache = TRUE)

# -------------------------------------------------------------------------------------------------------------------
junkEdVars <- c(paste0("b15002_00",1:9),paste0("b15002_0",10:35))
popTractTot2013   <- get_acs(geography = "tract", variables = junkEdVars, state = "CA",year=2015)  




edVars <- paste0("B15002_00",1:9)
popTractTot2013   <- get_acs(geography = "tract", variables = edVars, state = "CA")  # GEOID is 11 digit character string #2011-2015


popTractTot2013   <- get_acs(geography = "tract", variables = "B01003_001", state = "CA",year=2013)  # GEOID is 11 digit character string #2011-2015


#8057
cbdLinkCA      <- read.csv(paste0(myPlace,"/myInfo/cbdLinkCA.csv"),colClasses = "character")           # file linking census tracts to MSSAs and counties 
#8036
popTractTot2013   <- mutate(popTractTot2013,
                           yearG = "2011-2015",  
                           comID  = cbdLinkCA[match(popTractTot2013$GEOID,cbdLinkCA[,"GEOID"]),"comID"],
                           county = cbdLinkCA[match(popTractTot2013$GEOID,cbdLinkCA[,"GEOID"]),"county"],
                           pop    = estimate              )
popTractTot2013$NAME  <- popTractTot2013$variable <- popTractTot2013$estimate <- NULL                                       # remove unnecessary "variables"

# NOTE 22 tracts from get_acs do not match to cbdLinkCA  (and 21 based on endyer 2010!) - tracts with NO population
supTracts <- popTractTot2013[is.na(popTractTot2013$county),]
write.csv(supTracts,(paste0(upPlace,"/tempOutput/subTracts.csv")))

#popCensusCom <- popCensusCom[!is.na(popCensusCom$county),]

saveRDS(popTractTot2013, file=paste0(upPlace,"/upData/popTractTot2013.RDS"))


# END ================================================================================================================


