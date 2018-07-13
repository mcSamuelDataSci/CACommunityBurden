# =====================================================================================
# "tractPopulationProcessorTotal.R" file                                              |
#            designate folder locations and load packages                             |
#            load packages                                                            |
#            get census tract population data                                         |
#            link tracts to "community IDs" and to county names                       |
#            save data set                                                            |
#                                                                                     |   
# =====================================================================================

# ADD SEX
# ADD RACE

#-- Load Packages ---------------------------------------------------------------------------------------------------

library(tidycensus)  # gets census and ACS data - get_acs function
library(tigris)      # gets "shape" files  # requires census_api_key("MYKEY") to be run once
library(tidyverse)   # data processing
library(fs)          # just for path function

#-- Set Locations Etc----------------------------------------------------------------------

myDrive  <- "E:"  
myPlace  <- path(myDrive,"/0.CBD/myCBD///")  
upPlace  <- path(myDrive,"/0.CBD/myUpstream")  

#-- Get, process, and export data -----------------------------------------------------------------------------------

options(tigris_use_cache = TRUE)
popTractTot2013   <- get_acs(geography = "tract", variables = "B01003_001", state = "CA",year=2013)  # GEOID is 11 digit character string - 2011-2015 - N=8057

cbdLinkCA         <- read.csv(path(myPlace,"/myInfo/cbdLinkCA.csv"),colClasses = "character") # maps census tracts to MSSAs and counties - N= 8036
popTractTot2013   <- popTractTot2013 %>% mutate(yearG = "2011-2015",  
                                                comID  = cbdLinkCA[match(popTractTot2013$GEOID,cbdLinkCA[,"GEOID"]),"comID"],
                                                county = cbdLinkCA[match(popTractTot2013$GEOID,cbdLinkCA[,"GEOID"]),"county"],
                                                pop    = estimate              ) %>%
                                         select(-NAME,-estimate,-variable)

saveRDS(popTractTot2013, file=paste0(upPlace,"/upData/popTractTot2013.RDS"))

# NOTE 22 tracts from get_acs do not match to cbdLinkCA  (and 21 based on endyer 2010!) - tracts with NO population
supTracts <- popTractTot2013[is.na(popTractTot2013$county),]
write.csv(supTracts,(paste0(upPlace,"/tempOutput/subTracts.csv")))


# END ---------------------------------------------------------------------------------------------------------------


