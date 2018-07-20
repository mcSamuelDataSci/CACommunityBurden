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

#-- Set API ACS Table/variable list ----------------------------------------------------------------------
# B01001_*E except totals   MCS FIXED FROM ---- c(3:26,28:49))

acs.varlist <- "B01003_001"                                     # B01003_001       total: census tract x year
# acs.varlist <- sprintf("B02001_%03d",c(1:10)) 		              # B02001_001-010   total: census tract x year x race (H incl)
# acs.varlist <- c("B01001_001E","B01001_002E", "B01001_026E")    # B01001_002E,026E total: census tract x year x sex (Total, Male, Female)

#-- Get, process, and export data -----------------------------------------------------------------------------------

options(tigris_use_cache = TRUE)
popTractTot2013   <- get_acs(geography = "tract", variables = acs.varlist, state = "CA",year=2013)  # GEOID is 11 digit character string - 2011-2015 - N=8057

.acsvariables <- load_variables(2013,"acs5")
mylabels<-.acsvariables
names(mylabels)[names(mylabels)== "name"] = "variable"                                                        # rename variable to Name (to merge with labels)
mylabels<-mylabels[ (substr(mylabels$variable,0,7)==substring(acs.varlist,0,7)) &
                    (substr(mylabels$variable,nchar(mylabels$variable),nchar(mylabels$variable)) == "E"), ]   # filter mylabels to contain only entries from acs.varlist
mylabels$variable<-substr(mylabels$variable,1,nchar(mylabels$variable)-1)
popTractTot2013<-merge(popTractTot2013,mylabels)

cbdLinkCA         <- read.csv(path(myPlace,"/myInfo/cbdLinkCA.csv"),colClasses = "character") # maps census tracts to MSSAs and counties - N= 8036
popTractTot2013   <- popTractTot2013 %>% mutate(yearG = "2011-2015",  
                                                comID  = cbdLinkCA[match(popTractTot2013$GEOID,cbdLinkCA[,"GEOID"]),"comID"],
                                                county = cbdLinkCA[match(popTractTot2013$GEOID,cbdLinkCA[,"GEOID"]),"county"],
                                                pop    = estimate              ) %>%
                                         select(-NAME,-estimate,-variable,-concept)

saveRDS(popTractTot2013, file=paste0(upPlace,"/upData/popTractTot2013.RDS"))

# NOTE 22 tracts from get_acs do not match to cbdLinkCA  (and 21 based on endyer 2010!) - tracts with NO population
supTracts <- popTractTot2013[is.na(popTractTot2013$county),]
write.csv(supTracts,(paste0(upPlace,"/tempOutput/subTracts.csv")))


# END ---------------------------------------------------------------------------------------------------------------


