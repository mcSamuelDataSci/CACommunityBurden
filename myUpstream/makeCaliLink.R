# =====================================================================================
# "makeCaliLink" file                                                                 |
#            ....                                                                     |
#                                                                                     |
#            generates .csv file that must have                                       |
#              GEOID -- 11 character geoid for census tract (with leading "0")        |
#              comID -- ID variable for community                                     |
#              comName -- Name of community                                           |
#              county -- county name                                                  |
#              year -- year if relevant                                               |   
# =====================================================================================

myDrive  <- "E:"
myPlace  <- paste0(myDrive,"/0.CBD/myCBD")
upPlace  <- paste0(myDrive,"/0.CBD/myUpstream")

library(dplyr) 

# OSHPD -- why doesn't CENSUS_KEY have leading 0??
# ERROR in Open Data Potal file -- county names have blanks on both sides! trimws fixes this -- error reported to OSHPD
# file linking MSSAs to census tracts
tractMSSA  <- read.csv(paste0(upPlace,"/upData/medical-service-study-area-mssa---census-detail-2013.csv"),colClasses = "character")            
tractMSSA  <- tractMSSA %>% rename(county=COUNTY) %>% transform(GEOID = paste0("0",CENSUS_KEY), year = 2010, county=trimws(county)) %>%
                         select(year,GEOID,MSSA_ID,county) 
                         #select("STATE_FIPS","CNTY_FIPS","GEOID","county","MSSA_ID","MSSA_NAME") 

adjMSSA    <- read.csv(paste0(upPlace,"/upData/mssaMSSA-Adjusted.csv"),colClasses = "character")    # file listings adjusted (and original) MSSAs
# would be nice to read directly from Excel file, but generates awkward strings  (e.g. "1.1000000000000001" rather than "1.1")
# library(readxl)
# adjMSSA   <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/mssaMSSA-Adjusted.xlsx"),col_types = "text"))  

#  adjMSSA$comID   <- adjMSSA$adjustedID              # Adjusted
#  adjMSSA$comName <- adjMSSA$adjustedMSSA_NAME       # Adjusted
 adjMSSA$comID   <- adjMSSA$MSSA_ID                 # Original
 adjMSSA$comName <- adjMSSA$MSSA_NAME               # Original
adjMSSA           <- adjMSSA[, c("MSSA_ID","comID","comName")]

cbdLinkCA <- full_join(tractMSSA,adjMSSA,by= "MSSA_ID") %>%  select(-MSSA_ID)

write.csv(cbdLinkCA,paste0(myPlace,"/myInfo/cbdLinkCA.csv"),row.names=FALSE)
