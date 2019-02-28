# =====================================================================================
# "make_California_tract_to_community_link.R
#                                                                                     |
#            generates .csv file that must have                                       |
#              GEOID -- 11 character geoid for census tract (with leading "0")        |
#              comID -- ID variable for community                                     |
#              comName -- Name of community                                           |
#              county -- county name                                                  |
#              year -- year if relevant                                               |   
# =====================================================================================

myDrive  <- getwd()                           
myPlace  <- paste0(myDrive,"/myCBD")  
upPlace  <- paste0(myDrive,"/myUpstream")

library(dplyr) 

# Read file from CHHS Open Data Portal, that links census tracts to
#   Medical Service Sutdy Area (MSSAs) ID and name.  MSSAs are used
#   to define communities in the CBD

# NOTE: The CENSUS_KEY Does NOT have a leading 0
# RESOLVED:  ERROR in Open Data Potal file -- county names had blanks on both sides

openDataUrl <- 
"https://data.chhs.ca.gov/dataset/f2245276-1769-42fe-a9b9-e3078e2f9111/
resource/9a19c798-b2e6-4334-bf0f-ec7321e3c7f4/download/
medical-service-study-area-mssa-census-detail-2013.csv"

tractMSSA  <- read.csv(openDataUrl,colClasses = "character")            

tractMSSA  <- tractMSSA %>% rename(county=COUNTY) %>% transform(GEOID = paste0("0",CENSUS_KEY), year = 2010, county=trimws(county)) %>%
                         select(year,GEOID,MSSA_ID,county) 
                         #select("STATE_FIPS","CNTY_FIPS","GEOID","county","MSSA_ID","MSSA_NAME") 


# -- "Adjusted MSSA work" ---------------------------------------------------------

# mssaMSSA-Adjusted.xlsx list original MSSAs AND MSSAs that have be "ADJUSTED"
#   "by hand" to have > 20,000 population, for possible data
#   de-identification and CHHS data de-indentificagtion Guidles (DDG)

adjMSSA         <- read.csv(paste0(upPlace,"/upData/mssaMSSA-Adjusted.csv"),
                            colClasses = "character")    

adjMSSA$comID   <- adjMSSA$adjustedID              # Adjusted
adjMSSA$comName <- adjMSSA$adjustedMSSA_NAME       # Adjusted

adjMSSA$comID   <- adjMSSA$MSSA_ID                 # Original
adjMSSA$comName <- adjMSSA$MSSA_NAME               # Original

adjMSSA         <- adjMSSA[, c("MSSA_ID","comID","comName")]

cbdLinkCA <- full_join(tractMSSA,adjMSSA,by= "MSSA_ID") %>%  select(-MSSA_ID)

write.csv(cbdLinkCA,paste0(myPlace,"/myInfo/Tract to Community Linkage.csv"),row.names=FALSE)
