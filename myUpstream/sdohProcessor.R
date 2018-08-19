# =====================================================================================
# "sdohProcessor.R" file                                                              |
#            Read and process Social Determiants of Health (SDOH) data                |
#            load packages                                                            |
#                                                                                     |   
# =====================================================================================

# -- Designate locations and load packages-------------------------------------------------

library(fs)
library(dplyr)

myDrive <- "E:/0.CBD"  
myPlace <- paste0(myDrive,"/myCBD") 
upPlace <- paste0(myDrive,"/myUpstream") 

#-- LOAD MAIN DATA SET, AND "INFO FILES", BEGIN KEY TRANSFORMATIONS------------------------

# maybe change to read_csv....
HPIdat     <- read.csv(path(upPlace,"/upData/HPI2_MasterFile_2018-04-04.csv"),as.is=TRUE)

county.map <- read.csv(paste0(myPlace,"/myInfo/county_crosswalk.csv"),as.is=TRUE)
cbdLinkCA  <- read.csv(paste0(myPlace,"/myInfo/cbdLinkCA.csv"),colClasses = "character")  # file linking MSSAs to census 
comName    <- unique(cbdLinkCA[,c("comID","comName")])                                    # dataframe linking comID and comName

myYear <- 2015

sdohTract     <- mutate(HPIdat,
                         year         = myYear,
                         geoLev       = "Census Tract",
                         GEOID        = paste0("0",CensusTract),
                         pop          = pop2010,
                         lessBachelor = 100 - bachelorsed,
                         belowPov     = 100 - abovepoverty,
                         hpiScore     = hpi2score,
                         comID        = cbdLinkCA[match(GEOID,cbdLinkCA[,"GEOID"]),"comID"],
                         region       = county.map[match(County_Name,county.map[,"NAME1_PROPER"]),"REGION_HEALTHYCA"],
                         regionF      = as.numeric(as.factor(region))
                     ) %>%
               transform(county=County_Name)

# why are there NA's ?
sdohComm    <- sdohTract %>% group_by(region,county,comID) %>%
  summarize(pop          = sum(pop),
            geoLev       = "Communuity",
            lessBachelor = sum((100 - bachelorsed)*pop2010,na.rm=TRUE)/pop,
            belowPov     = sum((100 - abovepoverty)*pop2010,na.rm=TRUE)/pop,
            hpiScore     = sum(hpi2score*pop2010,na.rm=TRUE)/pop
  )

sdohCounty    <- sdohTract %>% group_by(region,county) %>%
  summarize(pop          = sum(pop2010),
            geoLev       = "County",
            lessBachelor = sum((100 - bachelorsed)*pop2010,na.rm=TRUE)/pop,
            belowPov     = sum((100 - abovepoverty)*pop2010,na.rm=TRUE)/pop,
            hpiScore     = sum(hpi2score*pop2010,na.rm=TRUE)/pop
            ) 
  
sdohTract <- select(sdohTract,year,GEOID,county,region,pop,lessBachelor,belowPov,hpiScore)


save(sdohTract,  file= path(myPlace,"/myData/","sdohTract.R"))
save(sdohComm,   file= path(myPlace,"/myData/","sdohComm.R"))
save(sdohCounty, file= path(myPlace,"/myData/","sdohCOunty.R"))

