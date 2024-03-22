# =====================================================================================
# "sdohProcessor.R" file                                                              |
#            Read and process Social Determiants of Health (SDOH) data                |
#            load packages                                                            |
#                                                                                     |   
# =====================================================================================

# -- Designate locations and load packages-------------------------------------------------

library(fs)
library(dplyr)
library(readxl)

myDrive <- getwd()  
myPlace <- paste0(myDrive,"/myCBD") 
upPlace <- paste0(myDrive,"/myUpstream") 

#-- LOAD MAIN DATA SET, AND "INFO FILES", BEGIN KEY TRANSFORMATIONS------------------------

# maybe change to read_csv....
HPIdat     <- read.csv(path(upPlace,"/upData/HPI2_MasterFile_2018-04-04.csv"),as.is=TRUE)

county.map <- as.data.frame(read_xlsx(paste0(myPlace,"/myInfo/County Codes to County Names Linkage.xlsx")))
cbdLinkCA  <- read.csv(paste0(myPlace,"/myInfo/Tract to Community Linkage.csv"),colClasses = "character")  # file linking MSSAs to census 
comName    <- unique(cbdLinkCA[,c("comID","comName")])                                    # dataframe linking comID and comName


# hpiScore      	 Percentile ranking of HPI score                                                            	              0-100 (most-least advantaged)
# insured	         Percentage of adults aged 18 to 64 years currently insured	                                                0-100 (least-most advantaged)
# inpreschool	     Percentage of 3 and 4 year olds enrolled in school                                           	            0-100 (least-most advantaged)
# bachelorsed	     Percentage of population over age 25 with a bachelor's education or higher                               	0-100 (least-most advantaged)
# abovePoverty     Percent of the population with an income exceeding 200% of federal poverty level	                          0-100 (least-most advantaged)
# parkaccess	     Percentage of the population living within a half-mile of a park, beach, or open space greater than 1 acre	0-100 (least-most advantaged)
# houserepair	     Percent of households with kitchen facilities and plumbing	                                                0-100 (least-most advantaged)

sdohTract     <- mutate(HPIdat,
                         year         = 2015,
                         geoLev       = "Census Tract",
                         GEOID        = paste0("0",CensusTract),
                         pop          = pop2010,
                         comID        = cbdLinkCA[match(GEOID,cbdLinkCA[,"GEOID"]),"comID"],
                         region       = county.map[match(County_Name,county.map[,"countyName"]),"REGION_HEALTHYCA"],
                         regionF      = as.numeric(as.factor(region))
                     ) %>%
               transform(county=County_Name)    %>% 
                   select(year, county, geoLev, GEOID, pop, comID, region, regionF, hpi2score, insured, inpreschool, bachelorsed, abovepoverty, parkaccess, houserepair) %>% 
                filter(county !="Alpine")


# why are there NA's ?
sdohComm    <- sdohTract %>% group_by(region,county,comID) %>%
  summarize(POP          = sum(pop),
            geoLev       = "Communuity",
            hpi2score    = sum(hpi2score*pop,na.rm=TRUE)/POP,
            insured      = sum(insured *pop,na.rm=TRUE)/POP,
            inpreschool  = sum(inpreschool*pop,na.rm=TRUE)/POP,
            bachelorsed  = sum(bachelorsed*pop,na.rm=TRUE)/POP,
            abovepoverty = sum(abovepoverty*pop,na.rm=TRUE)/POP,
            parkaccess   = sum(parkaccess*pop,na.rm=TRUE)/POP,
            houserepair  = sum(houserepair*pop,na.rm=TRUE)/POP)  %>% transform(pop=POP)



sdohCounty    <- sdohTract %>% group_by(region,county) %>%
  summarize(POP          = sum(pop),
            geoLev       = "County",
            hpi2score    = sum(hpi2score*pop,na.rm=TRUE)/POP,
            insured      = sum(insured *pop,na.rm=TRUE)/POP,
            inpreschool  = sum(inpreschool*pop,na.rm=TRUE)/POP,
            bachelorsed  = sum(bachelorsed*pop,na.rm=TRUE)/POP,
            abovepoverty = sum(abovepoverty*pop,na.rm=TRUE)/POP,
            parkaccess   = sum(parkaccess*pop,na.rm=TRUE)/POP,
            houserepair  = sum(houserepair*pop,na.rm=TRUE)/POP)  %>% transform(pop=POP)


save(sdohTract,  file= path(myPlace,"/myData/","sdohTract.R"))
save(sdohComm,   file= path(myPlace,"/myData/","sdohComm.R"))
save(sdohCounty, file= path(myPlace,"/myData/","sdohCounty.R"))

