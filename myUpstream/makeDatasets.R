# =====================================================================================
# "makeDatasets.R" file                                                               |
#            designate folder locations and load packages                             |
#            load packages                                                            |
#                                                                                     |   
# =====================================================================================

# -- Designate locations and load packages-------------------------------------------------

myDrive <- "E:"  
myPlace <- paste0(myDrive,"/0.CBD/myCBD") 
upPlace <- paste0(myDrive,"/0.CBD/myUpstream") 

library(tidyverse)
library(epitools)
library(sqldf)
library(readxl)
library(fs)

#-- LOAD MAIN DATA SET, AND "INFO FILES", BEGIN KEY TRANSFORMATIONS------------------------

whichDat <- "fake"

if (whichDat == "real") {
# CAUTION --- if using REAL DATA INCLUDE these two lines below and edit the first one with your secure location
# load("G:/CCB/0.Secure.Data/myData/cbdDat0FULL.R")     
 load("H:/0.Secure.Data/myData/cbdDat0FULL.R")      
  cbdDat0 <- cbdDat0FULL    
}
 
if (whichDat == "fake") { 
# Load FAKE Data --- COMMENT OUT these two lines if using REAL DATA
   load(paste0(upPlace,"/upData/cbdDat0SAMP.R"))      
   cbdDat0 <- cbdDat0SAMP
}
   #forEthan <- sample_n(cbdDat0SAMP,100000)
   #saveRDS(forEthan, file=paste0(upPlace,"/upData/forEthan.RDS"))

# this "as.data.frame" below and elsewhere is really annoying.... but at least icdToGroup function below does not work otherwise;
# becuase the "tibble" is double precision or for some other reason this messes up; 
# and get error "Error: Can't use matrix or array for column indexing"

gbdMap0    <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/gbd.ICD.Map.xlsx"), sheet="main"))   # also have e.g. range="A1:J167"
leMap      <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/le.Map.xlsx"), sheet="LifeExpLink", range = cell_cols("A:B")))
yearMap    <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/year.Map.xlsx")))
geoMap     <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/countycodes.Map.xlsx")))
cbdLinkCA  <- read.csv(paste0(myPlace,"/myInfo/cbdLinkCA.csv"),colClasses = "character")  # file linking MSSAs to census 
comName    <- unique(cbdLinkCA[,c("comID","comName")])                                    # dataframe linking comID and comName

cbdDat0       <- mutate(cbdDat0,
                         sex    = c("Male","Female")[match(sex,c("M","F"))],
                         age    = as.numeric(age),                                                  # redundant...
                         ICD10  = as.character(ICD10),                                              # redundant...
                         comID  = cbdLinkCA[match(cbdDat0$GEOID,cbdLinkCA[,"GEOID"]),"comID"],   
                         yll    = leMap[match(cbdDat0$age,leMap[,"Age"]),"LE"],
                         yearG  = yearMap[match(year,yearMap[,"year"]),"yGroup1"]           )

# Add age-Group variable for age-specific rates and age-adjustment -------------------------------------------------------
ageMap        <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/Age Group and Standard US 2000 population.xlsx"),sheet = "data"))
aL            <-      ageMap$lAge     # lower age ranges
aU            <- c(-1,ageMap$uAge)    # upper age ranges, plus inital value of "-1" for lower limit
aLabs         <- paste(aL,"-",aU[-1]) # make label for ranges
aMark         <- findInterval(cbdDat0$age,aU,left.open = TRUE)  # vector indicating age RANGE value of each INDIVIDUAL age value
cbdDat0$ageG  <- aLabs[aMark]                                   # make new "ageG" variable based on two objects above 


# core function to map ICD-10 codes to GBD conditions ---------------------------------------------------------------------
icdToGroup <- function(myIn,myInMap) {
  Cause   <- rep(NA,length(myIn))
  for (i in 1:nrow(myInMap)) {Cause[grepl(myInMap[i,2],myIn)] <- myInMap[i,1] } 
  Cause}

cbdDat0$gbd36   <- icdToGroup(myIn=cbdDat0$ICD10,gbdMap0[!is.na(gbdMap0$list36),c("gbdCode","regEx10")])      
cbdDat0$gbd3    <- icdToGroup(myIn=cbdDat0$ICD10,gbdMap0[is.na(gbdMap0$L2)     ,c("gbdCode","regEx10")])      
cbdDat0$gbdSpec <- icdToGroup(myIn=cbdDat0$ICD10,gbdMap0[ (gbdMap0$L2 %in% c(2,6,8,11,13,14,15,16,21,22) & !is.na(gbdMap0$L3) & is.na(gbdMap0$L4) & is.na(gbdMap0$list36) )    ,c("gbdCode","regEx10")])      

popTract         <- readRDS(path(upPlace,"/upData/popTract2013.RDS"))
popTractTotal    <- filter(popTract,sex == "Total" & ageG == "Total")
popTractSexTotal <- filter(popTract,                 ageG == "Total")
  
  
#---------------

# DATA CLEANING ISSUES --------------------------------------------------------------------------------

# in 2012 Los Angeles Census Tract 9304.01 was merged into tract 1370.00
# "The deletion of Census 2000 Tract 1370.00 is now corrected, and the tract is reinstated
#   with its former boundaries. This change incorporates all of former (2010) Census Tract 9304.01
#   and part of (2010) Census Tract 8002.04 into the reinstated (2012) tract 1370.00.
# https://www.census.gov/programs-surveys/acs/technical-documentation/table-and-geography-changes/2012/geography-changes.html

# LA CENSUS TRACT TO RECODE
# 06037930401 should be recoded to  06037137000 in all data files

# CENSUS TRACTS
# current from acsWork0     has 8057 tracts 
# current cbdLinkCA         has 8036 (2010 data)
# current cbddat0           has 8603! bad geocodes?        
# something ??              has 8035 ... check...

#temp <- popCensusCom$GEOID
#junk <- cbdDat0[!(cbdDat0$GEOID %in% temp),]
#junk <- junk[junk$GEOID != "",]
#write.csv(junk,(paste0(upPlace,"/tempOutput/junk Tracts.csv")))

# these records have a GEOID but not comID suggesting the GEOID is "bad"
# junk <- filter(cbdDat0,is.na(comID) & GEOID != ""  & year > 2004)  
# 651 records
# length(unique(junk$GEOID))
# 590 unique GEOID not in California (based on current link file)
#  write.csv(table(junk$GEOID,junk$year),(paste0(upPlace,"/tempOutput/junk Tracts.csv")))

# county missing from 3797 records     
# junk <- filter(cbdDat0,is.na(county))   
# 3797 records
# countyFIPS blank=2145 and 999=1652 (but State="CA; based on "F71" only)
#  write.csv(table(junk$year,junk$countyFIPS),(paste0(upPlace,"/tempOutput/missing County FIPS.csv")))

# MAJOR cleaning issue!!!
# junk <- filter(cbdDat0,is.na(gbd36))   
# 82775 records where ICD10 does not map to gbd36 -- errors in info file!
#  write.csv(table(junk$year,junk$countyFIPS),(paste0(upPlace,"/tempOutput/no ICD10 to gbd36.csv")))

#-- AGE ADJUSTED ("AA") RATES --- make function?   move lower in code? -------------------------------------------------------------------------------


# make dataframe of all possible combinations of county, year, CAUSE, and ageG for ----------------

year   <- data.frame(year   = 2000:2015) # these "vectors" need to be dataframes for the sq merge below to work
yearG  <- data.frame(yearG  = "2011-2015")
CAUSE  <- data.frame(CAUSE  = c(0,sort(unique(cbdDat0$gbd36))))  # "0" is for "all cause"
ageG   <- data.frame(ageG   = sort(unique(cbdDat0$ageG)))
county <- data.frame(county = geoMap$countyName)         
comID  <- data.frame(comID  = unique(cbdLinkCA[,"comID"]))
GEOID  <- data.frame(GEOID  = cbdLinkCA[,"GEOID"])

                                     
# other cool approach from Adam:
# fullMatCounty <- Reduce(function(...) merge(..., all = TRUE), list(county, year, CAUSE, ageG))
fullMatCounty <- sqldf(" select * from  county cross join year  cross join CAUSE cross join ageG")
fullMatComm   <- sqldf(" select * from  comID  cross join yearG cross join CAUSE cross join ageG")
fullMatTract  <- sqldf(" select * from  GEOID  cross join yearG cross join CAUSE cross join ageG")

fullMatCounty <- mutate(fullMatCounty, county = as.character(county),                             ageG   = as.character(ageG), tester = 0)
fullMatComm   <- mutate(fullMatComm,   comID  = as.character(comID), yearG = as.character(yearG), ageG   = as.character(ageG), tester = 0)
fullMatTract  <- mutate(fullMatTract,  GEOID  = as.character(GEOID), yearG = as.character(yearG), ageG   = as.character(ageG), tester = 0)

# -------------------------------------------------------------------------------------------------
# County age deaths ------------------------------------------------------

# number of deaths by county, year, *AGE GROUP*
datAA1 <- cbdDat0 %>% group_by(county,year,ageG,gbd36) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) )  # BY CAUSE
datAA0 <- cbdDat0 %>% group_by(county,year,ageG)       %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) )  # ALL CAUSE

names(datAA1)[names(datAA1)=="gbd36"] <- "CAUSE"
datAA0$CAUSE <- 0
datAA1 <- bind_rows(datAA1,datAA0)   # merge cause-specific and all-cause  # 140,192 records

# DATA CLEANING ISSUES as above
datAA1 <- filter(datAA1,!is.na(ageG))   # remove 403 records with missing age (0.065% of deaths)  -- impact of this?
datAA1 <- filter(datAA1,!is.na(CAUSE))  # remove 6955 records with missing CAUSE
datAA1 <- filter(datAA1,!is.na(county)) # remove 758 records with missing county

ageCounty   <- merge(fullMatCounty,datAA1,by = c("county","year","ageG","CAUSE"),all=TRUE)   # merge with "fullMatCounty"
popCOage    <- readRDS(paste0(upPlace,"/upData/popCountyAgeG2000to2015.RDS"))
ageCounty   <- merge(ageCounty,popCOage,by = c("county","year","ageG"),all=TRUE)   # merge with county age populations
popStandard <- readRDS(paste0(upPlace,"/upData/popStandard.RDS"))
ageCounty   <- merge(ageCounty,popStandard,by = c("ageG"),all=TRUE)  # merge with "Standard" population

ageCounty$Ndeaths[is.na(ageCounty$Ndeaths)] <- 0    # if NA deaths in strata change to "0"
ageCounty$YLL[is.na(ageCounty$YLL)]         <- 0    # if NA deaths in strata change to "0"


countyAA <- ageCounty %>% group_by(county,year,CAUSE) %>%
  summarize(oDeaths = sum(Ndeaths),
            aRate   = ageadjust.direct(count=Ndeaths, pop=pop, rate = NULL, stdpop=popStandard, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct(count=Ndeaths, pop=pop, rate = NULL, stdpop=popStandard, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct(count=Ndeaths, pop=pop, rate = NULL, stdpop=popStandard, conf.level = 0.95)[4]*100000, 
            YLL.adj.rate   = ageadjust.direct(count=YLL, pop=pop, rate = NULL, stdpop=popStandard, conf.level = 0.95)[2]*100000) # JUST ADDED -- numbers look reasonable but need to confirm --- once confirmed, will add CIs with ageadjust function
countyAA <- countyAA[!(countyAA$oDeaths==0),c("county","year","CAUSE","aRate","aLCI","aUCI","YLL.adj.rate")]  # remove strata with no deaths and select columns  


#calculate number of expected deaths in strata among standard population
#ageCounty$deathsE <- (ageCounty$Ndeaths/ageCounty$pop)*ageCounty$popStandard

# "manual" calculation of age-adjusted rates, AND using ageadjust.direct function from EpiTools package
# NOTE: oDeaths etc  != total deaths in other files because of missings removed# 
#   summarize(oDeaths = sum(Ndeaths),         # na.rm=TRUE not needed becuase of cleaning above
#             oPop    = sum(pop),
#             cRate   = 100000*oDeaths/oPop,
#             eDeaths = sum(deathsE),
#             ePop    = sum(popStandard),
#             aRate   = 100000*eDeaths/ePop)

# age-adjustment reference
# https://www.cdc.gov/nchs/data/nvsr/nvsr47/nvs47_03.pdf



# Community age deaths ----------------------------------------------------

datAA2 <- cbdDat0 %>% group_by(comID,yearG,ageG,gbd36) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) )  # BY CAUSE
datAA0 <- cbdDat0 %>% group_by(comID,yearG,ageG)       %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) )  # ALL CAUSE

names(datAA2)[names(datAA2)=="gbd36"] <- "CAUSE"
datAA0$CAUSE <- 0
datAA2 <- bind_rows(datAA2,datAA0)  
datAA2 <- na.omit(datAA2)           
datAA2 <- filter(datAA2,comID != "")

ageComm   <- merge(fullMatComm,datAA2,by = c("comID","yearG","ageG","CAUSE"),all=TRUE)
ageComm   <- filter(ageComm,yearG == "2011-2015")

#popCommAge <- readRDS(paste0(upPlace,"/upData/popTractAgeG2013.RDS")) 

popCommAge <-  readRDS(paste0(upPlace,"/upData/popTractAgeG2013.RDS")) %>%
               group_by(comID,yearG,ageG) %>%
               summarise(pop=sum(pop))


ageComm     <- merge(ageComm,popCommAge,by = c("comID","yearG","ageG"),all=TRUE)   # merge with county age populations
popStandard <- readRDS(paste0(upPlace,"/upData/popStandard.RDS"))
ageComm     <- merge(ageComm,popStandard,by = c("ageG"),all=TRUE)  # merge with "Standard" population

ageComm$Ndeaths[is.na(ageComm$Ndeaths)] <- 0    # if NA deaths in strata change to "0"
ageComm$YLL[is.na(ageComm$YLL)]         <- 0    # if NA deaths in strata change to "0"


commAA <- ageComm %>% group_by(comID,yearG,CAUSE) %>%
  summarize(oDeaths = sum(Ndeaths),
            aRate   = ageadjust.direct(count=Ndeaths, pop=pop, rate = NULL, stdpop=popStandard, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct(count=Ndeaths, pop=pop, rate = NULL, stdpop=popStandard, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct(count=Ndeaths, pop=pop, rate = NULL, stdpop=popStandard, conf.level = 0.95)[4]*100000, 
            YLL.adj.rate   = ageadjust.direct(count=YLL, pop=pop, rate = NULL, stdpop=popStandard, conf.level = 0.95)[2]*100000) 
commAA <- commAA[!(commAA$oDeaths==0),c("comID","yearG","CAUSE","aRate","aLCI","aUCI","YLL.adj.rate")]  


#######
# removes rows with aRate = inf HERE there are only ALPINE 
commAA  <- commAA[!(commAA$aRate > 10000),]


# Tract age deaths ----------------------------------------------------

datAA3 <- cbdDat0 %>% group_by(GEOID,yearG,ageG,gbd36) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) )  # BY CAUSE
datAA0 <- cbdDat0 %>% group_by(GEOID,yearG,ageG)       %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) )  # ALL CAUSE

names(datAA3)[names(datAA3)=="gbd36"] <- "CAUSE"
datAA0$CAUSE <- 0
datAA3 <- bind_rows(datAA3,datAA0)  
datAA3 <- na.omit(datAA3)           
datAA3 <- filter(datAA3,GEOID != "")



ageTract   <- merge(fullMatTract,datAA3,by = c("GEOID","yearG","ageG","CAUSE"),all=TRUE)
ageTract   <- filter(ageTract,yearG == "2011-2015")

popTractAge <- readRDS(paste0(upPlace,"/upData/popTractAgeG2013.RDS")) 

popTractAge <-  readRDS(paste0(upPlace,"/upData/popTractAgeG2013.RDS")) %>%
  group_by(GEOID,yearG,ageG) %>%
  summarise(pop=sum(pop))


ageTract     <- merge(ageTract,popTractAge,by = c("GEOID","yearG","ageG"),all=TRUE)   # merge with county age populations
popStandard <- readRDS(paste0(upPlace,"/upData/popStandard.RDS"))
ageTract     <- merge(ageTract,popStandard,by = c("ageG"),all=TRUE)  # merge with "Standard" population

ageTract$Ndeaths[is.na(ageTract$Ndeaths)] <- 0    # if NA deaths in strata change to "0"
ageTract$YLL[is.na(ageTract$YLL)]         <- 0    # if NA deaths in strata change to "0"


tractAA <- ageTract %>% group_by(GEOID,yearG,CAUSE) %>%
  summarize(oDeaths = sum(Ndeaths),
            oPop    = sum(pop),
            aRate   = ageadjust.direct(count=Ndeaths, pop=pop, rate = NULL, stdpop=popStandard, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct(count=Ndeaths, pop=pop, rate = NULL, stdpop=popStandard, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct(count=Ndeaths, pop=pop, rate = NULL, stdpop=popStandard, conf.level = 0.95)[4]*100000, 
            YLL.adj.rate   = ageadjust.direct(count=YLL, pop=pop, rate = NULL, stdpop=popStandard, conf.level = 0.95)[2]*100000) 

tractAA <- tractAA[!(tractAA$oDeaths==0),c("GEOID","yearG","CAUSE","aRate","aLCI","aUCI","YLL.adj.rate")]  

#######
# removes rows with aRate = inf, or infinity becuase some population strata is 0 AND some other odd strata
tractAA  <- tractAA[!(tractAA$aRate > 5000),]

tractAA  <- tractAA[!(is.na(tractAA$aRate)),]

# MEASURES FUNCTION =====================================================================================

myMeasures <- function(group_vars,levLab,myTotal=TRUE){
  
   dat <- cbdDat0 %>% group_by_(.dots = group_vars) %>% 
    summarize(Ndeaths = n() , 
              YLL     = sum(yll,   na.rm = TRUE),    # NEED TO ADD CIs
              m.YLL   = mean(yll,  na.rm = TRUE),     # NEED TO ADD CIs
              mean.age = mean(age,na.rm=TRUE)
              #med.age = median(age,na.rm = TRUE)
              ) %>%  ungroup
   names(dat)[grep("gbd", names(dat))] <- "CAUSE"     # CHANGE "gbd" here to iGRP and add that in front of any groupings
 
   
   if (myTotal) {
   temp <- length(group_vars) 
   dat2 <- cbdDat0 %>% group_by_(.dots = group_vars[-temp]) %>% 
     summarize(Ndeaths = n() , 
               YLL     = sum(yll,   na.rm = TRUE), # NEED TO ADD CIs
               m.YLL   = mean(yll,  na.rm = TRUE), # NEED TO ADD CIs
               mean.age = mean(age,na.rm=TRUE)
               #med.age = median(age,na.rm = TRUE)
               ) %>% ungroup
   dat2$CAUSE <- 0
   dat <- bind_rows(dat,dat2)
   }

   dat       <- filter(dat,!is.na(CAUSE))  # "HARD FIX" that should be assessed carefully
   dat$Level <- levLab
   dat %>% data.frame
}

#  Examples
#  grp <- c("county","year","sex","gbd3")
#  df  <- myMeasures(grp,grp[length(grp)])
#  df  <- myMeasures(grp,grp[length(grp)],myTotal=FALSE)
  
# RATES FUNCTION =====================================================================================

yF   <- 100000  # rate constant 
pop5 <- 5
pop1 <- 1

ccbRates <- function(inData,yearN){
              transform(inData, 
               YLLper     = yF*YLL/(yearN*pop),
               YLLrateLCI    = yF*pois.approx(YLL,yearN*pop, conf.level = 0.95)$lower,  # need to confirm that this is correct
               YLLrateUCI    = yF*pois.approx(YLL,yearN*pop, conf.level = 0.95)$upper,
       
               cDeathRate = yF*Ndeaths/(yearN*pop),
               rateLCI    = yF*pois.approx(Ndeaths,yearN*pop, conf.level = 0.95)$lower,
               rateUCI    = yF*pois.approx(Ndeaths,yearN*pop, conf.level = 0.95)$upper
               )
}
  
# =====================================================================================================================
# TRACT - level file

# Group Causes
grp       <- c("county","GEOID","yearG","gbd36")
datTract  <- myMeasures(grp,grp[length(grp)]) %>% mutate(sex="Total")

grp2      <- c("county","GEOID","yearG","sex","gbd36")
datTract2 <- myMeasures(grp2,grp2[length(grp2)])

datTract  <- bind_rows(datTract,datTract2) %>%
                arrange(county,GEOID,yearG,CAUSE)



# POPULATION file for tract level
popDat1  <- as.data.frame(popTractSexTotal   %>% group_by(county,GEOID,sex) %>% summarize(pop = sum(pop,na.rm = TRUE) ))

# MERGE Death and Population files
datTract <- merge(datTract,popDat1,by = c("county","GEOID","sex"))                     

# Calculate Rates
datTract <- ccbRates(datTract,5) %>%
             arrange(county,GEOID,yearG,CAUSE)

# add adjusted rates
datTract <- merge(datTract,tractAA ,by = c("GEOID","yearG","CAUSE"),all=TRUE)


# =====================================================================================================================
# COMMUNITY - level file  

grp36   <- c("county","comID","yearG","gbd36")
grp3    <- c("county","comID","yearG","gbd3")

datComm <- rbind(myMeasures(grp36,grp36[length(grp36)]),
                 myMeasures( grp3, grp3[ length(grp3)],myTotal=FALSE))

popDat1  <- as.data.frame(popTractTotal  %>% group_by(county,comID)  %>% summarize(pop = sum(pop,na.rm = TRUE) ))
datComm  <- merge(datComm,popDat1,by = c("county","comID"))

datComm <- ccbRates(datComm,5)

# add community names
datComm  <- merge(datComm, comName, by.x="comID", by.y = "comID",all=TRUE)

# add adjusted rates
datComm <- merge(datComm,commAA ,by = c("comID","yearG","CAUSE"),all=TRUE)



# =====================================================================================================================
# STATE-level file  

grp36    <- c("year","gbd36")
datState <- myMeasures(grp36, grp36[length(grp36)])

grpSpec  <- c("year","gbdSpec")
spec     <- myMeasures(grpSpec, grpSpec[length(grpSpec)],myTotal=FALSE)

datState <- rbind(datState,spec) 

popCounty <- readRDS(file= paste0(upPlace,"/upData/popCountyTot2000to2015.RDS"))
statePop  <- popCounty[popCounty$county=="California" ,c("year","pop")]    # CA population by year
datState  <- merge(datState, statePop, by="year",all=TRUE)

datState <- ccbRates(datState,1)

datState$causeName <- gbdMap0[match(datState$CAUSE,gbdMap0[,1]),"nameOnly"]   # needed? 
datState$county    <- "CALIFORNIA STATE"


# ---------------------------------------------------------------------------------------
# COUNTY - Level Data Set Generation 

grp       <- c("county","year","gbd36")
datCounty <- myMeasures(grp,grp[length(grp)])

datCounty$causeName <- gbdMap0[match(datCounty$CAUSE,gbdMap0[,1]),"nameOnly"]  #  "needed?"

datCounty <- merge(datCounty,popCounty,by = c("year","county"))

datCounty <- ccbRates(datCounty,1)

# SMR
tState               <- datState
tState$stateRate     <- tState$cDeathRate
tState               <- tState[,c("year","Level","CAUSE","stateRate")]

# for LOCAL installation of application EXCLUDE save line and INCLUDE load line
save(tState, file= paste0(upPlace,"/upData/tState.R"))
#load(file= paste0(upPlace,"/upData/tState.R"))

datCounty            <- merge(datCounty,tState,by = c("year","Level","CAUSE"))
datCounty$SMR        <- datCounty$cDeathRate / datCounty$stateRate

# Bring in age-adjusted rates
datCounty <- merge(datCounty,countyAA ,by = c("county","year","CAUSE"),all=TRUE)

# Bring State file into County file 

datCounty <- bind_rows(datState,datCounty)

# "SMALL CELL and "RISKY CAUSE" supression --------------------------------------------------------

#should be <- NA, but then shows up as "highest on ranking..."  fix at some point
datCounty$SMR[datCounty$Ndeaths < 6]                  <- 0
datCounty$SMR[datCounty$county == "CALIFORNIA STATE"] <- 0

xCause0 <- c(14,41,50,139,4,49,192)
xCause1 <- c(xCause0,10)

datTract  <- filter(datTract, !(CAUSE %in% xCause1))
datComm   <- filter(datComm,  !(CAUSE %in% xCause1))
datCounty <- filter(datCounty,!(CAUSE %in% xCause0))

# Output Files ------------------------------------------------------------------------------------

# write.csv(datTract,(paste0(upPlace,"/tempOutput/Tract CCB Work.csv")))
# write.csv(datComm,(paste0(upPlace,"/tempOutput/Community CCB Work.csv")))
# write.csv(datCounty,(paste0(upPlace,"/tempOutput/County CCB Work.csv")))
# write.csv(datState,(paste0(upPlace,"/tempOutput/State CCB Work.csv")))

save(datTract,  file= path(myPlace,"/myData/",whichDat,"datTract.R"))
save(datComm,   file= path(myPlace,"/myData/",whichDat,"datComm.R"))
save(datCounty, file= path(myPlace,"/myData/",whichDat,"datCounty.R"))
save(datState,  file= path(myPlace,"/myData/",whichDat,"datState.R"))


# END ---------------------------------------------------------------------------------------------

# could make aL and aU like this, or as below based on an input file:
# aL            <- c(   0, 5,15,25,35,45,55,65,75,85)
# aU            <- c(-1,4,14,24,34,44,54,64,74,84,999)



