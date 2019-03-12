# ============================================================================
# "makeDatasets.R" file   
#
#            designate key constants, folder locations, and load packages     
#            load data mapping files                                          
#            load population denominator data                                 
#            load death data (cbdDat0)                                        
#            build functions for YLL and rate calcuations                     
#            contruct initial tract, community & county CBD data files        
#            process data and calculate age-adjusted rates                    
#            final merges and processing of main CBD data files               
#            export files for use in CBD app                              
#
#            Michael Samuel
#            2018
#
#=============================================================================


# -- Designate locations and load packages---------------------------------------------------------

whichDat <- "real"
subSite  <- FALSE

# EDIT SECURE DATA LOCATION AS NEEDED
SecureDataFile <- "G:/CCB/0.Secure.Data/myData/cbdDat0FULL.R"     
SecureDataFile <- "g:/0.Secure.Data/myData/cbdDat0FULL.R"  

STATE    <- "California"

myDrive <- getwd()  
myPlace <- paste0(myDrive,"/myCBD") 
upPlace <- paste0(myDrive,"/myUpstream") 

library(tidyverse)
library(epitools)
library(sqldf)

library(readxl)
library(fs)

yF   <- 100000  # rate constant 
pop5 <- 5       # 5 years
pop1 <- 1       # 1 year

yearGrp <- "2013-2017"

#-- LOAD STANDARDS AND DATA MAPPING FILES ---------------------------------------------------------

# add to technical notes the purposes and contents of each data mapping file 

# this "as.data.frame" below and elsewhere is really annoying.... but at least icdToGroup function below does not work otherwise;
# becuase the "tibble" is double precision or for some other reason this messes up; 
# and get error "Error: Can't use matrix or array for column indexing"


leMap      <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/Age to Life-Expectancy Linkage.xlsx"), sheet="LifeExpLink", range = cell_cols("A:B")))
yearMap    <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/Year to Year-Group Linkage.xlsx")))
geoMap     <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/County Codes to County Names Linkage.xlsx")))
cbdLinkCA  <- read.csv(paste0(myPlace,"/myInfo/Tract to Community Linkage.csv"),colClasses = "character")  # file linking MSSAs to census 
comName    <- unique(cbdLinkCA[,c("comID","comName")])                                    # dataframe linking comID and comName
ageMap     <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/Age Group Standard and US Standard 2000 Population.xlsx"),sheet = "data"))

#-- LOAD AND PROCESS POPULATION DATA --------------------------------------------------------------

# ungrouping important for subsequent data set merging
popTract         <- readRDS(path(upPlace,"/upData/popTract2013.RDS")) %>% ungroup() 
popTractSex      <- filter(popTract,ageG == "Total")
popTractSexAgeG  <- filter(popTract,ageG != "Total")

popCommSex       <- popTractSex     %>% group_by(yearG,county,comID,sex)      %>% summarise(pop=sum(pop))  %>% ungroup()  
popCommSexAgeG   <- popTractSexAgeG %>% group_by(yearG,county,comID,sex,ageG) %>% summarise(pop=sum(pop))  %>% ungroup() 

popCounty        <- readRDS(path(upPlace,"/upData/popCounty.RDS")) %>% ungroup() 
popCountySex     <- filter(popCounty,ageG == "Total")
popCountySexAgeG <- filter(popCounty,ageG != "Total")

popCounty.RACE        <- readRDS(path(upPlace,"/upData/popCounty_RE.RDS")) %>% ungroup() 
popCountySex.RACE     <- filter(popCounty.RACE,ageG == "Total")
popCountySexAgeG.RACE <- filter(popCounty.RACE,ageG != "Total")

popStandard         <- ageMap %>% mutate(ageG = paste0(lAge," - ",uAge))


 # == LOAD AND PROCESS DEATH DATA =================================================================

if (whichDat == "real") {
  load(SecureDataFile)
  cbdDat0 <- cbdDat0FULL    
}

if (whichDat == "fake") { 
  load(paste0(upPlace,"/upData/cbdDat0SAMP.R"))      
  cbdDat0 <- cbdDat0SAMP
}


# GEOID/COUNTY CORRECTION HERE =============================================

# (1) LA CENSUS TRACT TO RECODE
# 06037930401 should be recoded to  06037137000 in all data files
cbdDat0$GEOID[cbdDat0$GEOID=="06037930401"] <- "06037137000"
# (2) all occurences of "06037800325" in death data are Ventura, all are LA in pop data
# (3) fix county based on GEOID analysis here:
allWater <- c("06017990000","06037990300","06061990000","06083990000","06111990100")



# TODO CHANGE yearG to yearG5 thoughout app at some point

cbdDat0       <- mutate(cbdDat0,
                         sex     = c("Male","Female")[match(sex,c("M","F"))],
                         age     = as.numeric(age),                                                  # redundant...
                         ICD10   = as.character(ICD10),                                              # redundant...
                         comID   = cbdLinkCA[match(cbdDat0$GEOID,cbdLinkCA[,"GEOID"]),"comID"],   
                         yll     = leMap[match(cbdDat0$age,leMap[,"Age"]),"LE"],
                         yearG   = yearMap[match(year,yearMap[,"year"]),"yearGroup5"], 
                         yearG3  = yearMap[match(year,yearMap[,"year"]),"yearGroup3"] 
                        )


.cbdDat0Sex   <- mutate(cbdDat0, sex = "Total")
cbdDat0       <- bind_rows(cbdDat0,.cbdDat0Sex)

#RECONSIDER THIS AT SOME POINT
# .cbdDat0Race  <- mutate(cbdDat0, raceCode = "Total")
# cbdDat0       <- bind_rows(cbdDat0,.cbdDat0Race)




# Add Age-Group variable ----------------------------------------

aL            <-      ageMap$lAge     # lower age ranges
aU            <- c(-1,ageMap$uAge)    # upper age ranges, plus inital value of "-1" for lower limit
aLabs         <- paste(aL,"-",aU[-1]) # make label for ranges
aMark         <- findInterval(cbdDat0$age,aU,left.open = TRUE)  # vector indicating age RANGE value of each INDIVIDUAL age value
cbdDat0$ageG  <- aLabs[aMark]                                   # make new "ageG" variable based on two objects above 


# Map ICD-10 codes to GBD conditions ----------------------------

gbdMap0   <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/gbd.ICD.Map.xlsx"), sheet="main"))   # also have e.g. range="A1:J167"
allLabels <- sort(gbdMap0$LABEL[!is.na(gbdMap0$LABEL)])

mapICD    <- gbdMap0[!is.na(gbdMap0$CODE),c("CODE","regEx10")]

icdToGroup <- function(inputVectorICD10) {
  Cause   <- rep(NA,length(inputVectorICD10))
  for (i in 1:nrow(mapICD)) {Cause[grepl(mapICD[i,"regEx10"],inputVectorICD10)] <- mapICD[i,"CODE"] } 
  Cause}

cbdDat0$icdCODE  <- icdToGroup(inputVectorICD10=cbdDat0$ICD10)

table(cbdDat0$icdCODE,useNA = "ifany")
# nrow(filter(cbdDat0,ICD10 %in% c("","000","0000")))
cbdDat0$icdCODE[cbdDat0$ICD10 %in% c("","000","0000")] <- "cZ02"  # >3500 records have no ICD10 code -- label them as cZ for now
codeDoesntMap  <- filter(cbdDat0,is.na(icdCODE))
table(codeDoesntMap$ICD10,useNA = "ifany")

codeLast4      <- str_sub(cbdDat0$icdCODE,2,5)
nLast4         <- nchar(codeLast4)

cbdDat0          <- cbdDat0  %>% mutate(lev0  = "0",
                                        lev1  = str_sub(icdCODE,2,2),
                                        lev2  = str_sub(icdCODE,2,4),
                                        lev3  = ifelse(nLast4 == 4,codeLast4,NA)
                                       )

#saveRDS(cbdDat0,  file= path("h:/0.Secure.Data/myData/cbdDat0-INVESTIGATION-FILE.RDS"))
#saveRDS(cbdDat0,  file= path("g:/CCB/0.Secure.Data/myData/cbdDat0-INVESTIGATION-FILE.RDS"))

# Investigations
if (1==2){
  
STDS  <- filter(cbdDat0,icdCODE=="cA02a")

cbdDat0$icd3 <-  str_sub(cbdDat0$ICD10,1,3)
syph         <-  filter(cbdDat0,icd3 %in% c("A50","A51","A52","A53"),sex != "Total")
table(syph$year)
syphTable<- table(syph$year,syph$ageG)
write.csv(syphTable,"syphilisDeaths.csv")
}

# DATA CLEANING ISSUES (see at bottom of file) ----------------------------------------------------


# DEATH MEASURES FUNCTIONS =========================================================================


calculateYLLmeasures <- function(group_vars,levLab){
  
  dat <- cbdDat0 %>% group_by_(.dots = group_vars) %>% 
    summarize(Ndeaths = n() , 
              YLL     = sum(yll,   na.rm = TRUE),     # NEED TO ADD CIs
              mean.age = mean(age,na.rm=TRUE)
    ) %>%  ungroup 
 
    names(dat)[grep("lev", names(dat))] <- "CAUSE"
    dat$Level                           <- levLab
    dat %>%  data.frame

}

calculateRates <- function(inData,yearN){
  transform(inData, 
            YLLper      = yF*YLL/(yearN*pop),
            cDeathRate  = yF*Ndeaths/(yearN*pop),
            rateLCI     = yF*pois.approx(Ndeaths,yearN*pop, conf.level = 0.95)$lower,
            rateUCI     = yF*pois.approx(Ndeaths,yearN*pop, conf.level = 0.95)$upper
  )
}



# https://github.com/cran/epitools/blob/master/R/ageadjust.direct.R

ageadjust.direct.SAM <- function (count, pop, rate = NULL, stdpop, conf.level = 0.95) 
{
  if (missing(count) == TRUE & !missing(pop) == TRUE & is.null(rate) == TRUE)   count <- rate * pop
  if (missing(pop) == TRUE & !missing(count) == TRUE & is.null(rate) == TRUE)     pop <- count/rate
  if (is.null(rate) == TRUE & !missing(count) == TRUE & !missing(pop) == TRUE)  rate <- count/pop
  
  rate[is.na(pop)]   <- 0
  rate[is.null(pop)] <- 0
  pop[is.na(pop)]    <- 0
  pop[is.null(pop)]  <- 0
  
  alpha <- 1 - conf.level
  cruderate <- sum(count,na.rm=TRUE)/sum(pop,na.rm=TRUE)
  stdwt <- stdpop/sum(stdpop,na.rm=TRUE)
  dsr <- sum(stdwt * rate,na.rm=TRUE)
  dsr.var <- sum((stdwt^2) * (count/pop^2))
  dsr.se  <- sqrt(dsr.var)
  wm<- max(stdwt/pop)
  gamma.lci <- qgamma(alpha/2, shape = (dsr^2)/dsr.var, scale = dsr.var/dsr)
  gamma.uci <- qgamma(1 - alpha/2, shape = ((dsr+wm)^2)/(dsr.var+wm^2), 
                      scale = (dsr.var+wm^2)/(dsr+wm))
  
  c(crude.rate = cruderate, adj.rate = dsr, lci = gamma.lci, 
    uci = gamma.uci, se = dsr.se)
}




# == build COUNTY-level file ======================================================================

c.t1      <- calculateYLLmeasures(c("county","year","sex","lev0"),"lev0")
c.t2      <- calculateYLLmeasures(c("county","year","sex","lev1"),"lev1")
c.t3      <- calculateYLLmeasures(c("county","year","sex","lev2"),"lev2")
c.t4      <- calculateYLLmeasures(c("county","year","sex","lev3"),"lev3")
datCounty <- bind_rows(c.t1,c.t2,c.t3,c.t4)

s.t1      <- calculateYLLmeasures(c("year","sex","lev0"),"lev0")
s.t2      <- calculateYLLmeasures(c("year","sex","lev1"),"lev1")
s.t3      <- calculateYLLmeasures(c("year","sex","lev2"),"lev2")
s.t4      <- calculateYLLmeasures(c("year","sex","lev3"),"lev3")
datState  <- bind_rows(s.t1,s.t2,s.t3,s.t4)
datState$county = STATE

datCounty <- bind_rows(datCounty,datState)

# datCounty$causeName <- gbdMap0[match(datCounty$CAUSE,gbdMap0[,1]),"nameOnly"]  #  "needed?"

datCounty <- merge(datCounty,popCountySex,by = c("year","county","sex"))

datCounty <- calculateRates(datCounty,1)

# == build RACE-ETHNICITY COUNTY-level file ======================================================================

c.t1.RE      <- calculateYLLmeasures(c("county","yearG3","sex","raceCode","lev0"),"lev0")
c.t2.RE      <- calculateYLLmeasures(c("county","yearG3","sex","raceCode","lev1"),"lev1")
c.t3.RE      <- calculateYLLmeasures(c("county","yearG3","sex","raceCode","lev2"),"lev2")
c.t4.RE      <- calculateYLLmeasures(c("county","yearG3","sex","raceCode","lev3"),"lev3")
datCounty.RE <- bind_rows(c.t1.RE,c.t2.RE,c.t3.RE,c.t4.RE)

s.t1.RE      <- calculateYLLmeasures(c("yearG3","sex","raceCode","lev0"),"lev0")
s.t2.RE      <- calculateYLLmeasures(c("yearG3","sex","raceCode","lev1"),"lev1")
s.t3.RE      <- calculateYLLmeasures(c("yearG3","sex","raceCode","lev2"),"lev2")
s.t4.RE      <- calculateYLLmeasures(c("yearG3","sex","raceCode","lev3"),"lev3")
datState.RE  <- bind_rows(s.t1.RE,s.t2.RE,s.t3.RE,s.t4.RE)
datState.RE$county = STATE

datCounty.RE <- bind_rows(datCounty.RE, datState.RE)
datCounty.RE <- merge(datCounty.RE, popCountySex.RACE, by = c("yearG3","county","sex","raceCode"))
datCounty.RE <- calculateRates(datCounty.RE,1)



# == build COMMUNITY-level file ===================================================================


c.t1      <- calculateYLLmeasures(c("comID","yearG","sex","lev0"),"lev0")  #removed "county",
c.t2      <- calculateYLLmeasures(c("comID","yearG","sex","lev1"),"lev1")
c.t3      <- calculateYLLmeasures(c("comID","yearG","sex","lev2"),"lev2")
datComm  <- bind_rows(c.t1,c.t2,c.t3) %>%
                filter(yearG == yearGrp)  %>%   # 2013-2017 ONLY!!!
                arrange(comID,yearG,CAUSE)

datComm  <- merge(datComm,popCommSex,by = c("yearG","comID","sex"),all=TRUE)

datComm  <- calculateRates(datComm,5)

# add community names  POSSIBLE REMOVE
datComm  <- merge(datComm, comName, by = "comID",all=TRUE) %>%
  arrange(comID,yearG,CAUSE)


# == build TRACT-level file =======================================================================


c.t1      <- calculateYLLmeasures(c("GEOID","yearG","sex","lev0"),"lev0")
c.t2      <- calculateYLLmeasures(c("GEOID","yearG","sex","lev1"),"lev1")

datTract  <- bind_rows(c.t1,c.t2) %>% 
  filter(yearG == yearGrp)  %>%    # 2013-2017 ONLY!!!
  arrange(GEOID,yearG,CAUSE)
# NOTE -- includes many with NA GEOID

# MERGE Death and Population files
datTract <- merge(datTract,popTractSex,by = c("yearG","GEOID","sex"),all=TRUE)                     

# Calculate Rates
datTract <- calculateRates(datTract,5) %>%
  arrange(GEOID,yearG,CAUSE)


# == AGE ADJUSTED ("AA") RATES =========================================================================================

# makes dataframe of all possible combinations of county, year, CAUSE, and ageG 

year     <- data.frame(year     = 2000:2017) # these "vectors" need to be dataframes for the sq merge below to work
yearG    <- data.frame(yearG    = yearGrp)
yearG3   <- data.frame(yearG3   = sort(unique(cbdDat0$yearG3)))
CAUSE1   <- data.frame(CAUSE    = allLabels) 
CAUSE2   <- data.frame(CAUSE    = CAUSE1[nchar(as.character(CAUSE1$CAUSE)) < 4,])
CAUSE3   <- data.frame(CAUSE    = CAUSE1[nchar(as.character(CAUSE1$CAUSE)) < 2,])
sex      <- data.frame(sex      = c("Male","Female","Total"))
ageG     <- data.frame(ageG     = sort(unique(cbdDat0$ageG)))
county   <- data.frame(county   = c(geoMap$countyName,"California"))         
comID    <- data.frame(comID    = unique(cbdLinkCA[,"comID"]))
GEOID    <- data.frame(GEOID    = cbdLinkCA[,"GEOID"])
raceCode <- data.frame(raceCode = sort(unique(cbdDat0$raceCode)))

# other cool approach from Adam:
# fullMatCounty <- Reduce(function(...) merge(..., all = TRUE), list(county, year, CAUSE, sex, ageG))
fullMatCounty <- sqldf(" select * from  county cross join year  cross join CAUSE1 cross join sex cross join ageG")
fullMatComm   <- sqldf(" select * from  comID  cross join yearG cross join CAUSE2 cross join sex cross join ageG")
fullMatTract  <- sqldf(" select * from  GEOID  cross join yearG cross join CAUSE3 cross join sex cross join ageG")


#######CAUSE CHARACTER##################

fullMatCounty <- mutate(fullMatCounty, county = as.character(county),                             CAUSE = as.character(CAUSE), sex = as.character(sex), ageG   = as.character(ageG), tester = 0)
fullMatComm   <- mutate(fullMatComm,   comID  = as.character(comID), yearG = as.character(yearG), CAUSE = as.character(CAUSE), sex = as.character(sex), ageG   = as.character(ageG), tester = 0)
fullMatTract  <- mutate(fullMatTract,  GEOID  = as.character(GEOID), yearG = as.character(yearG), CAUSE = as.character(CAUSE), sex = as.character(sex), ageG   = as.character(ageG), tester = 0)

# County age deaths -------------------------------------------------------------------------------

tA1      <- cbdDat0 %>% group_by(county,year, sex, ageG,CAUSE=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA2      <- cbdDat0 %>% group_by(county,year, sex, ageG,CAUSE=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA3      <- cbdDat0 %>% group_by(county,year, sex, ageG,CAUSE=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA4      <- cbdDat0 %>% group_by(county,year, sex, ageG,CAUSE=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA5      <- cbdDat0 %>% group_by(       year, sex, ageG,CAUSE=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA6      <- cbdDat0 %>% group_by(       year, sex, ageG,CAUSE=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA7      <- cbdDat0 %>% group_by(       year, sex, ageG,CAUSE=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA8      <- cbdDat0 %>% group_by(       year, sex, ageG,CAUSE=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)

datAA1 <- bind_rows(tA1,tA2,tA3,tA4,tA5,tA6,tA7,tA8)  %>% ungroup()  # UNGROUP HERE!!!!

# DATA CLEANING ISSUES as above
datAA1 <- filter(datAA1,!is.na(ageG))   # remove 403 records with missing age (0.065% of deaths)  -- impact of this?
# datAA1 <- filter(datAA1,!is.na(CAUSE))  # remove 6955 records with missing CAUSE
datAA1 <- filter(datAA1,!is.na(county)) # remove 758 records with missing county
# datAA1 <- filter(datAA1,!is.na(sex))    # remove 


ageCounty   <- full_join(fullMatCounty,datAA1 ,by = c("county","year","sex","ageG","CAUSE"))  %>%    # merge death data and "fullMatCounty"
                full_join(popCountySexAgeG, by = c("county","year","sex","ageG") )         %>%    # merge population
                 full_join(popStandard[,c("ageG","US2000POP")],          by="ageG")                  # merge standard population
 

ageCounty$Ndeaths[is.na(ageCounty$Ndeaths)] <- 0    # if NA deaths in strata change to "0"
ageCounty$YLL[is.na(ageCounty$YLL)]         <- 0    # if NA deaths in strata change to "0"


# saveRDS(ageCounty, file= path(myPlace,"/myData/",whichDat,"forZev.ageCounty.RDS"))
# githubURL <- "https://raw.githubusercontent.com/mcSamuelDataSci/CACommunityBurden/master/myCBD/myData/fake/forZev.ageCounty.RDS"
# download.file(githubURL,"temp.rds", method="curl")
# ageCounty <- readRDS("temp.rds")


countyAA <- ageCounty %>% group_by(county,year,sex,CAUSE) %>%
  summarize(oDeaths = sum(Ndeaths,na.rm=TRUE),
            aRate   = ageadjust.direct.SAM(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct.SAM(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct.SAM(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE     = ageadjust.direct.SAM(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000, 
            YLL.adj.rate   = ageadjust.direct.SAM(count=YLL, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) # CONFIRM


countyAA <- countyAA[!(countyAA$oDeaths==0),c("county","year","sex","CAUSE","aRate","aLCI","aUCI","aSE","YLL.adj.rate")]  # remove strata with no deaths and select columns  

#tester <- filter(ageCounty,year==2015,county=="Alameda",sex=="Male",CAUSE==0) 
#ageadjust.direct.SAM(count=tester$Ndeaths, pop=tester$pop, rate = NULL, stdpop=tester$US2000POP, conf.level = 0.95)*100000


##################  RACE ##################################

fullMatCounty <- sqldf(" select * from  county cross join yearG3  cross join CAUSE1 cross join sex cross join ageG join raceCode")

fullMatCounty <- mutate(fullMatCounty, county = as.character(county), yearG3 = as.character(yearG3),CAUSE = as.character(CAUSE), sex = as.character(sex), ageG   = as.character(ageG), raceCode = as.character(raceCode),tester = 0)


# County age deaths -------------------------------------------------------------------------------

tA1      <- cbdDat0 %>% group_by(county,yearG3, sex, ageG, raceCode, CAUSE=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA2      <- cbdDat0 %>% group_by(county,yearG3, sex, ageG, raceCode, CAUSE=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA3      <- cbdDat0 %>% group_by(county,yearG3, sex, ageG, raceCode, CAUSE=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA4      <- cbdDat0 %>% group_by(county,yearG3, sex, ageG, raceCode, CAUSE=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 

tA5      <- cbdDat0 %>% group_by(       yearG3, sex, ageG, raceCode, CAUSE=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA6      <- cbdDat0 %>% group_by(       yearG3, sex, ageG, raceCode, CAUSE=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA7      <- cbdDat0 %>% group_by(       yearG3, sex, ageG, raceCode, CAUSE=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA8      <- cbdDat0 %>% group_by(       yearG3, sex, ageG, raceCode, CAUSE=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)

datAA1 <- bind_rows(tA1,tA2,tA3,tA4,tA5,tA6,tA7,tA8)  %>% ungroup()  # UNGROUP HERE!!!!

datAA1 <- filter(datAA1,!is.na(ageG))   # remove 403 records with missing age (0.065% of deaths)  -- impact of this?
# datAA1 <- filter(datAA1,!is.na(county)) # remove 758 records with missing county ### removed this from RACE DATA......


ageCounty.RE   <- full_join(fullMatCounty,datAA1 ,by = c("county","yearG3","sex","ageG","raceCode","CAUSE"))  %>%    # merge death data and "fullMatCounty"
  full_join(popCountySexAgeG.RACE, by = c("county","yearG3","sex","ageG","raceCode") )         %>%    # merge population
  full_join(popStandard[,c("ageG","US2000POP")],          by="ageG")                  # merge standard population


ageCounty.RE$Ndeaths[is.na(ageCounty.RE$Ndeaths)] <- 0    # if NA deaths in strata change to "0"
ageCounty.RE$YLL[is.na(ageCounty.RE$YLL)]         <- 0    # if NA deaths in strata change to "0"


# saveRDS(ageCounty, file= path(myPlace,"/myData/",whichDat,"forZev.ageCounty.RDS"))
# githubURL <- "https://raw.githubusercontent.com/mcSamuelDataSci/CACommunityBurden/master/myCBD/myData/fake/forZev.ageCounty.RDS"
# download.file(githubURL,"temp.rds", method="curl")
# ageCounty <- readRDS("temp.rds")


countyAA.RE <- ageCounty.RE %>% group_by(county,yearG3,sex,raceCode,CAUSE) %>%
  summarize(oDeaths = sum(Ndeaths,na.rm=TRUE),
            aRate   = ageadjust.direct.SAM(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct.SAM(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct.SAM(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE     = ageadjust.direct.SAM(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000, 
            YLL.adj.rate   = ageadjust.direct.SAM(count=YLL, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) # CONFIRM


countyAA.RE <- countyAA.RE[!(countyAA.RE$oDeaths==0),c("county","yearG3","sex","raceCode","CAUSE","aRate","aLCI","aUCI","aSE","YLL.adj.rate")]  








##################  END RACE




# Community age deaths ----------------------------------------------------------------------------

tA1      <- cbdDat0 %>% group_by(comID, yearG, sex, ageG,CAUSE=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) )   
tA2      <- cbdDat0 %>% group_by(comID, yearG, sex, ageG,CAUSE=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA3      <- cbdDat0 %>% group_by(comID, yearG, sex, ageG,CAUSE=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 

datAA1 <- bind_rows(tA1,tA2,tA3)  %>% filter(comID != "")  
#datAA1 <- na.omit(datAA1)           

ageComm   <- full_join(fullMatComm,datAA1,by = c("comID","yearG","sex","ageG","CAUSE"))  %>% 
                filter(yearG == yearGrp)                                                 %>%
              full_join(popCommSexAgeG, by = c("comID","yearG","sex","ageG"))         %>% # population
               full_join(popStandard[,c("ageG","US2000POP")],by="ageG")                      # standard population


ageComm$Ndeaths[is.na(ageComm$Ndeaths)] <- 0    # if NA deaths in strata change to "0"
ageComm$YLL[is.na(ageComm$YLL)]         <- 0    # if NA deaths in strata change to "0"

commAA <- ageComm %>% group_by(comID,yearG,sex,CAUSE) %>%
  summarize(oDeaths = sum(Ndeaths,na.rm=TRUE),
            aRate   = ageadjust.direct.SAM(count=Ndeaths, pop=pop*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct.SAM(count=Ndeaths, pop=pop*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct.SAM(count=Ndeaths, pop=pop*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE     = ageadjust.direct.SAM(count=Ndeaths, pop=pop*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000, 
            
            YLL.adj.rate   = ageadjust.direct.SAM(count=YLL, pop=pop*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) 
commAA <- commAA[!(commAA$oDeaths==0),c("comID","yearG","sex","CAUSE","oDeaths","aRate","aLCI","aUCI","aSE","YLL.adj.rate")]  

# removes rows with aRate = inf HERE there are only ALPINE 
commAA  <- commAA[!(commAA$aRate > 10000),]

#tester <- filter(ageComm,yearG==yearGrp,comID=="104",sex=="Female",CAUSE==0) 
#ageadjust.direct.SAM(count=tester$Ndeaths, pop=tester$pop, rate = NULL, stdpop=tester$US2000POP, conf.level = 0.95)*100000

# Tract age deaths -----------------------------------------------------------------------------------------------------

tA1      <- cbdDat0 %>% group_by(GEOID, yearG, sex, ageG,CAUSE=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA2      <- cbdDat0 %>% group_by(GEOID, yearG, sex, ageG,CAUSE=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 

datAA1 <- bind_rows(tA1,tA2)  %>% filter(GEOID != "")  

ageTract   <- full_join(fullMatTract,datAA1,by = c("GEOID","yearG","sex","ageG","CAUSE"))  %>% 
                filter(yearG == yearGrp)                                                   %>%
               full_join(popTractSexAgeG,by = c("GEOID","yearG","sex","ageG"))          %>%   # add population
                full_join(popStandard[,c("ageG","US2000POP")],by="ageG")                        # add standard population 

ageTract$Ndeaths[is.na(ageTract$Ndeaths)] <- 0    # if NA deaths in strata change to "0"
ageTract$YLL[is.na(ageTract$YLL)]         <- 0    # if NA deaths in strata change to "0"

tractAA <- ageTract %>% group_by(GEOID,yearG,sex,CAUSE) %>%
  summarize(oDeaths = sum(Ndeaths,na.rm=TRUE),
            aRate   = ageadjust.direct.SAM(count=Ndeaths, pop=pop*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct.SAM(count=Ndeaths, pop=pop*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct.SAM(count=Ndeaths, pop=pop*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            YLL.adj.rate   = ageadjust.direct.SAM(count=YLL, pop=pop*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) 
tractAA <- tractAA[!(tractAA$oDeaths==0),c("GEOID","yearG","sex","CAUSE","oDeaths","aRate","aLCI","aUCI","YLL.adj.rate")]  


# removes rows with aRate = inf, or infinity becuase some population strata is 0 AND some other odd strata
tractAA  <- tractAA[!(tractAA$aRate > 5000),]
tractAA  <- tractAA[!(is.na(tractAA$aRate)),]

# -- Merge adjusted rates into main data files and final Clean Up ----------------------------------------------------------

datTract  <- merge(datTract,  tractAA ,by = c("GEOID","yearG","sex","CAUSE"),all=TRUE) %>% 
                    mutate_if(is.numeric, signif,digits=4) %>%
                    filter(!is.na(county))  %>%  # REMOVE ALL out-of-state GEOID and missing GEOID
                    filter(!is.na(CAUSE)) # removes about 130 records with bad/no GEOID and/or wrong County based on GEOID

datComm   <- merge(datComm,    commAA ,by = c("comID","yearG","sex","CAUSE"),all=TRUE) %>%
                   mutate_if(is.numeric, signif,digits=4) %>%
                   filter(!is.na(county)) #  as above


# -- Crude and Adjusted SMR Calculations for County Data Set ---------------------------------

datCounty <- merge(datCounty,countyAA ,by = c("county","year","sex","CAUSE"),all=TRUE)

datState  <-  datCounty  %>% filter(county == STATE) %>%
  mutate(stateCrudeRate    = cDeathRate,
         stateAdjustedRate = aRate) %>%
  select(year,sex,Level,CAUSE,stateCrudeRate,stateAdjustedRate)

if (!subSite) save(datState, file= paste0(upPlace,"/upData/datState.R"))
if ( subSite) load(file= paste0(upPlace,"/upData/datState.R"))

datCounty            <- merge(datCounty,datState,by = c("year","sex","Level","CAUSE")) %>%
                        mutate(SMRcrude = cDeathRate / stateCrudeRate,
                               SMR      = aRate      / stateAdjustedRate)


# -- Final Clean Up of County File  ----------------------------------------------------------

datCounty <-       datCounty %>% 
                   filter(!(is.na(CAUSE)))  %>%                                  # removes "Level3" NA (most 'causes' are NA on Level3) 
                   select(-ageG,-stateCrudeRate,-stateAdjustedRate) %>%
                   mutate_if(is.numeric, signif,digits=4)                        %>%  # much smaller file and easier to read
                   mutate(county = ifelse(county==STATE, toupper(STATE),county)      # e.g. California --> CALIFORNIA
                   )


# -- Merge and Clean County RACE ETHNICITY Data Set ---------------------------------

datCounty.RE <- merge(datCounty.RE,countyAA.RE ,by = c("county","yearG3","sex","raceCode","CAUSE"),all=TRUE)

datCounty.RE <- datCounty.RE %>% 
  filter(!(is.na(CAUSE)))  %>%                                  # removes "Level3" NA (most 'causes' are NA on Level3) 
  select(-ageG) %>%
  mutate_if(is.numeric, signif,digits=4)                        %>%  # much smaller file and easier to read
  mutate(county = ifelse(county==STATE, toupper(STATE),county)      # e.g. California --> CALIFORNIA
  )




# == Final Data Clean Up and Export ==================================================================================


# "SMALL CELL and "RISKY CAUSE" supression ----------
# xCause0 <- c(14,41,50,139,4,49,192)
# xCause1 <- c(xCause0,10)
# datTract  <- filter(datTract, !(CAUSE %in% xCause1))
# datComm   <- filter(datComm,  !(CAUSE %in% xCause1))
# datCounty <- filter(datCounty,!(CAUSE %in% xCause0))



if (1==2){
datTract  <- readRDS(path(myPlace,"/myData/",whichDat,"datTract.RDS"))
datComm   <- readRDS(path(myPlace,"/myData/",whichDat,"datComm.RDS"))
datCounty <- readRDS(path(myPlace,"/myData/",whichDat,"datCounty.RDS"))
}


criticalNum <- 11


datTract     <- filter(datTract,     Ndeaths >= criticalNum)
datComm      <- filter(datComm,      Ndeaths >= criticalNum)
datCounty    <- filter(datCounty,    Ndeaths >= criticalNum)
datCounty.RE <- filter(datCounty.RE, Ndeaths >= criticalNum)


datTract     <- filter(datTract,    !(CAUSE=="A09" & sex %in% c("Male","Female")))
datComm      <- filter(datComm,     !(CAUSE=="A09" & sex %in% c("Male","Female")))
datCounty    <- filter(datCounty,   !(CAUSE=="A09" & sex %in% c("Male","Female")))
datCounty.RE <- filter(datCounty.RE,!(CAUSE=="A09" & sex %in% c("Male","Female")))



# 
# 
# datTract <-  datTract %>% mutate(Ndeaths     = ifelse(Ndeaths < criticalNum,NA,Ndeaths),
#                                  cDeathRate  = ifelse(Ndeaths < criticalNum,NA,cDeathRate),
#                                  YLL         = ifelse(Ndeaths < criticalNum,NA,YLL),
#                                  YLLper      = ifelse(Ndeaths < criticalNum,NA,YLLper),
#                                  rateLCI     = ifelse(Ndeaths < criticalNum,NA,rateLCI),
#                                  rateUCI     = ifelse(Ndeaths < criticalNum,NA,rateUCI),
#                                  mean.age    = ifelse(Ndeaths < criticalNum,NA,mean.age)
#                                  )
# 
# 
# datComm  <-  datComm  %>%  mutate(Ndeaths     = ifelse(Ndeaths < criticalNum,NA,Ndeaths),
#                                   cDeathRate  = ifelse(Ndeaths < criticalNum,NA,cDeathRate),
#                                   YLL         = ifelse(Ndeaths < criticalNum,NA,YLL),
#                                   YLLper      = ifelse(Ndeaths < criticalNum,NA,YLLper),
#                                   rateLCI     = ifelse(Ndeaths < criticalNum,NA,rateLCI),
#                                   rateUCI     = ifelse(Ndeaths < criticalNum,NA,rateUCI),
#                                   mean.age    = ifelse(Ndeaths < criticalNum,NA,mean.age)
#                                   )
# 
# datCounty <- datCounty %>% mutate(Ndeaths     = ifelse(Ndeaths < criticalNum,NA,Ndeaths),
#                                   cDeathRate  = ifelse(Ndeaths < criticalNum,NA,cDeathRate),
#                                   YLL         = ifelse(Ndeaths < criticalNum,NA,YLL),
#                                   YLLper      = ifelse(Ndeaths < criticalNum,NA,YLLper),
#                                   SMR         = ifelse(Ndeaths < criticalNum,NA,SMR),
#                                   rateLCI     = ifelse(Ndeaths < criticalNum,NA,rateLCI),
#                                   rateUCI     = ifelse(Ndeaths < criticalNum,NA,rateUCI),
#                                   mean.age    = ifelse(Ndeaths < criticalNum,NA,mean.age)
#                                   )
# 


# Quick fix to replace with Version Beta 1.1
# eliminates pop 0 and therefore infinity rates

datTract  <- filter(datTract,pop>0)

# No!
#  datCounty <- filter(datCounty,!is.na(Ndeaths))

saveRDS(datTract,     file= path(myPlace,"/myData/",whichDat,"datTract.RDS"))
saveRDS(datComm,      file= path(myPlace,"/myData/",whichDat,"datComm.RDS"))
saveRDS(datCounty,    file= path(myPlace,"/myData/",whichDat,"datCounty.RDS"))
saveRDS(datCounty.RE, file= path(myPlace,"/myData/",whichDat,"datCounty.RE.RDS"))



# # 
# datTract$causeName <- gbdMap0[match(datTract$CAUSE,gbdMap0[,"LABEL"]),"causeList"]
# datComm$causeName <- gbdMap0[match(datComm$CAUSE,gbdMap0[,"LABEL"]),"causeList"]
# datCounty$causeName <- gbdMap0[match(datCounty$CAUSE,gbdMap0[,"LABEL"]),"causeList"]
#  
#  write.csv(datTract,(paste0(upPlace,"/tempOutput/Tract CCB Work.csv")))
#  write.csv(datComm,(paste0(upPlace,"/tempOutput/Community CCB Work.csv")))
#  write.csv(datCounty,(paste0(upPlace,"/tempOutput/County CCB Work.csv")))

# END ===================================================================================================================



 # DATA CLEANING ISSUES ----------------------------------
 
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
 
# Potentially useful old code bits:


# could make aL and aU like this, or as below based on an input file:
# aL            <- c(   0, 5,15,25,35,45,55,65,75,85)
# aU            <- c(-1,4,14,24,34,44,54,64,74,84,999)


# "Manual' calcuation of age-adjustment
# popStandard <- readRDS(paste0(upPlace,"/upData/popStandard.RDS"))
# ageCounty   <- merge(ageCounty,popStandard,by = c("ageG"),all=TRUE)  # merge with "Standard" population

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




