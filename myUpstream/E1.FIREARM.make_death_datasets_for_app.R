# SPECAIL RUN TO CREATE DATA SET SPECIFICALLY FOR FIREARM-RELATED DEATHS


# == Designate locations and load packages  =======================================================

whichDat <- "real"

# EDIT SECURE DATA LOCATION AS NEEDED
securePath     <- "g:/0.Secure.Data/"
secureDataFile <- paste0(securePath,"myData/cbdDat0FULL.R") 

STATE    <- "California"

myDrive <- getwd()  
myPlace <- paste0(myDrive,"/myCBD") 
upPlace <- paste0(myDrive,"/myUpstream") 

library(readr)
library(stringr)
library(dplyr)
library(epitools)
library(sqldf)
library(readxl)
library(fs)

yF   <- 100000  # rate constant 
pop5 <- 5       # 5 years
pop1 <- 1       # 1 year

yearGrp <- c("2004-2008","2009-2013","2014-2018")

criticalNum <- 11


#== LOAD STANDARDS AND DATA MAPPING FILES =========================================================

leMap      <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/Age to Life-Expectancy Linkage.xlsx"), sheet="LifeExpLink", range = cell_cols("A:B")))
yearMap    <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/Year to Year-Group Linkage.xlsx")))
geoMap     <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/County Codes to County Names Linkage.xlsx")))
cbdLinkCA  <- read.csv(paste0(myPlace,"/myInfo/Tract to Community Linkage.csv"),colClasses = "character")  # file linking MSSAs to census 
comName    <- unique(cbdLinkCA[,c("comID","comName")])                                    # dataframe linking comID and comName
ageMap     <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/Age Group Standard and US Standard 2000 Population.xlsx"),sheet = "data"))
ageMap_EDU  <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/Age Group Standard and US Standard 2000 Population.xlsx"),sheet = "dataEducation"))


#== LOAD AND PROCESS POPULATION DATA ==============================================================

# ungrouping important for subsequent data set merging

popCounty        <- readRDS(path(upPlace,"/upData/popCounty.RDS")) %>% ungroup() 
popCountySex     <- filter(popCounty,ageG == "Total") %>% select(-ageG) # no need for ageG
popCountySexAgeG <- filter(popCounty,ageG != "Total")

popCounty.RACE        <- readRDS(path(upPlace,"/upData/popCounty_RE_3year.RDS")) %>% ungroup() 
popCountySex.RACE     <- filter(popCounty.RACE,ageG == "Total")
popCountySexAgeG.RACE <- filter(popCounty.RACE,ageG != "Total")


popCountySex_3year      <- popCountySex %>%
                           mutate(yearG3  = yearMap[match(year,yearMap[,"year"]),"yearGroup3"]) %>%
                           group_by(yearG3,county,sex) %>%
                           summarize(pop=sum(pop))

popCountySexAgeG_3year  <- popCountySexAgeG %>%
                           mutate(yearG3  = yearMap[match(year,yearMap[,"year"]),"yearGroup3"]) %>%
                           group_by(yearG3,county,sex,ageG) %>%
                           summarize(pop=sum(pop))

popCountySex_5year    <- popCountySex %>%
                           mutate(yearG5  = yearMap[match(year,yearMap[,"year"]),"yearGroup5"]) %>% 
                           group_by(yearG5,county,sex) %>%
                           summarize(pop=sum(pop))

popCountySexAgeG_5year  <- popCountySexAgeG %>%
                           mutate(yearG5  = yearMap[match(year,yearMap[,"year"]),"yearGroup5"]) %>%
                           group_by(yearG5,county,sex,ageG) %>%
                           summarize(pop=sum(pop))


popCounty_EDU        <- readRDS(path(upPlace,"/upData/popCounty_Education.RDS")) %>% ungroup() 
popCountySex_EDU     <- filter(popCounty_EDU,ageG_EDU == "Total") %>% select(-ageG_EDU) # no need for ageG
popCountySexAgeG_EDU <- filter(popCounty_EDU,ageG_EDU != "Total")

popTract         <- readRDS(path(upPlace,"/upData/popTract.RDS")) %>% ungroup() 
popTractSex      <- filter(popTract,ageG == "Total")
popTractSexAgeG  <- filter(popTract,ageG != "Total")

popCommSex       <- popTractSex     %>% group_by(yearG5,county,comID,sex)      %>% summarise(pop=sum(pop))  %>% ungroup()  
popCommSexAgeG   <- popTractSexAgeG %>% group_by(yearG5,county,comID,sex,ageG) %>% summarise(pop=sum(pop))  %>% ungroup() 

popStandard         <- ageMap    %>% mutate(ageG = paste0(lAge," - ",uAge))
popStandard_EDU     <- ageMap_EDU %>% mutate(ageG_EDU = paste0(lAge," - ",uAge))


 # == LOAD AND PROCESS DEATH DATA =================================================================

if (whichDat == "real") {
  load(secureDataFile)
  cbdDat0 <- cbdDat0FULL    
}

if (whichDat == "fake") { 
  load(paste0(upPlace,"/upData/cbdDat0SAMP.R"))      
  cbdDat0 <- cbdDat0SAMP
}


# -- GEOID/COUNTY CORRECTION HERE ---------------------------------------------
# TODO TODO TODO
# (1) LA CENSUS TRACT TO RECODE
# 06037930401 should be recoded to  06037137000 in all data files
cbdDat0$GEOID[cbdDat0$GEOID=="06037930401"] <- "06037137000"
# (2) all occurences of "06037800325" in death data are Ventura, all are LA in pop data
# (3) fix county based on GEOID analysis here:
allWater <- c("06017990000","06037990300","06061990000","06083990000","06111990100")


# -- RECODES AND CALCULATIONS -------------------------------------------------

cbdDat0       <- mutate(cbdDat0,
                         sex     = c("Male","Female")[match(sex,c("M","F"))],
                         age     = as.numeric(age),                                                  # redundant...
                         ICD10   = as.character(ICD10),                                              # redundant...
                         comID   = cbdLinkCA[match(cbdDat0$GEOID,cbdLinkCA[,"GEOID"]),"comID"],   
                       # yll     = leMap[match(cbdDat0$age,leMap[,"Age"]),"LE"],
                         yll     = ifelse(age > 75, 0, 75-age),
                         yearG5  = yearMap[match(year,yearMap[,"year"]),"yearGroup5"], 
                         yearG3  = yearMap[match(year,yearMap[,"year"]),"yearGroup3"],
                         education = ifelse(education == 8,7,education)  # one "Graduate or professional degree" category for now
                        ) %>%
                rename(eduCode = education) 


cbdDat0Sex   <- mutate(cbdDat0, sex = "Total")
cbdDat0       <- bind_rows(cbdDat0, cbdDat0Sex)


# -- Add Age-Group variable ---------------------------------------------------

aL            <-      ageMap$lAge     # lower age ranges
aU            <- c(-1,ageMap$uAge)    # upper age ranges, plus inital value of "-1" for lower limit
aLabs         <- paste(aL,"-",aU[-1]) # make label for ranges
aMark         <- findInterval(cbdDat0$age,aU,left.open = TRUE)  # vector indicating age RANGE value of each INDIVIDUAL age value
cbdDat0$ageG  <- aLabs[aMark]                                   # make new "ageG" variable based on two objects above 

aL_EDU            <-     ageMap_EDU$lAge     # lower age ranges
aU_EDU            <- c(-1,ageMap_EDU$uAge)     # upper age ranges, plus inital value of "-1" for lower limit
aLabs_EDU         <- c("less",paste(aL_EDU,"-",aU_EDU[-1])) # make label for ranges
aMark_EDU         <- findInterval(cbdDat0$age,c(-1,24,aU_EDU[2:5]),left.open = TRUE)  # vector indicating age RANGE value of each INDIVIDUAL age value
cbdDat0$ageG_EDU  <- aLabs_EDU[aMark_EDU]  

# -- Map ICD-10 codes to GBD conditions ----------------------------------------

gbdMap0   <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/gbd.ICD.Map.FIREARM.xlsx"), sheet="main"))   
allLabels <- sort(gbdMap0$LABEL[!is.na(gbdMap0$LABEL)])

mapICD    <- gbdMap0[!is.na(gbdMap0$CODE),c("CODE","regEx10")]

icdToGroup <- function(inputVectorICD10) {
  Cause   <- rep(NA,length(inputVectorICD10))
  for (i in 1:nrow(mapICD)) {Cause[grepl(mapICD[i,"regEx10"],inputVectorICD10)] <- mapICD[i,"CODE"] } 
  Cause}

cbdDat0$icdCODE  <- icdToGroup(inputVectorICD10=cbdDat0$ICD10)

codeLast4      <- str_sub(cbdDat0$icdCODE,2,5)
nLast4         <- nchar(codeLast4)

cbdDat0          <- cbdDat0  %>% mutate(lev0  = "0",
                                        lev1  = str_sub(icdCODE,2,2),
                                        lev2  = str_sub(icdCODE,2,4),
                                        lev3  = ifelse(nLast4 == 4,codeLast4,NA)
                                       )

# DEATH MEASURES FUNCTIONS =========================================================================
# ==================================================================================================

calculateYLLmeasures <- function(group_vars,levLab){
  
  dat <- cbdDat0 %>% group_by(.dots = group_vars) %>% 
  #  dat <- cbdDat0 %>% group_by_(.dots = group_vars) %>% 
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



# == CALCULATE CRUDE RATES ========================================================================
# =================================================================================================


# -- COUNTY -------------------------------------------------------------------
# -----------------------------------------------------------------------------

c.t1      <- calculateYLLmeasures(c("county","year","sex","lev0"),"lev0")
c.t2      <- calculateYLLmeasures(c("county","year","sex","lev1"),"lev1")
c.t3      <- calculateYLLmeasures(c("county","year","sex","lev2"),"lev2")
c.t4      <- calculateYLLmeasures(c("county","year","sex","lev3"),"lev3")
datCounty <- bind_rows(c.t1,c.t2,c.t3,c.t4)

s.t1      <- calculateYLLmeasures(c(         "year","sex","lev0"),"lev0")
s.t2      <- calculateYLLmeasures(c(         "year","sex","lev1"),"lev1")
s.t3      <- calculateYLLmeasures(c(         "year","sex","lev2"),"lev2")
s.t4      <- calculateYLLmeasures(c(         "year","sex","lev3"),"lev3")
datState  <- bind_rows(s.t1,s.t2,s.t3,s.t4)
datState$county = STATE

datCounty <- bind_rows(datCounty,datState)

# MERGE Death and Population files
datCounty <- merge(datCounty,popCountySex,by = c("year","county","sex"))

# CALCULATE RATES
datCounty <- calculateRates(datCounty,1)

# -- COUNTY 3-YEAR -------------------------------------------------------------
# ------------------------------------------------------------------------------

c.t1      <- calculateYLLmeasures(c("county","yearG3 ","sex","lev0"),"lev0")
c.t2      <- calculateYLLmeasures(c("county","yearG3 ","sex","lev1"),"lev1")
c.t3      <- calculateYLLmeasures(c("county","yearG3 ","sex","lev2"),"lev2")
c.t4      <- calculateYLLmeasures(c("county","yearG3 ","sex","lev3"),"lev3")
datCounty_3year <- bind_rows(c.t1,c.t2,c.t3,c.t4)

s.t1      <- calculateYLLmeasures(c(         "yearG3 ","sex","lev0"),"lev0")
s.t2      <- calculateYLLmeasures(c(         "yearG3 ","sex","lev1"),"lev1")
s.t3      <- calculateYLLmeasures(c(         "yearG3 ","sex","lev2"),"lev2")
s.t4      <- calculateYLLmeasures(c(         "yearG3 ","sex","lev3"),"lev3")
datState_3year  <- bind_rows(s.t1,s.t2,s.t3,s.t4)
datState_3year$county = STATE

datCounty_3year <- bind_rows(datCounty_3year,datState_3year)
datCounty_3year <- merge(datCounty_3year, popCountySex_3year,by = c("yearG3","county","sex"))
datCounty_3year <- calculateRates(datCounty_3year,1)

# NcOTE: "1" used above for number of years, because both numerators (deaths) and denominators 
# (population data) are already BOTH aggregated over THREE year 


# -- COUNTY 3-YEAR *AGE SPECIFIC* ----------------------------------------------
# ------------------------------------------------------------------------------

c.t1      <- calculateYLLmeasures(c("county","yearG3 ","sex","ageG","lev0"),"lev0")
c.t2      <- calculateYLLmeasures(c("county","yearG3 ","sex","ageG","lev1"),"lev1")
c.t3      <- calculateYLLmeasures(c("county","yearG3 ","sex","ageG","lev2"),"lev2")
c.t4      <- calculateYLLmeasures(c("county","yearG3 ","sex","ageG","lev3"),"lev3")
datCounty_AGE_3year <- bind_rows(c.t1,c.t2,c.t3,c.t4)

s.t1      <- calculateYLLmeasures(c(         "yearG3 ","sex","ageG","lev0"),"lev0")
s.t2      <- calculateYLLmeasures(c(         "yearG3 ","sex","ageG","lev1"),"lev1")
s.t3      <- calculateYLLmeasures(c(         "yearG3 ","sex","ageG","lev2"),"lev2")
s.t4      <- calculateYLLmeasures(c(         "yearG3 ","sex","ageG","lev3"),"lev3")
datState_AGE_3year  <- bind_rows(s.t1,s.t2,s.t3,s.t4)
datState_AGE_3year$county = STATE

datCounty_AGE_3year <- bind_rows(datCounty_AGE_3year,datState_AGE_3year)
datCounty_AGE_3year <- left_join(datCounty_AGE_3year, popCountySexAgeG_3year,by = c("yearG3","county","sex","ageG"))
datCounty_AGE_3year <- calculateRates(datCounty_AGE_3year,1)

# NcOTE: "1" used above for number of years, because both numerators (deaths) and denominators 
# (population data) are already BOTH aggregated over THREE year 


# -- COUNTY 5-YEAR -------------------------------------------------------------
# ------------------------------------------------------------------------------

c.t1      <- calculateYLLmeasures(c("county","yearG5 ","sex","lev0"),"lev0")
c.t2      <- calculateYLLmeasures(c("county","yearG5 ","sex","lev1"),"lev1")
c.t3      <- calculateYLLmeasures(c("county","yearG5 ","sex","lev2"),"lev2")
c.t4      <- calculateYLLmeasures(c("county","yearG5 ","sex","lev3"),"lev3")
datCounty_5year <- bind_rows(c.t1,c.t2,c.t3,c.t4)

s.t1      <- calculateYLLmeasures(c(         "yearG5 ","sex","lev0"),"lev0")
s.t2      <- calculateYLLmeasures(c(         "yearG5 ","sex","lev1"),"lev1")
s.t3      <- calculateYLLmeasures(c(         "yearG5 ","sex","lev2"),"lev2")
s.t4      <- calculateYLLmeasures(c(         "yearG5 ","sex","lev3"),"lev3")
datState_5year  <- bind_rows(s.t1,s.t2,s.t3,s.t4)
datState_5year$county = STATE

datCounty_5year <- bind_rows(datCounty_5year,datState_5year)
datCounty_5year <- merge(datCounty_5year, popCountySex_5year,by = c("yearG5","county","sex"))
datCounty_5year <- calculateRates(datCounty_5year,1)

# NcOTE: "1" used above for number of years, because both numerators (deaths) and denominators 
# (population data) are already BOTH aggregated over THREE year 

# -- RACE-ETHNICITY COUNTY 3-YEAR ----------------------------------------------
# ------------------------------------------------------------------------------

c.t1.RE      <- calculateYLLmeasures(c("county","yearG3","sex","raceCode","lev0"),"lev0")
c.t2.RE      <- calculateYLLmeasures(c("county","yearG3","sex","raceCode","lev1"),"lev1")
c.t3.RE      <- calculateYLLmeasures(c("county","yearG3","sex","raceCode","lev2"),"lev2")
c.t4.RE      <- calculateYLLmeasures(c("county","yearG3","sex","raceCode","lev3"),"lev3")
datCounty_RE <- bind_rows(c.t1.RE,c.t2.RE,c.t3.RE,c.t4.RE)

s.t1.RE      <- calculateYLLmeasures(c(         "yearG3","sex","raceCode","lev0"),"lev0")
s.t2.RE      <- calculateYLLmeasures(c(         "yearG3","sex","raceCode","lev1"),"lev1")
s.t3.RE      <- calculateYLLmeasures(c(         "yearG3","sex","raceCode","lev2"),"lev2")
s.t4.RE      <- calculateYLLmeasures(c(         "yearG3","sex","raceCode","lev3"),"lev3")
datState.RE  <- bind_rows(s.t1.RE,s.t2.RE,s.t3.RE,s.t4.RE)
datState.RE$county = STATE

datCounty_RE <- bind_rows(datCounty_RE, datState.RE)
datCounty_RE <- merge(datCounty_RE, popCountySex.RACE, by = c("yearG3","county","sex","raceCode"))
datCounty_RE <- calculateRates(datCounty_RE,1)

# NOTE: "1" used above for number of years, because both numerators (deaths) and denominators 
# (population data) are already BOTH aggregated over THREE year 




# -- COMMUNITY -----------------------------------------------------------------
# ------------------------------------------------------------------------------

c.t1      <- calculateYLLmeasures(c("comID","yearG5","sex","lev0"),"lev0")  
c.t2      <- calculateYLLmeasures(c("comID","yearG5","sex","lev1"),"lev1")
c.t3      <- calculateYLLmeasures(c("comID","yearG5","sex","lev2"),"lev2")
datComm   <- bind_rows(c.t1,c.t2,c.t3)     %>%
                filter(yearG5  %in%  yearGrp)  %>%   # 2013-2017 ONLY!!!
                arrange(comID,yearG5,CAUSE)

datComm  <- merge(datComm,popCommSex,by = c("yearG5","comID","sex"),all=TRUE)
datComm  <- calculateRates(datComm,5)

# add community names  POSSIBLE REMOVE
datComm  <- merge(datComm, comName, by = "comID",all=TRUE) %>%
  arrange(comID,yearG5,CAUSE)



# == CALCULATE AGE-ADJUSTED RATES==================================================================
# =================================================================================================

# -- makes dataframeS of all possible combinations -----------------------------
# ------------------------------------------------------------------------------

year     <- data.frame(year     = sort(unique(cbdDat0$year))) # these "vectors" need to be dataframes for the sq merge below to work
yearG5   <- data.frame(yearG5   = sort(unique(cbdDat0$yearG5)),                   stringsAsFactors = FALSE) 
yearG3   <- data.frame(yearG3   = sort(unique(cbdDat0$yearG3)),                   stringsAsFactors = FALSE)
CAUSE1   <- data.frame(CAUSE    = allLabels,                                      stringsAsFactors = FALSE) 
CAUSE2   <- data.frame(CAUSE    = CAUSE1[nchar(as.character(CAUSE1$CAUSE)) < 4,], stringsAsFactors = FALSE)
CAUSE3   <- data.frame(CAUSE    = CAUSE1[nchar(as.character(CAUSE1$CAUSE)) < 2,], stringsAsFactors = FALSE)
sex      <- data.frame(sex      = c("Male","Female","Total"),                     stringsAsFactors = FALSE)
ageG     <- data.frame(ageG     = sort(unique(cbdDat0$ageG)),                     stringsAsFactors = FALSE)
county   <- data.frame(county   = c(geoMap$countyName,"California"),              stringsAsFactors = FALSE)        
comID    <- data.frame(comID    = unique(cbdLinkCA[,"comID"]),                    stringsAsFactors = FALSE)
GEOID    <- data.frame(GEOID    = cbdLinkCA[,"GEOID"],                            stringsAsFactors = FALSE)
raceCode <- data.frame(raceCode = sort(unique(cbdDat0$raceCode)),                 stringsAsFactors = FALSE)

# other cool approach from Adam:
# fullMatCounty <- Reduce(function(...) merge(..., all = TRUE), list(county, year, CAUSE, sex, ageG))
fullMatCounty       <- sqldf(" select * from  county cross join year   cross join CAUSE1 cross join sex cross join ageG")  %>% mutate(tester=0)
fullMatCounty_3year <- sqldf(" select * from  county cross join yearG3 cross join CAUSE1 cross join sex cross join ageG")  %>% mutate(tester=0)
fullMatCounty_5year <- sqldf(" select * from  county cross join yearG5 cross join CAUSE1 cross join sex cross join ageG")  %>% mutate(tester=0)
fullMatComm         <- sqldf(" select * from  comID  cross join yearG5 cross join CAUSE2 cross join sex cross join ageG")  %>% mutate(tester=0)
fullMatTract        <- sqldf(" select * from  GEOID  cross join yearG5 cross join CAUSE3 cross join sex cross join ageG")  %>% mutate(tester=0)
fullMatCounty_RE    <- sqldf(" select * from  county cross join yearG3 cross join CAUSE1 cross join sex cross join ageG join raceCode") %>% mutate(tester=0)



# -- COUNTY (age-adjusted) ----------------------------------------------------
# -----------------------------------------------------------------------------

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
datAA1 <- filter(datAA1,!is.na(ageG))     # remove 403 records with missing age (0.065% of deaths)  -- impact of this?
# datAA1 <- filter(datAA1,!is.na(CAUSE))  # remove 6955 records with missing CAUSE
datAA1 <- filter(datAA1,!is.na(county))   # remove 758 records with missing county
# datAA1 <- filter(datAA1,!is.na(sex))    # remove 

ageCounty   <- full_join(fullMatCounty,datAA1 ,by = c("county","year","sex","ageG","CAUSE"))  %>%    # merge death data and "fullMatCounty"
               full_join(popCountySexAgeG, by = c("county","year","sex","ageG") )             %>%    # merge population
               full_join(popStandard[,c("ageG","US2000POP")],          by="ageG")                    # merge standard population
 
ageCounty$Ndeaths[is.na(ageCounty$Ndeaths)] <- 0    # if NA deaths in strata change to "0"
ageCounty$YLL[is.na(ageCounty$YLL)]         <- 0    # if NA deaths in strata change to "0"

countyAA <- ageCounty %>% group_by(county,year,sex,CAUSE) %>%
  summarize(oDeaths = sum(Ndeaths,na.rm=TRUE),
            aRate   = ageadjust.direct.SAM(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct.SAM(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct.SAM(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE     = ageadjust.direct.SAM(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000, 
            YLL.adj.rate   = ageadjust.direct.SAM(count=YLL, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) # CONFIRM


countyAA <- countyAA[!(countyAA$oDeaths==0),c("county","year","sex","CAUSE","aRate","aLCI","aUCI","aSE","YLL.adj.rate")]  # remove strata with no deaths and select columns  


# -- COUNTY 3-YEAR (age-adjusted) ----------------------------------------------
# ------------------------------------------------------------------------------

tA1      <- cbdDat0 %>% group_by(county,yearG3, sex, ageG,CAUSE=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA2      <- cbdDat0 %>% group_by(county,yearG3, sex, ageG,CAUSE=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA3      <- cbdDat0 %>% group_by(county,yearG3, sex, ageG,CAUSE=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA4      <- cbdDat0 %>% group_by(county,yearG3, sex, ageG,CAUSE=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA5      <- cbdDat0 %>% group_by(       yearG3, sex, ageG,CAUSE=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA6      <- cbdDat0 %>% group_by(       yearG3, sex, ageG,CAUSE=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA7      <- cbdDat0 %>% group_by(       yearG3, sex, ageG,CAUSE=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA8      <- cbdDat0 %>% group_by(       yearG3, sex, ageG,CAUSE=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)

datAA1 <- bind_rows(tA1,tA2,tA3,tA4,tA5,tA6,tA7,tA8)  %>% ungroup()  # UNGROUP HERE!!!!

datAA1 <- filter(datAA1,!is.na(ageG))     # remove 403 records with missing age (0.065% of deaths)  -- impact of this?
datAA1 <- filter(datAA1,!is.na(county))   # remove 758 records with missing county

ageCounty_3year   <- full_join(fullMatCounty_3year,datAA1 ,by = c("county","yearG3","sex","ageG","CAUSE"))  %>%    # merge death data and "fullMatCounty"
  full_join(popCountySexAgeG_3year, by = c("county","yearG3","sex","ageG") )             %>%    # merge population
  full_join(popStandard[,c("ageG","US2000POP")],          by="ageG")                    # merge standard population

ageCounty_3year$Ndeaths[is.na(ageCounty_3year$Ndeaths)] <- 0    # if NA deaths in strata change to "0"
ageCounty_3year$YLL[is.na(ageCounty_3year$YLL)]         <- 0    # if NA deaths in strata change to "0"

countyAA_3year <- ageCounty_3year %>% group_by(county,yearG3,sex,CAUSE) %>%
  summarize(oDeaths = sum(Ndeaths,na.rm=TRUE),
            aRate   = ageadjust.direct.SAM(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct.SAM(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct.SAM(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE     = ageadjust.direct.SAM(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000, 
            YLL.adj.rate   = ageadjust.direct.SAM(count=YLL, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) # CONFIRM


countyAA_3year <- countyAA_3year[!(countyAA_3year$oDeaths==0),c("county","yearG3","sex","CAUSE","aRate","aLCI","aUCI","aSE","YLL.adj.rate")]  # remove strata with no deaths and select columns  

# -- COUNTY 5-YEAR (age-adjusted) ----------------------------------------------
# ------------------------------------------------------------------------------

tA1      <- cbdDat0 %>% group_by(county,yearG5, sex, ageG,CAUSE=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA2      <- cbdDat0 %>% group_by(county,yearG5, sex, ageG,CAUSE=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA3      <- cbdDat0 %>% group_by(county,yearG5, sex, ageG,CAUSE=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA4      <- cbdDat0 %>% group_by(county,yearG5, sex, ageG,CAUSE=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA5      <- cbdDat0 %>% group_by(       yearG5, sex, ageG,CAUSE=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA6      <- cbdDat0 %>% group_by(       yearG5, sex, ageG,CAUSE=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA7      <- cbdDat0 %>% group_by(       yearG5, sex, ageG,CAUSE=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA8      <- cbdDat0 %>% group_by(       yearG5, sex, ageG,CAUSE=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)

datAA1 <- bind_rows(tA1,tA2,tA3,tA4,tA5,tA6,tA7,tA8)  %>% ungroup()  # UNGROUP HERE!!!!

datAA1 <- filter(datAA1,!is.na(ageG))     # remove 403 records with missing age (0.065% of deaths)  -- impact of this?
datAA1 <- filter(datAA1,!is.na(county))   # remove 758 records with missing county

ageCounty_5year   <- full_join(fullMatCounty_5year,datAA1 ,by = c("county","yearG5","sex","ageG","CAUSE"))  %>%    # merge death data and "fullMatCounty"
  full_join(popCountySexAgeG_5year, by = c("county","yearG5","sex","ageG") )             %>%    # merge population
  full_join(popStandard[,c("ageG","US2000POP")],          by="ageG")                    # merge standard population

ageCounty_5year$Ndeaths[is.na(ageCounty_5year$Ndeaths)] <- 0    # if NA deaths in strata change to "0"
ageCounty_5year$YLL[is.na(ageCounty_5year$YLL)]         <- 0    # if NA deaths in strata change to "0"




countyAA_5year <- ageCounty_5year %>% group_by(county,yearG5,sex,CAUSE) %>%
  summarize(oDeaths = sum(Ndeaths,na.rm=TRUE),
            aRate   = ageadjust.direct.SAM(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct.SAM(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct.SAM(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE     = ageadjust.direct.SAM(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000, 
            YLL.adj.rate   = ageadjust.direct.SAM(count=YLL, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) # CONFIRM


countyAA_5year <- countyAA_5year[!(countyAA_5year$oDeaths==0),c("county","yearG5","sex","CAUSE","aRate","aLCI","aUCI","aSE","YLL.adj.rate")]  # remove strata with no deaths and select columns  


# -- RACE-ETHNICITY COUNTY 3-YEAR (age-adjusted) -------------------------------
# ------------------------------------------------------------------------------

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

ageCounty.RE   <- full_join(fullMatCounty_RE,datAA1 ,by = c("county","yearG3","sex","ageG","raceCode","CAUSE"))  %>%   
                  full_join(popCountySexAgeG.RACE, by = c("county","yearG3","sex","ageG","raceCode") )           %>%   
                  full_join(popStandard[,c("ageG","US2000POP")],          by="ageG")                

ageCounty.RE$Ndeaths[is.na(ageCounty.RE$Ndeaths)] <- 0  
ageCounty.RE$YLL[is.na(ageCounty.RE$YLL)]         <- 0  

countyAA.RE <- ageCounty.RE %>% group_by(county,yearG3,sex,raceCode,CAUSE) %>%
  summarize(oDeaths = sum(Ndeaths,na.rm=TRUE),
            aRate   = ageadjust.direct.SAM(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct.SAM(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct.SAM(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE     = ageadjust.direct.SAM(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000, 
            YLL.adj.rate   = ageadjust.direct.SAM(count=YLL, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) 

countyAA.RE <- countyAA.RE[!(countyAA.RE$oDeaths==0),c("county","yearG3","sex","raceCode","CAUSE","aRate","aLCI","aUCI","aSE","YLL.adj.rate")]  



# -- COMMUNITY (age-adjusted) --------------------------------------------------
# ------------------------------------------------------------------------------

tA1      <- cbdDat0 %>% group_by(comID, yearG5, sex, ageG,CAUSE=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) )   
tA2      <- cbdDat0 %>% group_by(comID, yearG5, sex, ageG,CAUSE=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA3      <- cbdDat0 %>% group_by(comID, yearG5, sex, ageG,CAUSE=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 

datAA1 <- bind_rows(tA1,tA2,tA3)  %>% filter(comID != "")  

ageComm   <- full_join(fullMatComm,datAA1,by = c("comID","yearG5","sex","ageG","CAUSE"))  %>% 
             filter(yearG5  %in%  yearGrp)                                                     %>%
             full_join(popCommSexAgeG, by = c("comID","yearG5","sex","ageG"))             %>%
             full_join(popStandard[,c("ageG","US2000POP")],by="ageG")                     

ageComm$Ndeaths[is.na(ageComm$Ndeaths)] <- 0    
ageComm$YLL[is.na(ageComm$YLL)]         <- 0    

### LINE BELOW ADDED 7/2/2018
ageComm <- filter(ageComm,!is.na(ageG))

commAA <- ageComm %>% group_by(comID,yearG5,sex,CAUSE) %>%
  summarize(oDeaths = sum(Ndeaths,na.rm=TRUE),
            aRate   = ageadjust.direct.SAM(count=Ndeaths, pop=pop*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct.SAM(count=Ndeaths, pop=pop*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct.SAM(count=Ndeaths, pop=pop*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE     = ageadjust.direct.SAM(count=Ndeaths, pop=pop*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000, 
            YLL.adj.rate   = ageadjust.direct.SAM(count=YLL, pop=pop*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) 
commAA <- commAA[!(commAA$oDeaths==0),c("comID","yearG5","sex","CAUSE","oDeaths","aRate","aLCI","aUCI","aSE","YLL.adj.rate")]  

# removes rows with aRate = inf HERE there are only ALPINE 
commAA  <- commAA[!(commAA$aRate > 10000),]



# == MERGE CRUDE AND AGJECT RATES AND CLEAN UP ====================================================
# =================================================================================================
# =================================================================================================

# COUNTY ----------------------------------------------------------------------

datCounty <- merge(datCounty,countyAA ,by = c("county","year","sex","CAUSE"),all=TRUE)

# SMR Calculations --------

datState  <- datCounty  %>% 
               filter(county == STATE) %>%
               mutate(stateCrudeRate = cDeathRate,
               stateAdjustedRate = aRate) %>%
               select(year,sex,Level,CAUSE,stateCrudeRate,stateAdjustedRate)

if (!subSite & whichDat == "real") save(datState, file = path(myPlace,"/myData/datState.RDS"))
if ( subSite)                      load(          file = path(myPlace,"/myData/datState.RDS"))

datCounty  <- merge(datCounty,datState,by = c("year","sex","Level","CAUSE")) %>%
                mutate(SMRcrude = cDeathRate / stateCrudeRate,
                       SMR      = aRate      / stateAdjustedRate)
# ------------------------

datCounty <-  datCounty %>% 
               filter(!(is.na(CAUSE)))                                       %>% # removes "Level3" NA (most 'causes' are NA on Level3) 
               select(-stateCrudeRate,-stateAdjustedRate)                    %>%
               mutate_if(is.numeric, signif,digits=4)                        %>%  # much smaller file and easier to read
               mutate(county = ifelse(county==STATE, toupper(STATE),county))      # e.g. California --> CALIFORNIA
               

# -- COUNTY 3-YEAR ------------------------------------------------------------


datCounty_3year <- merge(datCounty_3year,countyAA_3year ,by = c("county","yearG3","sex","CAUSE"),all=TRUE)

# SMR Calculations --------

datState_3year  <- datCounty_3year  %>% 
                   filter(county == STATE) %>%
                   mutate(stateCrudeRate = cDeathRate,
                          stateAdjustedRate = aRate) %>%
                   select(yearG3,sex,Level,CAUSE,stateCrudeRate,stateAdjustedRate)

if (!subSite & whichDat == "real") save(datState_3year, file = path(myPlace,"/myData/datState_3yr.RDS"))
if ( subSite)                      load(                file = path(myPlace,"/myData/datState_3yr.RDS"))

datCounty_3year  <- merge(datCounty_3year,datState_3year,by = c("yearG3","sex","Level","CAUSE")) %>%
  mutate(SMRcrude = cDeathRate / stateCrudeRate,
         SMR      = aRate      / stateAdjustedRate)
# ------------------------

datCounty_3year <-  datCounty_3year %>% 
  filter(!(is.na(CAUSE)))                                       %>% # removes "Level3" NA (most 'causes' are NA on Level3) 
  select(-stateCrudeRate,-stateAdjustedRate)                    %>%
  mutate_if(is.numeric, signif,digits=4)                        %>%  # much smaller file and easier to read
  mutate(county = ifelse(county==STATE, toupper(STATE),county))      # e.g. California --> CALIFORNIA

# -- COUNTY 3-YEAR AGE SPECIFIC ------------------------------------------------------------



datCounty_AGE_3year <-  datCounty_AGE_3year %>% 
  filter(!(is.na(CAUSE)))                                       %>% # removes "Level3" NA (most 'causes' are NA on Level3) 
  mutate_if(is.numeric, signif,digits=4)                        %>%  # much smaller file and easier to read
  mutate(county = ifelse(county==STATE, toupper(STATE),county))      # e.g. California --> CALIFORNIA






# -- COUNTY 5-YEAR ------------------------------------------------------------


datCounty_5year <- merge(datCounty_5year,countyAA_5year ,by = c("county","yearG5","sex","CAUSE"),all=TRUE)

# SMR Calculations --------

datState_5year  <- datCounty_5year  %>% 
  filter(county == STATE) %>%
  mutate(stateCrudeRate = cDeathRate,
         stateAdjustedRate = aRate) %>%
  select(yearG5,sex,Level,CAUSE,stateCrudeRate,stateAdjustedRate)

if (!subSite & whichDat == "real") save(datState_5year, file = path(myPlace,"/myData/datState_5yr.RDS"))
if ( subSite)                      load(                file = path(myPlace,"/myData/datState_5yr.RDS"))

datCounty_5year  <- merge(datCounty_5year,datState_5year,by = c("yearG5","sex","Level","CAUSE")) %>%
  mutate(SMRcrude = cDeathRate / stateCrudeRate,
         SMR      = aRate      / stateAdjustedRate)
# ------------------------

datCounty_5year <-  datCounty_5year %>% 
  filter(!(is.na(CAUSE)))                                       %>% # removes "Level3" NA (most 'causes' are NA on Level3) 
  select(-stateCrudeRate,-stateAdjustedRate)                    %>%
  mutate_if(is.numeric, signif,digits=4)                        %>%  # much smaller file and easier to read
  mutate(county = ifelse(county==STATE, toupper(STATE),county))      # e.g. California --> CALIFORNIA




# -- RACE ---------------------------------------------------------------------

datCounty_RE <- merge(datCounty_RE,countyAA.RE, by = c("county","yearG3","sex","raceCode","CAUSE"),all=TRUE)

datCounty_RE <- datCounty_RE                                    %>% 
  filter(!(is.na(CAUSE)))                                       %>%  
  select(-ageG)                                                 %>%
  mutate_if(is.numeric, signif,digits=4)                        %>%  
  mutate(county = ifelse(county==STATE, toupper(STATE),county))      
  



# -- COMMUNITY ----------------------------------------------------------------

datComm   <- merge(datComm,    commAA ,by = c("comID","yearG5","sex","CAUSE"),all=TRUE) %>%
  mutate_if(is.numeric, signif,digits=4) %>%
  filter(!is.na(county)) #  as above



# == CELL SUPRESSION WITH COMPLEMTARY CELL SUPRESSION==============================================
# =================================================================================================


# 
# source(path(upPlace,"upstreamInfo","suppressionFunction.R"))
# 
# # COUNTY
# gBy       <-  c("county","year","Level","CAUSE")         # FOR MAIN
# datCounty <- mutate(datCounty, supIndicator = mySuppress(datCounty,gBy,"Ndeaths"))
# datCounty <- filter(datCounty, 
#                     supIndicator != 1, 
#                     !(CAUSE=="A09" & sex %in% c("Male","Female"))
#                     ) %>%
#              select(-supIndicator)
# 
# 
# 
# 
# # COUNTY 3-YEAR
# gBy       <-  c("county","yearG3","Level","CAUSE")         # FOR 3 year
# datCounty_3year <- mutate(datCounty_3year, supIndicator = mySuppress(datCounty_3year,gBy,"Ndeaths"))
# datCounty_3year <- filter(datCounty_3year, 
#                     supIndicator != 1, 
#                     !(CAUSE=="A09" & sex %in% c("Male","Female"))
# ) %>%
#   select(-supIndicator)
# 
# 
# 
# 
# 
# # COUNTY 3-YEAR AGE SPECIFIC 
# gBy       <-  c("county","yearG3","Level","sex","CAUSE")         # FOR 3 year
# datCounty_AGE_3year <- mutate(datCounty_AGE_3year, supIndicator = mySuppress(datCounty_AGE_3year,gBy,"Ndeaths"))
# datCounty_AGE_3year <- filter(datCounty_AGE_3year, 
#                           supIndicator != 1, 
#                           !(CAUSE=="A09" & sex %in% c("Male","Female"))
# ) %>%
#   select(-supIndicator)
# 
# 
# 
# 
# 
# 
# # COUNTY 5-YEAR
# gBy       <-  c("county","yearG5","Level","CAUSE")         # FOR 5 year
# datCounty_5year <- mutate(datCounty_5year, supIndicator = mySuppress(datCounty_5year,gBy,"Ndeaths"))
# datCounty_5year <- filter(datCounty_5year, 
#                           supIndicator != 1, 
#                           !(CAUSE=="A09" & sex %in% c("Male","Female"))
# ) %>%
#   select(-supIndicator)
# 
# 
# # COUNTY - RACE
# gBy          <- c("county","yearG3","Level","CAUSE","sex")   # FOR RACE
# datCounty_RE <- mutate(datCounty_RE, supIndicator = mySuppress(datCounty_RE,gBy,"Ndeaths"))
# datCounty_RE <- filter(datCounty_RE,
#                        supIndicator != 1,
#                        !(CAUSE=="A09" & sex %in% c("Male","Female"))
#                        ) %>%
#                 select(-supIndicator)
# 
# 
# 
# # COUNTY - EDUCATION
# gBy          <- c("county","year","Level","CAUSE","sex")   # FOR EDUCATION
# datCounty_EDU <- mutate(datCounty_EDU, supIndicator = mySuppress(datCounty_EDU,gBy,"Ndeaths"))
# datCounty_EDU <- filter(datCounty_EDU,
#                        supIndicator != 1,
#                        !(CAUSE=="A09" & sex %in% c("Male","Female"))
#                        ) %>%
#                  select(-supIndicator)
# 
# 
# 
# # COMMUNITY
# datComm      <- filter(datComm, Ndeaths >= criticalNum)
# datComm      <- filter(datComm, !(CAUSE=="A09" & sex %in% c("Male","Female")))
# 
# # TRACT
# datTract     <- filter(datTract, Ndeaths >= criticalNum)
# datTract     <- filter(datTract, !(CAUSE=="A09" & sex %in% c("Male","Female")))
# # Quick fix to replace with Version Beta 1.1
# # eliminates pop 0 and therefore infinity rates
# datTract  <- filter(datTract,pop>0)
# 
# 


# == SAVE DATA SETS FOR APPLICATION ===============================================================
# =================================================================================================

upGuns  <- paste0(myDrive,"/myAnalyses/FireArms") 




causeNameLink <- gbdMap0 %>% 
                  filter(!is.na(causeList)) %>%
                  select(CAUSE=LABEL,causeName=nameOnly)


datCounty             <- left_join(datCounty,      causeNameLink,by="CAUSE")
datCounty_3year       <- left_join(datCounty_3year,      causeNameLink,by="CAUSE")
datCounty_AGE_3year   <- left_join(datCounty_AGE_3year,      causeNameLink,by="CAUSE")
datCounty_5year       <- left_join(datCounty_5year,      causeNameLink,by="CAUSE")
datCounty_RE          <- left_join(datCounty_RE,      causeNameLink,by="CAUSE")
datComm               <- left_join(datComm,      causeNameLink,by="CAUSE")


saveRDS(datCounty,           file= path(upGuns,"datCounty.RDS"))
saveRDS(datCounty_3year,     file= path(upGuns,"datCounty_3year.RDS"))
saveRDS(datCounty_AGE_3year, file= path(upGuns,"datCounty_AGE_3year.RDS"))
saveRDS(datCounty_5year,     file= path(upGuns,"datCounty_5year.RDS"))
saveRDS(datCounty_RE,        file= path(upGuns,"datCounty_RE.RDS"))
saveRDS(datComm,             file= path(upGuns,"datComm.RDS"))


# END ===================================================================================================================
# =======================================================================================================================
# =======================================================================================================================
