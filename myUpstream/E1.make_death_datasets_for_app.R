# RACE STANDARIZATION
# CALIFORNIA



# ============================================================================
# "makeDatasets.R" file   
#
#            designate key constants, folder locations, and load packages     
#            load data mapping files                                          
#            load population denominator data                                 
#            load death data (cbdDat0)                                        
#            build functions for YLL and rate calcuations                     
#            process data and calculate age-adjusted rates                    
#            final merges and processing of main CBD data files               
#            export files for use in CBD app                              
#
#            Michael Samuel
#            2018
#
#=============================================================================


# Exclude year

excludeYear <- 2020
excludeYear3 <- NA # 2020 is labeled as NA
excludeYear5 <- NA # 2020 is labeled as NA

# JASPO - Specify the years for the quarterly data
forQuarter_selectYears <- 2016:2020

# == Designate locations and load packages  =======================================================

standardsPlace <- "g:/FusionData/Standards/"
# standardsPlace <- "/mnt/projects/CCB/Standards/"

server <- F
source(paste0(standardsPlace, "FusionStandards.R"))

myCCBPlace <- paste0(ccbPlace, "myCCB/")


whichDat <- "real"
subSite  <- FALSE

# EDIT SECURE DATA LOCATION AS NEEDED 
# securePath     <- "h:/0.Secure.Data/" # - Save this line
#securePath     <- "g:/FusionData/0.Secure.Data/"
# securePath     <- "g:/0.Secure.Data/"
# securePath     <- "/mnt/projects/CCB/0.Secure.Data/"


secureDataFile <- paste0(securePlace,"myData/ccb_processed_deaths.RDS") 

# secureDataFile <- "/mnt/projects/CCB/0.Secure.Data/myData/ccb_processed_deaths.RDS" 

STATE    <- "CALIFORNIA"

# myDrive <- getwd()  
# myPlace <- paste0(myDrive,"/myCCB") 
# upPlace <- paste0(myDrive,"/myUpstream") 





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

# yearGrp <- "2013-2017"
  yearGrp <- c("2005-2009","2010-2014","2015-2019")

myDigits= 6
  
criticalNum <- 11


#== LOAD STANDARDS AND DATA MAPPING FILES =========================================================

# add to technical notes the purposes and contents of each data mapping file 

# this "as.data.frame" below and elsewhere is really annoying.... but at least icdToGroup function below does not work otherwise;
# becuase the "tibble" is double precision or for some other reason this messes up; 
# and get error "Error: Can't use matrix or array for column indexing"


leMap      <- as.data.frame(read_excel(paste0(ccbInfo,"Age to Life-Expectancy Linkage.xlsx"), sheet="LifeExpLink", range = cell_cols("A:B")))
yearMap    <- as.data.frame(read_excel(paste0(ccbInfo,"Year to Year-Group Linkage.xlsx")))
geoMap     <- as.data.frame(read_excel(paste0(ccbInfo,"County Codes to County Names Linkage.xlsx")))
mssaLink   <- read.csv(paste0(ccbInfo,"Tract to Community Linkage.csv"),colClasses = "character")  # file linking MSSAs to census  #### WAS cbdLinkCA
comName    <- unique(mssaLink[,c("comID","comName")])                                    # dataframe linking comID and comName
ageMap     <- as.data.frame(read_excel(paste0(ccbInfo,"Age Group Standard and US Standard 2000 Population.xlsx"),sheet = "data"))
ageMap_EDU  <- as.data.frame(read_excel(paste0(ccbInfo,"Age Group Standard and US Standard 2000 Population.xlsx"),sheet = "dataEducation"))
raceLink <- as.data.frame(read_excel(paste0(standardsPlace,"raceLink.xlsx")))  %>% select(raceCode,CHSI)

#== LOAD AND PROCESS POPULATION DATA ==============================================================

# ungrouping important for subsequent data set merging

popCounty        <- readRDS(path(ccbUpstream,"upData/popCounty.RDS")) %>%
                         # filter(raceCode == "Total") %>% select(-raceCode) # JASPO commented this out
                         ungroup()  

# JASPO changed some parts of this, plus added popCountyRACE, and popCountyAgeGRACE
popCountySex     <- filter(popCounty,ageGroup == "Total", raceCode == "Total") %>% select(-ageGroup, -raceCode) # no need for ageGroup
popCountySexAgeG <- filter(popCounty,ageGroup != "Total", raceCode == "Total") %>% select(-raceCode)

# For quarter
popCountySexRACE    <- filter(popCounty,ageGroup == "Total", year %in% forQuarter_selectYears) 
popCountySexAgeGRACE <- filter(popCounty, ageGroup != "Total", year %in% forQuarter_selectYears)
###


popCountyRACE_3year        <- readRDS(path(ccbUpstream,"upData/popCounty_RE_3year.RDS")) %>% ungroup() 
popCountySexRACE_3year     <- filter(popCountyRACE_3year,ageGroup == "Total")
popCountySexAgeGRACE_3year <- filter(popCountyRACE_3year,ageGroup != "Total")


popCountySex_3year      <- popCountySex %>%
                           mutate(yearG3  = yearMap[match(year,yearMap[,"year"]),"yearGroup3"]) %>%
                           group_by(yearG3,county,sex) %>%
                           summarize(population=sum(population))

popCountySexAgeG_3year  <- popCountySexAgeG %>%
                           mutate(yearG3  = yearMap[match(year,yearMap[,"year"]),"yearGroup3"]) %>%
                           group_by(yearG3,county,sex,ageGroup) %>%
                           summarize(population=sum(population))

popCountySex_5year    <- popCountySex %>%
                           mutate(yearG5  = yearMap[match(year,yearMap[,"year"]),"yearGroup5"]) %>% 
                           group_by(yearG5,county,sex) %>%
                           summarize(population=sum(population))

popCountySexAgeG_5year  <- popCountySexAgeG %>%
                           mutate(yearG5  = yearMap[match(year,yearMap[,"year"]),"yearGroup5"]) %>%
                           group_by(yearG5,county,sex,ageGroup) %>%
                           summarize(population=sum(population))


popCounty_EDU        <- readRDS(path(ccbUpstream,"upData/popCounty_Education.RDS")) %>% ungroup() %>%
  mutate(county = ifelse(county == "California", STATE, county))
popCountySex_EDU     <- filter(popCounty_EDU,ageG_EDU == "Total") %>% select(-ageG_EDU) # no need for ageGroup
popCountySexAgeG_EDU <- filter(popCounty_EDU,ageG_EDU != "Total")

popTract         <- readRDS(path(ccbUpstream,"upData/popTract.RDS")) %>% ungroup() %>% rename(population=pop)
popTractSex      <- filter(popTract,ageGroup == "Total")
popTractSexAgeG  <- filter(popTract,ageGroup != "Total")

popCommSex       <- popTractSex     %>% group_by(yearG5,county,comID,sex)      %>% summarise(population=sum(population))  %>% ungroup()  
popCommSexAgeG   <- popTractSexAgeG %>% group_by(yearG5,county,comID,sex,ageGroup) %>% summarise(population=sum(population))  %>% ungroup() 

popStandard         <- ageMap    %>% mutate(ageGroup = ageLabel)
popStandard_EDU     <- ageMap_EDU  %>% mutate(ageG_EDU = ageLabel)

###################
#####  POP 65 JASPO #####
###################

popCounty65        <- readRDS(path(ccbUpstream,"upData/popCounty65.RDS")) %>% ungroup() 
popCountySex65     <- filter(popCounty65,ageGroup == "Total", raceCode == "Total") %>% select(-ageGroup, -raceCode) # no need for ageGroup
popCountySexAgeG65 <- filter(popCounty65,ageGroup != "Total", raceCode == "Total") %>% select(-raceCode)

popCountyRACE65_3year        <- readRDS(path(ccbUpstream,"upData/popCounty65_RE_3year.RDS")) %>% ungroup() 
popCountySexRACE65_3year     <- filter(popCountyRACE65_3year,ageGroup == "Total")
popCountySexAgeGRACE65_3year <- filter(popCountyRACE65_3year,ageGroup != "Total")

popStandard65         <- ageMap    %>% mutate(ageGroup = paste0(lAge," - ",uAge))  %>% filter(lAge < 65)



 # == LOAD AND PROCESS DEATH DATA =================================================================

if (whichDat == "real") {
  #load(secureDataFile)
  cbdDat0 <- readRDS(secureDataFile)
}

# if (whichDat == "fake") { 
#   #load(paste0(ccbUpstream,"upData/cbdDat0SAMP.R"))  
#   
#   cbdDat0 <- cbdDat0SAMP
# }


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
                         comID   = mssaLink[match(cbdDat0$GEOID,mssaLink[,"GEOID"]),"comID"],   
                       # yll     = leMap[match(cbdDat0$age,leMap[,"Age"]),"LE"],
                         yll     = ifelse(age > 75, 0, 75-age),
                         yearG5  = yearMap[match(year,yearMap[,"year"]),"yearGroup5"], 
                         yearG3  = yearMap[match(year,yearMap[,"year"]),"yearGroup3"],
                         quarter = dplyr::case_when(month %in% c("01", "02", "03") ~ "01", 
                                             month %in% c("04", "05", "06") ~ "02", 
                                             month %in% c("07", "08", "09") ~ "03", 
                                             month %in% c("10", "11", "12") ~ "04", 
                                             TRUE ~ month),
                         education = ifelse(education == 8,7,education)  # one "Graduate or professional degree" category for now
                        ) %>%
                rename(eduCode = education) 


#cbdDat0    <- rename(cbdDat0,CHSI=raceCode) # remove this step soon by chaning in B2 to use "CHSI" as name rather than raceCode
cbdDat0    <- left_join(cbdDat0,raceLink,by="CHSI")
cbdDat0    <- select(cbdDat0,-CHSI)

cbdDat0Sex   <- mutate(cbdDat0, sex = "Total")
cbdDat0       <- bind_rows(cbdDat0, cbdDat0Sex)


# 
# > freq(cbdDat0$raceCode)
# Frequencies  
# cbdDat0$raceCode  
# Type: Character  
# 
# Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
# -------------- --------- --------- -------------- --------- --------------
#   -missing      2104      0.02           0.02      0.02           0.02
# AIAN-NH     43364      0.43           0.46      0.43           0.46
# Asian-NH    823170      8.25           8.70      8.25           8.70
# Black-NH    762330      7.64          16.34      7.64          16.34
# Hisp   1671212     16.75          33.09     16.75          33.09
# Multi-NH     69286      0.69          33.79      0.69          33.79
# NHPI-NH     30854      0.31          34.10      0.31          34.10
# Other-NH     12040      0.12          34.22      0.12          34.22
# Unk-NH     12862      0.13          34.34      0.13          34.34
# White-NH   6551598     65.66         100.00     65.66         100.00
# <NA>         0                               0.00         100.00
# Total   9978820    100.00         100.00    100.00         100.00











########
#### CONSIDER ADDING CALIFONRIA TOTAL HERE




# -- Add Age-Group variable ---------------------------------------------------

aL            <-      ageMap$lAge     # lower age ranges
aU            <- c(-1,ageMap$uAge)    # upper age ranges, plus inital value of "-1" for lower limit
aLabs         <- ageMap$ageLabel 
aMark         <- findInterval(cbdDat0$age,aU,left.open = TRUE)  # vector indicating age RANGE value of each INDIVIDUAL age value
cbdDat0$ageGroup  <- aLabs[aMark]                                   # make new "ageGroup" variable based on two objects above 




aL_EDU            <-     ageMap_EDU$lAge     # lower age ranges
aU_EDU            <- c(-1,ageMap_EDU$uAge)     # upper age ranges, plus inital value of "-1" for lower limit
aLabs_EDU         <- c("less",paste(aL_EDU,"-",aU_EDU[-1])) # make label for ranges
aMark_EDU         <- findInterval(cbdDat0$age,c(-1,24,aU_EDU[2:5]),left.open = TRUE)  # vector indicating age RANGE value of each INDIVIDUAL age value
cbdDat0$ageG_EDU  <- aLabs_EDU[aMark_EDU]  



# -- Map ICD-10 codes to GBD conditions ----------------------------------------

# Jaspo "source" or use below temporariy .....

gbdMap0       <- as.data.frame(read_excel(paste0(ccbInfo,"icd10_to_CAUSE.xlsx"), sheet="main"))   

# Will get sourced
allCauseCodes <- sort(gbdMap0$causeCode[!is.na(gbdMap0$causeCode)])
#allCauseCodes <- gbdMap0

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
table(codeDoesntMap$ICD10,useNA = "ifany") # These codes are not assigned in the CCB

codeLast4      <- str_sub(cbdDat0$icdCODE,2,5)
nLast4         <- nchar(codeLast4)

cbdDat0          <- cbdDat0  %>% mutate(lev0  = "0",
                                        lev1  = str_sub(icdCODE,2,2),
                                        lev2  = str_sub(icdCODE,2,4),
                                        lev3  = ifelse(nLast4 == 4,codeLast4,NA)
                                       )


# -- MORE DATA CLEANING ISSUES (see at bottom of file) ------------------------

# -- SAVE FILE FOR AD HOC ANALYSIS AND ERROR/ISSUE INVESTIGATION --------------


# saveRDS(cbdDat0,  file= path(securePlace,"myData/cbdDat0-INVESTIGATION-FILE.RDS"))

# JK: Left off here

# DEATH MEASURES FUNCTIONS =========================================================================
# ==================================================================================================

calculateYLLmeasures <- function(group_vars,levLab){
  
  dat <- cbdDat0 %>% group_by(.dots = group_vars) %>% 
  #  dat <- cbdDat0 %>% group_by_(.dots = group_vars) %>% 
      summarize(Ndeaths = n() , 
              YLL     = sum(yll,   na.rm = TRUE),     # NEED TO ADD CIs
              mean.age = mean(age,na.rm=TRUE)
    ) %>%  ungroup 
 
    names(dat)[grep("lev", names(dat))] <- "causeCode"
    dat$Level                           <- levLab
    dat %>%  data.frame

}

pois.approx <- function (x, pt = 1, conf.level = 0.95) 
{
  Z <- qnorm(0.5 * (1 + conf.level))
  SE.R <- sqrt(x/pt^2)
  lower <- x/pt - Z * SE.R
  upper <- x/pt + Z * SE.R
  data.frame(x = x, pt = pt, rate = x/pt, se = SE.R, lower = lower, upper = upper, 
             conf.level = conf.level)
}


calculateRates <- function(inData,yearN){
  transform(inData, 
            YLLper      = yF*YLL/(yearN*population),
            cDeathRate  = yF*Ndeaths/(yearN*population),
            rateSE      = yF*pois.approx(Ndeaths,yearN*population, conf.level = 0.95)$se,
            rateLCI     = yF*pois.approx(Ndeaths,yearN*population, conf.level = 0.95)$lower,
            rateUCI     = yF*pois.approx(Ndeaths,yearN*population, conf.level = 0.95)$upper
  )
}


# https://github.com/cran/epitools/blob/master/R/ageadjust.direct.R
ageadjust.direct.SAM <- function (count, population, rate = NULL, stdpop, conf.level = 0.95) 
{
  if (missing(count) == TRUE & !missing(population) == TRUE & is.null(rate) == TRUE)     count      <- rate * population
  if (missing(population) == TRUE & !missing(count) == TRUE & is.null(rate) == TRUE)     population <- count/rate
  if (is.null(rate) == TRUE & !missing(count) == TRUE & !missing(population) == TRUE)    rate       <- count/population
  
  rate[is.na(population)]          <- 0
  rate[is.null(population)]        <- 0
  population[is.na(population)]    <- 0
  population[is.null(population)]  <- 0
  
  alpha <- 1 - conf.level
  cruderate <- sum(count,na.rm=TRUE)/sum(population,na.rm=TRUE)
  stdwt <- stdpop/sum(stdpop,na.rm=TRUE)
  dsr <- sum(stdwt * rate,na.rm=TRUE)
  dsr.var <- sum((stdwt^2) * (count/population^2))
  dsr.se  <- sqrt(dsr.var)
  wm<- max(stdwt/population)
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
datCounty <-  merge(datCounty,popCountySex,by = c("year","county","sex"))

# CALCULATE RATES
datCounty <- calculateRates(datCounty,1)



# -- COUNTY 65 JASPO -------------------------------------------------------------------
# -----------------------------------------------------------------------------

cbdDat0_SAVE <- cbdDat0
cbdDat0 <- filter(cbdDat0, age < 65)

c.t1      <- calculateYLLmeasures(c("county","year","sex","lev0"),"lev0")
c.t2      <- calculateYLLmeasures(c("county","year","sex","lev1"),"lev1")
c.t3      <- calculateYLLmeasures(c("county","year","sex","lev2"),"lev2")
c.t4      <- calculateYLLmeasures(c("county","year","sex","lev3"),"lev3")
datCounty65 <- bind_rows(c.t1,c.t2,c.t3,c.t4)

s.t1      <- calculateYLLmeasures(c(         "year","sex","lev0"),"lev0")
s.t2      <- calculateYLLmeasures(c(         "year","sex","lev1"),"lev1")
s.t3      <- calculateYLLmeasures(c(         "year","sex","lev2"),"lev2")
s.t4      <- calculateYLLmeasures(c(         "year","sex","lev3"),"lev3")
datState65  <- bind_rows(s.t1,s.t2,s.t3,s.t4)
datState65$county = STATE

datCounty65 <- bind_rows(datCounty65,datState65)

# MERGE Death and Population files
datCounty65 <- merge(datCounty65,popCountySex65,by = c("year","county","sex"))

# CALCULATE RATES
datCounty65 <- calculateRates(datCounty65,1)


# -- RACE-ETHNICITY COUNTY65 3-YEAR JASPO ----------------------------------------------
# ------------------------------------------------------------------------------

c.t1.RE      <- calculateYLLmeasures(c("county","yearG3","sex","raceCode","lev0"),"lev0")
c.t2.RE      <- calculateYLLmeasures(c("county","yearG3","sex","raceCode","lev1"),"lev1")
c.t3.RE      <- calculateYLLmeasures(c("county","yearG3","sex","raceCode","lev2"),"lev2")
c.t4.RE      <- calculateYLLmeasures(c("county","yearG3","sex","raceCode","lev3"),"lev3")
datCounty65_RE <- bind_rows(c.t1.RE,c.t2.RE,c.t3.RE,c.t4.RE)

s.t1.RE      <- calculateYLLmeasures(c(         "yearG3","sex","raceCode","lev0"),"lev0")
s.t2.RE      <- calculateYLLmeasures(c(         "yearG3","sex","raceCode","lev1"),"lev1")
s.t3.RE      <- calculateYLLmeasures(c(         "yearG3","sex","raceCode","lev2"),"lev2")
s.t4.RE      <- calculateYLLmeasures(c(         "yearG3","sex","raceCode","lev3"),"lev3")
datState65.RE  <- bind_rows(s.t1.RE,s.t2.RE,s.t3.RE,s.t4.RE)
datState65.RE$county = STATE

datCounty65_RE <- bind_rows(datCounty65_RE, datState65.RE)
datCounty65_RE <- merge(datCounty65_RE, popCountySexRACE65_3year, by = c("yearG3","county","sex","raceCode"))
datCounty65_RE <- calculateRates(datCounty65_RE,1)

# NOTE: "1" used above for number of years, because both numerators (deaths) and denominators 
# (population data) are already BOTH aggregated over THREE year


cbdDat0 <- cbdDat0_SAVE # JASPO - Done with calculating crude rate for <65. Going back to original cbdDat0



# -- COUNTY BY MONTH - JASPO -------------------------------------------------------------------
# -----------------------------------------------------------------------------

cbdDat0_SAVE <- cbdDat0

cbdDat0 <- filter(cbdDat0, year %in% forQuarter_selectYears)

c.t1      <- calculateYLLmeasures(c("county","year", "month", "sex","lev0"),"lev0")
c.t2      <- calculateYLLmeasures(c("county","year", "month", "sex","lev1"),"lev1")
c.t3      <- calculateYLLmeasures(c("county","year", "month", "sex","lev2"),"lev2")
c.t4      <- calculateYLLmeasures(c("county","year", "month", "sex","lev3"),"lev3")
datCounty_M <- bind_rows(c.t1,c.t2,c.t3,c.t4)

s.t1      <- calculateYLLmeasures(c(         "year", "month", "sex","lev0"),"lev0")
s.t2      <- calculateYLLmeasures(c(         "year", "month", "sex","lev1"),"lev1")
s.t3      <- calculateYLLmeasures(c(         "year", "month", "sex","lev2"),"lev2")
s.t4      <- calculateYLLmeasures(c(         "year", "month", "sex","lev3"),"lev3")
datState_M  <- bind_rows(s.t1,s.t2,s.t3,s.t4)
datState_M$county = STATE

datCounty_M <- bind_rows(datCounty_M,datState_M)

# MERGE Death and Population files
datCounty_M <-  merge(datCounty_M,popCountySex,by = c("year","county","sex")) #%>%
  # mutate(population = population / 12) # Divide by 12?

# CALCULATE RATES
datCounty_M <- calculateRates(datCounty_M,1)

cbdDat0 <- cbdDat0_SAVE



# -- COUNTY BY RACE BY QUARTER BY JASPO------------------------------------------------
# -----------------------------------------------------------------------------

cbdDat0_SAVE <- cbdDat0

cbdDat0 <- filter(cbdDat0, year %in% forQuarter_selectYears)
cbdDat0_RACE <- mutate(cbdDat0, raceCode = "Total")
cbdDat0 <- bind_rows(cbdDat0, cbdDat0_RACE)

c.t1      <- calculateYLLmeasures(c("county","year","quarter","sex","raceCode","lev0"),"lev0")
c.t2      <- calculateYLLmeasures(c("county","year","quarter","sex","raceCode","lev1"),"lev1")
c.t3      <- calculateYLLmeasures(c("county","year","quarter","sex","raceCode","lev2"),"lev2")
c.t4      <- calculateYLLmeasures(c("county","year","quarter","sex","raceCode","lev3"),"lev3")
datCounty_Q <- bind_rows(c.t1,c.t2,c.t3,c.t4)

s.t1      <- calculateYLLmeasures(c(         "year","quarter","sex","raceCode","lev0"),"lev0")
s.t2      <- calculateYLLmeasures(c(         "year","quarter","sex","raceCode","lev1"),"lev1")
s.t3      <- calculateYLLmeasures(c(         "year","quarter","sex","raceCode","lev2"),"lev2")
s.t4      <- calculateYLLmeasures(c(         "year","quarter","sex","raceCode","lev3"),"lev3")
datState_Q  <- bind_rows(s.t1,s.t2,s.t3,s.t4)
datState_Q$county = STATE

datCounty_Q <- bind_rows(datCounty_Q,datState_Q)

# MERGE Death and Population files - THIS IS PERFORMING AN INNER JOIN. MAY WANT TO PERFORM ANOTHER TYPE OF JOIN
datCounty_Q <-  merge(datCounty_Q,popCountySexRACE,by = c("year","county","sex", "raceCode")) #%>%
                #mutate(population = population / 4) # Divide by 4?

# CALCULATE RATES
datCounty_Q <- calculateRates(datCounty_Q,1)

cbdDat0 <- cbdDat0_SAVE


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

c.t1      <- calculateYLLmeasures(c("county","yearG3 ","sex","ageGroup","lev0"),"lev0")
c.t2      <- calculateYLLmeasures(c("county","yearG3 ","sex","ageGroup","lev1"),"lev1")
c.t3      <- calculateYLLmeasures(c("county","yearG3 ","sex","ageGroup","lev2"),"lev2")
c.t4      <- calculateYLLmeasures(c("county","yearG3 ","sex","ageGroup","lev3"),"lev3")
datCounty_AGE_3year <- bind_rows(c.t1,c.t2,c.t3,c.t4)

s.t1      <- calculateYLLmeasures(c(         "yearG3 ","sex","ageGroup","lev0"),"lev0")
s.t2      <- calculateYLLmeasures(c(         "yearG3 ","sex","ageGroup","lev1"),"lev1")
s.t3      <- calculateYLLmeasures(c(         "yearG3 ","sex","ageGroup","lev2"),"lev2")
s.t4      <- calculateYLLmeasures(c(         "yearG3 ","sex","ageGroup","lev3"),"lev3")
datState_AGE_3year  <- bind_rows(s.t1,s.t2,s.t3,s.t4)
datState_AGE_3year$county = STATE

datCounty_AGE_3year <- bind_rows(datCounty_AGE_3year,datState_AGE_3year)
datCounty_AGE_3year <- left_join(datCounty_AGE_3year, popCountySexAgeG_3year,by = c("yearG3","county","sex","ageGroup"))
datCounty_AGE_3year <- calculateRates(datCounty_AGE_3year,1)

# NcOTE: "1" used above for number of years, because both numerators (deaths) and denominators 
# (population data) are already BOTH aggregated over THREE year 


# -- COUNTY 3-YEAR *OLDER AGE SPECIFIC* -------JASPO---------------------------------------
# ------------------------------------------------------------------------------


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
datCounty_RE <- merge(datCounty_RE, popCountySexRACE_3year, by = c("yearG3","county","sex","raceCode"))
datCounty_RE <- calculateRates(datCounty_RE,1)

# NOTE: "1" used above for number of years, because both numerators (deaths) and denominators 
# (population data) are already BOTH aggregated over THREE year 



# -- EDUCATION COUNTY ----------------------------------------------------------
# ------------------------------------------------------------------------------

##### AGE > 25 ONLY
cbdDat0_SAVE <- cbdDat0
cbdDat0 <- filter(cbdDat0, age >= 25)


c.t1.EDU      <- calculateYLLmeasures(c("county","year","sex","eduCode","lev0"),"lev0")
c.t2.EDU      <- calculateYLLmeasures(c("county","year","sex","eduCode","lev1"),"lev1")
c.t3.EDU      <- calculateYLLmeasures(c("county","year","sex","eduCode","lev2"),"lev2")
datCounty_EDU <- bind_rows(c.t1.EDU,c.t2.EDU,c.t3.EDU)

s.t1.EDU      <- calculateYLLmeasures(c(         "year","sex","eduCode","lev0"),"lev0")
s.t2.EDU      <- calculateYLLmeasures(c(         "year","sex","eduCode","lev1"),"lev1")
s.t3.EDU      <- calculateYLLmeasures(c(         "year","sex","eduCode","lev2"),"lev2")
datState.EDU  <- bind_rows(s.t1.EDU,s.t2.EDU,s.t3.EDU)
datState.EDU$county = STATE

datCounty_EDU <- bind_rows(datCounty_EDU, datState.EDU)


datCounty_EDU <- merge(datCounty_EDU, popCountySex_EDU, by = c("year","county","sex","eduCode"))

datCounty_EDU <- calculateRates(datCounty_EDU,1)




cbdDat0 <- cbdDat0_SAVE

# -- COMMUNITY -----------------------------------------------------------------
# ------------------------------------------------------------------------------

c.t1      <- calculateYLLmeasures(c("comID","yearG5","sex","lev0"),"lev0")  
c.t2      <- calculateYLLmeasures(c("comID","yearG5","sex","lev1"),"lev1")
c.t3      <- calculateYLLmeasures(c("comID","yearG5","sex","lev2"),"lev2")
datComm   <- bind_rows(c.t1,c.t2,c.t3)     %>%
                filter(yearG5  %in%  yearGrp)  %>%   # 2013-2017 ONLY!!!
                arrange(comID,yearG5,causeCode)

datComm  <- merge(datComm,popCommSex,by = c("yearG5","comID","sex"),all=TRUE)
datComm  <- calculateRates(datComm,5)

# add community names  POSSIBLE REMOVE
datComm  <- merge(datComm, comName, by = "comID",all=TRUE) %>%
  arrange(comID,yearG5,causeCode)


# -- TRACT ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

c.t1      <- calculateYLLmeasures(c("GEOID","yearG5","sex","lev0"),"lev0")
c.t2      <- calculateYLLmeasures(c("GEOID","yearG5","sex","lev1"),"lev1")
# c.t3      <- calculateYLLmeasures(c("GEOID","yearG5","sex","lev2"),"lev2") -- include for SPECIAL RUNS only

datTract  <- bind_rows(c.t1,c.t2,c.t3) %>% 
                filter(yearG5  %in%  yearGrp)  %>%    # 2013-2017 ONLY!!!
                arrange(GEOID,yearG5,causeCode)
# NOTE -- includes many with NA GEOID

datTract <- merge(datTract,popTractSex,by = c("yearG5","GEOID","sex"),all=TRUE)                     

datTract <- calculateRates(datTract,5) %>%
  arrange(GEOID,yearG5,causeCode)





# == CALCULATE AGE-ADJUSTED RATES==================================================================
# =================================================================================================

# -- makes dataframeS of all possible combinations -----------------------------
# ------------------------------------------------------------------------------
month    <- data.frame(month     = sort(unique(cbdDat0$month)),                   stringsAsFactors = FALSE)
quarter  <- data.frame(quarter  = sort(unique(cbdDat0$quarter)),                  stringsAsFactors = FALSE)
year     <- data.frame(year     = sort(unique(cbdDat0$year))) # these "vectors" need to be dataframes for the sq merge below to work
yearG5   <- data.frame(yearG5   = sort(unique(cbdDat0$yearG5)),                   stringsAsFactors = FALSE) 
yearG3   <- data.frame(yearG3   = sort(unique(cbdDat0$yearG3)),                   stringsAsFactors = FALSE)
CAUSE1   <- data.frame(causeCode    = allCauseCodes,                                  stringsAsFactors = FALSE) 
CAUSE2   <- data.frame(causeCode    = CAUSE1[nchar(as.character(CAUSE1$causeCode)) < 4,], stringsAsFactors = FALSE)
CAUSE3   <- data.frame(causeCode    = CAUSE1[nchar(as.character(CAUSE1$causeCode)) < 2,], stringsAsFactors = FALSE)
sex      <- data.frame(sex      = c("Male","Female","Total"),                     stringsAsFactors = FALSE)
ageGroup     <- data.frame(ageGroup     = sort(unique(cbdDat0$ageGroup)),                     stringsAsFactors = FALSE)
county   <- data.frame(county   = c(geoMap$countyName,STATE),                 stringsAsFactors = FALSE)        
comID    <- data.frame(comID    = unique(mssaLink[,"comID"]),                    stringsAsFactors = FALSE)
GEOID    <- data.frame(GEOID    = mssaLink[,"GEOID"],                            stringsAsFactors = FALSE)
raceCode <- data.frame(raceCode = sort(unique(cbdDat0$raceCode)),                 stringsAsFactors = FALSE)


yearForQuarterly <- data.frame(year = forQuarter_selectYears) # forQuarter_selectYears assigned at the top of script. Run into memory issues when too many years are selected
raceCodeForQuarterly <- raceCode %>% tibble::add_row(raceCode = "Total") # For the quarterly dataset, we need to add Total race to crossjoin in fullmat

# other cool approach from Adam:
# fullMatCounty <- Reduce(function(...) merge(..., all = TRUE), list(county, year, CAUSE, sex, ageGroup))
fullMatCounty          <- sqldf(" select * from  county cross join year   cross join CAUSE1 cross join sex cross join ageGroup")  %>% mutate(tester=0)
fullMatCounty_3year    <- sqldf(" select * from  county cross join yearG3 cross join CAUSE1 cross join sex cross join ageGroup")  %>% mutate(tester=0)
fullMatCounty_5year    <- sqldf(" select * from  county cross join yearG5 cross join CAUSE1 cross join sex cross join ageGroup")  %>% mutate(tester=0)
fullMatComm            <- sqldf(" select * from  comID  cross join yearG5 cross join CAUSE2 cross join sex cross join ageGroup")  %>% mutate(tester=0)
fullMatTract           <- sqldf(" select * from  GEOID  cross join yearG5 cross join CAUSE3 cross join sex cross join ageGroup")  %>% mutate(tester=0)
fullMatCounty_RE_3year <- sqldf(" select * from  county cross join yearG3 cross join CAUSE1 cross join sex cross join ageGroup join raceCode") %>% mutate(tester=0)


fullMatCounty_RE_Q     <- sqldf(" select * from  county cross join yearForQuarterly cross join quarter cross join CAUSE1 cross join sex cross join ageGroup cross join raceCodeForQuarterly") %>% mutate(tester=0)
# JASPO added above for quarterly

fullMatCounty_M     <- sqldf(" select * from  county cross join yearForQuarterly cross join month cross join CAUSE1 cross join sex cross join ageGroup") %>% mutate(tester=0)
# JASPO added above for monthly

# -- COUNTY (age-adjusted) ----------------------------------------------------
# -----------------------------------------------------------------------------

tA1      <- cbdDat0 %>% group_by(county,year, sex, ageGroup,causeCode=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA2      <- cbdDat0 %>% group_by(county,year, sex, ageGroup,causeCode=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA3      <- cbdDat0 %>% group_by(county,year, sex, ageGroup,causeCode=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA4      <- cbdDat0 %>% group_by(county,year, sex, ageGroup,causeCode=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA5      <- cbdDat0 %>% group_by(       year, sex, ageGroup,causeCode=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA6      <- cbdDat0 %>% group_by(       year, sex, ageGroup,causeCode=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA7      <- cbdDat0 %>% group_by(       year, sex, ageGroup,causeCode=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA8      <- cbdDat0 %>% group_by(       year, sex, ageGroup,causeCode=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)

datAA1 <- bind_rows(tA1,tA2,tA3,tA4,tA5,tA6,tA7,tA8)  %>% ungroup()  # UNGROUP HERE!!!!

# DATA CLEANING ISSUES as above
datAA1 <- filter(datAA1,!is.na(ageGroup))     # remove 403 records with missing age (0.065% of deaths)  -- impact of this?
# datAA1 <- filter(datAA1,!is.na(causeCode))  # remove 6955 records with missing causeCode
datAA1 <- filter(datAA1,!is.na(county))   # remove 758 records with missing county
# datAA1 <- filter(datAA1,!is.na(sex))    # remove 

ageCounty   <- full_join(fullMatCounty,datAA1 ,by = c("county","year","sex","ageGroup","causeCode"))  %>%    # merge death data and "fullMatCounty"
               full_join(popCountySexAgeG, by = c("county","year","sex","ageGroup") )             %>%    # merge population
               full_join(popStandard[,c("ageGroup","US2000POP")],          by="ageGroup")                    # merge standard population
 
ageCounty$Ndeaths[is.na(ageCounty$Ndeaths)] <- 0    # if NA deaths in strata change to "0"
ageCounty$YLL[is.na(ageCounty$YLL)]         <- 0    # if NA deaths in strata change to "0"

countyAA <- ageCounty %>% group_by(county,year,sex,causeCode) %>%
  summarize(oDeaths = sum(Ndeaths,na.rm=TRUE),
            aRate   = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE     = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000, 
            YLL.adj.rate   = ageadjust.direct.SAM(count=YLL, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) # CONFIRM


countyAA <- countyAA[!(countyAA$oDeaths==0),c("county","year","sex","causeCode","aRate","aLCI","aUCI","aSE","YLL.adj.rate")]  # remove strata with no deaths and select columns  


# -- COUNTY65 (age-adjusted) - JASPO ----------------------------------------------------
# -----------------------------------------------------------------------------

cbdDat0_SAVE <- cbdDat0
cbdDat0 <- filter(cbdDat0, age < 65)

tA1      <- cbdDat0 %>% group_by(county,year, sex, ageGroup,causeCode=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA2      <- cbdDat0 %>% group_by(county,year, sex, ageGroup,causeCode=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA3      <- cbdDat0 %>% group_by(county,year, sex, ageGroup,causeCode=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA4      <- cbdDat0 %>% group_by(county,year, sex, ageGroup,causeCode=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA5      <- cbdDat0 %>% group_by(       year, sex, ageGroup,causeCode=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA6      <- cbdDat0 %>% group_by(       year, sex, ageGroup,causeCode=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA7      <- cbdDat0 %>% group_by(       year, sex, ageGroup,causeCode=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA8      <- cbdDat0 %>% group_by(       year, sex, ageGroup,causeCode=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)

datAA1 <- bind_rows(tA1,tA2,tA3,tA4,tA5,tA6,tA7,tA8)  %>% ungroup()  # UNGROUP HERE!!!!

# DATA CLEANING ISSUES as above
datAA1 <- filter(datAA1,!is.na(ageGroup))     # remove 403 records with missing age (0.065% of deaths)  -- impact of this?
# datAA1 <- filter(datAA1,!is.na(causeCode))  # remove 6955 records with missing causeCode
datAA1 <- filter(datAA1,!is.na(county))   # remove 758 records with missing county
# datAA1 <- filter(datAA1,!is.na(sex))    # remove 

ageCounty65   <- full_join(filter(fullMatCounty, !ageGroup %in% c("65 - 74", "75 - 84", "85+")),datAA1 ,by = c("county","year","sex","ageGroup","causeCode"))  %>%    # merge death data and "fullMatCounty"
  full_join(popCountySexAgeG65, by = c("county","year","sex","ageGroup") )             %>%    # merge population
  full_join(popStandard65[,c("ageGroup","US2000POP")],          by="ageGroup")                    # merge standard population

ageCounty65$Ndeaths[is.na(ageCounty65$Ndeaths)] <- 0    # if NA deaths in strata change to "0"
ageCounty65$YLL[is.na(ageCounty65$YLL)]         <- 0    # if NA deaths in strata change to "0"

countyAA65 <- ageCounty65 %>% group_by(county,year,sex,causeCode) %>%
  summarize(oDeaths = sum(Ndeaths,na.rm=TRUE),
            aRate   = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE     = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000, 
            YLL.adj.rate   = ageadjust.direct.SAM(count=YLL, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) # CONFIRM


countyAA65 <- countyAA65[!(countyAA65$oDeaths==0),c("county","year","sex","causeCode","aRate","aLCI","aUCI","aSE","YLL.adj.rate")]  # remove strata with no deaths and select columns  


# -- RACE-ETHNICITY COUNTY65 3-YEAR (age-adjusted) - JASPO -------------------------------
# ------------------------------------------------------------------------------

tA1      <- cbdDat0 %>% group_by(county,yearG3, sex, ageGroup, raceCode, causeCode=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA2      <- cbdDat0 %>% group_by(county,yearG3, sex, ageGroup, raceCode, causeCode=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA3      <- cbdDat0 %>% group_by(county,yearG3, sex, ageGroup, raceCode, causeCode=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA4      <- cbdDat0 %>% group_by(county,yearG3, sex, ageGroup, raceCode, causeCode=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 

tA5      <- cbdDat0 %>% group_by(       yearG3, sex, ageGroup, raceCode, causeCode=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA6      <- cbdDat0 %>% group_by(       yearG3, sex, ageGroup, raceCode, causeCode=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA7      <- cbdDat0 %>% group_by(       yearG3, sex, ageGroup, raceCode, causeCode=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA8      <- cbdDat0 %>% group_by(       yearG3, sex, ageGroup, raceCode, causeCode=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)

datAA1 <- bind_rows(tA1,tA2,tA3,tA4,tA5,tA6,tA7,tA8)  %>% ungroup()  # UNGROUP HERE!!!!

datAA1 <- filter(datAA1,!is.na(ageGroup))   # remove 403 records with missing age (0.065% of deaths)  -- impact of this?
# datAA1 <- filter(datAA1,!is.na(county)) # remove 758 records with missing county ### removed this from RACE DATA......

ageCounty.RE65   <- full_join(filter(fullMatCounty_RE_3year, !ageGroup %in% c("65 - 74", "75 - 84", "85+")),datAA1 ,by = c("county","yearG3","sex","ageGroup","raceCode","causeCode"))  %>%   
  full_join(popCountySexAgeGRACE65_3year, by = c("county","yearG3","sex","ageGroup","raceCode") )           %>%   
  full_join(popStandard65[,c("ageGroup","US2000POP")],          by="ageGroup")                

ageCounty.RE65$Ndeaths[is.na(ageCounty.RE65$Ndeaths)] <- 0  
ageCounty.RE65$YLL[is.na(ageCounty.RE65$YLL)]         <- 0  



countyAA.RE65 <- ageCounty.RE65 %>% group_by(county,yearG3,sex,raceCode,causeCode) %>%
  summarize(oDeaths = sum(Ndeaths,na.rm=TRUE),
            aRate   = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE     = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000, 
            YLL.adj.rate   = ageadjust.direct.SAM(count=YLL, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) 

countyAA.RE65 <- countyAA.RE65[!(countyAA.RE65$oDeaths==0),c("county","yearG3","sex","raceCode","causeCode","aRate","aLCI","aUCI","aSE","YLL.adj.rate")]  


cbdDat0 <- cbdDat0_SAVE


# -- COUNTY BY MONTH (age-adjusted) - JASPO ----------------------------------------------------
# -----------------------------------------------------------------------------

cbdDat0_SAVE <- cbdDat0

cbdDat0 <- filter(cbdDat0, year %in% forQuarter_selectYears)

tA1      <- cbdDat0 %>% group_by(county,year, month, sex, ageGroup,causeCode=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA2      <- cbdDat0 %>% group_by(county,year, month, sex, ageGroup,causeCode=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA3      <- cbdDat0 %>% group_by(county,year, month, sex, ageGroup,causeCode=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA4      <- cbdDat0 %>% group_by(county,year, month, sex, ageGroup,causeCode=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA5      <- cbdDat0 %>% group_by(       year, month, sex, ageGroup,causeCode=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA6      <- cbdDat0 %>% group_by(       year, month, sex, ageGroup,causeCode=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA7      <- cbdDat0 %>% group_by(       year, month, sex, ageGroup,causeCode=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA8      <- cbdDat0 %>% group_by(       year, month, sex, ageGroup,causeCode=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)

datAA1 <- bind_rows(tA1,tA2,tA3,tA4,tA5,tA6,tA7,tA8)  %>% ungroup()  # UNGROUP HERE!!!!

# DATA CLEANING ISSUES as above
datAA1 <- filter(datAA1,!is.na(ageGroup))     # remove 403 records with missing age (0.065% of deaths)  -- impact of this?
# datAA1 <- filter(datAA1,!is.na(causeCode))  # remove 6955 records with missing causeCode
datAA1 <- filter(datAA1,!is.na(county))   # remove 758 records with missing county
# datAA1 <- filter(datAA1,!is.na(sex))    # remove 

ageCounty_M   <- full_join(fullMatCounty_M,datAA1 ,by = c("county","year","month","sex","ageGroup","causeCode"))  %>%    # merge death data and "fullMatCounty"
  full_join(popCountySexAgeG, by = c("county","year","sex","ageGroup") )             %>%    # merge population
  full_join(popStandard[,c("ageGroup","US2000POP")],          by="ageGroup") #%>%                    # merge standard population
  #mutate(population = population / 12) # Divide by 12?

ageCounty_M$Ndeaths[is.na(ageCounty_M$Ndeaths)] <- 0    # if NA deaths in strata change to "0"
ageCounty_M$YLL[is.na(ageCounty_M$YLL)]         <- 0    # if NA deaths in strata change to "0"

countyAA_M <- ageCounty_M %>% group_by(county,year, month, sex,causeCode) %>%
  summarize(oDeaths = sum(Ndeaths,na.rm=TRUE),
            aRate   = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE     = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000, 
            YLL.adj.rate   = ageadjust.direct.SAM(count=YLL, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) # CONFIRM


countyAA_M <- countyAA_M[!(countyAA_M$oDeaths==0),c("county","year","month", "sex","causeCode","aRate","aLCI","aUCI","aSE","YLL.adj.rate")]  # remove strata with no deaths and select columns  

cbdDat0 <- cbdDat0_SAVE

# -- Quarter By Race (age-adjusted) - JASPO -------------------------------
# ------------------------------------------------------------------------------

cbdDat0_SAVE <- cbdDat0

cbdDat0 <- filter(cbdDat0, year %in% forQuarter_selectYears)
cbdDat0_RACE <- mutate(cbdDat0, raceCode = "Total")
cbdDat0 <- bind_rows(cbdDat0, cbdDat0_RACE)

tA1      <- cbdDat0 %>% group_by(county,year, quarter, sex, ageGroup, raceCode, causeCode=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA2      <- cbdDat0 %>% group_by(county,year, quarter, sex, ageGroup, raceCode, causeCode=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA3      <- cbdDat0 %>% group_by(county,year, quarter, sex, ageGroup, raceCode, causeCode=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA4      <- cbdDat0 %>% group_by(county,year, quarter, sex, ageGroup, raceCode, causeCode=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 

tA5      <- cbdDat0 %>% group_by(       year, quarter, sex, ageGroup, raceCode, causeCode=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA6      <- cbdDat0 %>% group_by(       year, quarter, sex, ageGroup, raceCode, causeCode=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA7      <- cbdDat0 %>% group_by(       year, quarter, sex, ageGroup, raceCode, causeCode=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA8      <- cbdDat0 %>% group_by(       year, quarter, sex, ageGroup, raceCode, causeCode=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)

datAA1 <- bind_rows(tA1,tA2,tA3,tA4,tA5,tA6,tA7,tA8)  %>% ungroup()  # UNGROUP HERE!!!!

datAA1 <- filter(datAA1,!is.na(ageGroup))   # remove 403 records with missing age (0.065% of deaths)  -- impact of this?
# datAA1 <- filter(datAA1,!is.na(county)) # remove 758 records with missing county ### removed this from RACE DATA......

ageCounty_Q   <- full_join(fullMatCounty_RE_Q,datAA1 ,by = c("county","year","quarter", "sex","ageGroup","raceCode","causeCode"))  %>%   
  full_join(popCountySexAgeGRACE, by = c("county","year","sex","ageGroup","raceCode") )           %>%   
  full_join(popStandard[,c("ageGroup","US2000POP")],          by="ageGroup") # %>%
  # mutate(population = population/4) # JASPO ADDITION - DIVIDE BY 4 HERE?

ageCounty_Q$Ndeaths[is.na(ageCounty_Q$Ndeaths)] <- 0  
ageCounty_Q$YLL[is.na(ageCounty_Q$YLL)]         <- 0  


# TEMPORARY - for State of Health Report
saveRDS(ageCounty_Q, file= path(myCCBPlace,"myData/",whichDat,"/datCounty_RACE_AGE_Q.RDS"))



countyAA_Q <- ageCounty_Q %>% group_by(county,year,quarter,sex,raceCode,causeCode) %>%
  summarize(oDeaths = sum(Ndeaths,na.rm=TRUE),
            aRate   = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE     = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000, 
            YLL.adj.rate   = ageadjust.direct.SAM(count=YLL, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) 

countyAA_Q <- countyAA_Q[!(countyAA_Q$oDeaths==0),c("county","year","quarter","sex","raceCode","causeCode","aRate","aLCI","aUCI","aSE","YLL.adj.rate")]  

cbdDat0 <- cbdDat0_SAVE

# -- COUNTY 3-YEAR (age-adjusted) ----------------------------------------------
# ------------------------------------------------------------------------------

tA1      <- cbdDat0 %>% group_by(county,yearG3, sex, ageGroup,causeCode=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA2      <- cbdDat0 %>% group_by(county,yearG3, sex, ageGroup,causeCode=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA3      <- cbdDat0 %>% group_by(county,yearG3, sex, ageGroup,causeCode=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA4      <- cbdDat0 %>% group_by(county,yearG3, sex, ageGroup,causeCode=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA5      <- cbdDat0 %>% group_by(       yearG3, sex, ageGroup,causeCode=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA6      <- cbdDat0 %>% group_by(       yearG3, sex, ageGroup,causeCode=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA7      <- cbdDat0 %>% group_by(       yearG3, sex, ageGroup,causeCode=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA8      <- cbdDat0 %>% group_by(       yearG3, sex, ageGroup,causeCode=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)

datAA1 <- bind_rows(tA1,tA2,tA3,tA4,tA5,tA6,tA7,tA8)  %>% ungroup()  # UNGROUP HERE!!!!

datAA1 <- filter(datAA1,!is.na(ageGroup))     # remove 403 records with missing age (0.065% of deaths)  -- impact of this?
datAA1 <- filter(datAA1,!is.na(county))   # remove 758 records with missing county

ageCounty_3year   <- full_join(fullMatCounty_3year,datAA1 ,by = c("county","yearG3","sex","ageGroup","causeCode"))  %>%    # merge death data and "fullMatCounty"
  full_join(popCountySexAgeG_3year, by = c("county","yearG3","sex","ageGroup") )             %>%    # merge population
  full_join(popStandard[,c("ageGroup","US2000POP")],          by="ageGroup")                    # merge standard population

ageCounty_3year$Ndeaths[is.na(ageCounty_3year$Ndeaths)] <- 0    # if NA deaths in strata change to "0"
ageCounty_3year$YLL[is.na(ageCounty_3year$YLL)]         <- 0    # if NA deaths in strata change to "0"



countyAA_3year <- ageCounty_3year %>% group_by(county,yearG3,sex,causeCode) %>%
  summarize(oDeaths = sum(Ndeaths,na.rm=TRUE),
            aRate   = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE     = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000, 
            YLL.adj.rate   = ageadjust.direct.SAM(count=YLL, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) # CONFIRM


countyAA_3year <- countyAA_3year[!(countyAA_3year$oDeaths==0),c("county","yearG3","sex","causeCode","aRate","aLCI","aUCI","aSE","YLL.adj.rate")]  # remove strata with no deaths and select columns  

# -- COUNTY 5-YEAR (age-adjusted) ----------------------------------------------
# ------------------------------------------------------------------------------

tA1      <- cbdDat0 %>% group_by(county,yearG5, sex, ageGroup,causeCode=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA2      <- cbdDat0 %>% group_by(county,yearG5, sex, ageGroup,causeCode=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA3      <- cbdDat0 %>% group_by(county,yearG5, sex, ageGroup,causeCode=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA4      <- cbdDat0 %>% group_by(county,yearG5, sex, ageGroup,causeCode=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA5      <- cbdDat0 %>% group_by(       yearG5, sex, ageGroup,causeCode=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA6      <- cbdDat0 %>% group_by(       yearG5, sex, ageGroup,causeCode=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA7      <- cbdDat0 %>% group_by(       yearG5, sex, ageGroup,causeCode=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA8      <- cbdDat0 %>% group_by(       yearG5, sex, ageGroup,causeCode=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)

datAA1 <- bind_rows(tA1,tA2,tA3,tA4,tA5,tA6,tA7,tA8)  %>% ungroup()  # UNGROUP HERE!!!!

datAA1 <- filter(datAA1,!is.na(ageGroup))     # remove 403 records with missing age (0.065% of deaths)  -- impact of this?
datAA1 <- filter(datAA1,!is.na(county))   # remove 758 records with missing county

ageCounty_5year   <- full_join(fullMatCounty_5year,datAA1 ,by = c("county","yearG5","sex","ageGroup","causeCode"))  %>%    # merge death data and "fullMatCounty"
  full_join(popCountySexAgeG_5year, by = c("county","yearG5","sex","ageGroup") )             %>%    # merge population
  full_join(popStandard[,c("ageGroup","US2000POP")],          by="ageGroup")                    # merge standard population

ageCounty_5year$Ndeaths[is.na(ageCounty_5year$Ndeaths)] <- 0    # if NA deaths in strata change to "0"
ageCounty_5year$YLL[is.na(ageCounty_5year$YLL)]         <- 0    # if NA deaths in strata change to "0"




countyAA_5year <- ageCounty_5year %>% group_by(county,yearG5,sex,causeCode) %>%
  summarize(oDeaths = sum(Ndeaths,na.rm=TRUE),
            aRate   = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE     = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000, 
            YLL.adj.rate   = ageadjust.direct.SAM(count=YLL, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) # CONFIRM


countyAA_5year <- countyAA_5year[!(countyAA_5year$oDeaths==0),c("county","yearG5","sex","causeCode","aRate","aLCI","aUCI","aSE","YLL.adj.rate")]  # remove strata with no deaths and select columns  


# -- RACE-ETHNICITY COUNTY 3-YEAR (age-adjusted) -------------------------------
# ------------------------------------------------------------------------------

tA1      <- cbdDat0 %>% group_by(county,yearG3, sex, ageGroup, raceCode, causeCode=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA2      <- cbdDat0 %>% group_by(county,yearG3, sex, ageGroup, raceCode, causeCode=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA3      <- cbdDat0 %>% group_by(county,yearG3, sex, ageGroup, raceCode, causeCode=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA4      <- cbdDat0 %>% group_by(county,yearG3, sex, ageGroup, raceCode, causeCode=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 

tA5      <- cbdDat0 %>% group_by(       yearG3, sex, ageGroup, raceCode, causeCode=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA6      <- cbdDat0 %>% group_by(       yearG3, sex, ageGroup, raceCode, causeCode=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA7      <- cbdDat0 %>% group_by(       yearG3, sex, ageGroup, raceCode, causeCode=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA8      <- cbdDat0 %>% group_by(       yearG3, sex, ageGroup, raceCode, causeCode=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)

datAA1 <- bind_rows(tA1,tA2,tA3,tA4,tA5,tA6,tA7,tA8)  %>% ungroup()  # UNGROUP HERE!!!!

datAA1 <- filter(datAA1,!is.na(ageGroup))   # remove 403 records with missing age (0.065% of deaths)  -- impact of this?
# datAA1 <- filter(datAA1,!is.na(county)) # remove 758 records with missing county ### removed this from RACE DATA......

ageCounty.RE   <- full_join(fullMatCounty_RE_3year,datAA1 ,by = c("county","yearG3","sex","ageGroup","raceCode","causeCode"))  %>%   
                  full_join(popCountySexAgeGRACE_3year, by = c("county","yearG3","sex","ageGroup","raceCode") )           %>%   
                  full_join(popStandard[,c("ageGroup","US2000POP")],          by="ageGroup")                

ageCounty.RE$Ndeaths[is.na(ageCounty.RE$Ndeaths)] <- 0  
ageCounty.RE$YLL[is.na(ageCounty.RE$YLL)]         <- 0  


# TEMPORARY - for State of Health Report
saveRDS(ageCounty.RE, file= path(ccbData,whichDat,"datCounty_RACE_AGE.RDS")) # JASPO - RACE AGE TEMP PRODUCED HERE



countyAA.RE <- ageCounty.RE %>% group_by(county,yearG3,sex,raceCode,causeCode) %>%
  summarize(oDeaths = sum(Ndeaths,na.rm=TRUE),
            aRate   = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE     = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000, 
            YLL.adj.rate   = ageadjust.direct.SAM(count=YLL, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) 

countyAA.RE <- countyAA.RE[!(countyAA.RE$oDeaths==0),c("county","yearG3","sex","raceCode","causeCode","aRate","aLCI","aUCI","aSE","YLL.adj.rate")]  


# -- EDUCATION COUNTY 1-YEAR SELECTED COUNTIES (age-adjusted) -_----------------
# ------------------------------------------------------------------------------


# add level 3 back in when convinient sometime

cbdDat0_SAVE <- cbdDat0
cbdDat0 <- filter(cbdDat0, age >= 25, year > 2011)  # 2000-2002 education not processed
                                                    # pop data currenlty only  > 2006

tA1      <- cbdDat0 %>% group_by(county,year, sex, ageG_EDU, eduCode, causeCode=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA2      <- cbdDat0 %>% group_by(county,year, sex, ageG_EDU, eduCode, causeCode=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA3      <- cbdDat0 %>% group_by(county,year, sex, ageG_EDU, eduCode, causeCode=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 

tA5      <- cbdDat0 %>% group_by(       year, sex, ageG_EDU, eduCode, causeCode=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA6      <- cbdDat0 %>% group_by(       year, sex, ageG_EDU, eduCode, causeCode=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA7      <- cbdDat0 %>% group_by(       year, sex, ageG_EDU, eduCode, causeCode=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)

datAA1 <- bind_rows(tA1,tA2,tA3,tA5,tA6,tA7)  %>% ungroup()  # UNGROUP HERE!!!!

#datAA1 <- filter(datAA1,!is.na(ageGroup))   ... NONE
# datAA1 <- filter(datAA1,!is.na(county)) # remove 758 records with missing county ### removed this from RACE DATA......



# JASPO - REVISE D4 script that generates popCountySexAgeG_EDU file
# rename po
ageCounty.EDU   <- full_join(datAA1 ,popCountySexAgeG_EDU, by = c("county","year","sex","ageG_EDU","eduCode")) %>%
  full_join(popStandard_EDU[,c("ageG_EDU","US2000POP")], by="ageG_EDU")   %>%
  # rename(population = pop) %>%
  filter(population > 0) # this needs to be done more carefully, but removed counties with data not available from ACS


ageCounty.EDU$Ndeaths[is.na(ageCounty.EDU$Ndeaths)] <- 0  
ageCounty.EDU$YLL[is.na(ageCounty.EDU$YLL)]         <- 0  

countyAA.EDU <- ageCounty.EDU %>% group_by(county,year,sex,eduCode,causeCode) %>%
  summarize(oDeaths = sum(Ndeaths,na.rm=TRUE),
            aRate   = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE     = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000, 
            YLL.adj.rate   = ageadjust.direct.SAM(count=YLL, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) 

countyAA.EDU <- countyAA.EDU[!(countyAA.EDU$oDeaths==0),c("county","year","sex","eduCode","causeCode","aRate","aLCI","aUCI","aSE","YLL.adj.rate")]  


cbdDat0 <- cbdDat0_SAVE



# -- COMMUNITY (age-adjusted) --------------------------------------------------
# ------------------------------------------------------------------------------

tA1      <- cbdDat0 %>% group_by(comID, yearG5, sex, ageGroup,causeCode=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) )   
tA2      <- cbdDat0 %>% group_by(comID, yearG5, sex, ageGroup,causeCode=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA3      <- cbdDat0 %>% group_by(comID, yearG5, sex, ageGroup,causeCode=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 

datAA1 <- bind_rows(tA1,tA2,tA3)  %>% filter(comID != "")  

ageComm   <- full_join(fullMatComm,datAA1,by = c("comID","yearG5","sex","ageGroup","causeCode"))  %>% 
             filter(yearG5  %in%  yearGrp)                                                     %>%
             full_join(popCommSexAgeG, by = c("comID","yearG5","sex","ageGroup"))             %>%
             full_join(popStandard[,c("ageGroup","US2000POP")],by="ageGroup")                     

ageComm$Ndeaths[is.na(ageComm$Ndeaths)] <- 0    
ageComm$YLL[is.na(ageComm$YLL)]         <- 0    

### LINE BELOW ADDED 7/2/2018
ageComm <- filter(ageComm,!is.na(ageGroup))

commAA <- ageComm %>% group_by(comID,yearG5,sex,causeCode) %>%
  summarize(oDeaths = sum(Ndeaths,na.rm=TRUE),
            aRate   = ageadjust.direct.SAM(count=Ndeaths, population=population*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct.SAM(count=Ndeaths, population=population*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct.SAM(count=Ndeaths, population=population*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE     = ageadjust.direct.SAM(count=Ndeaths, population=population*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000, 
            YLL.adj.rate   = ageadjust.direct.SAM(count=YLL, population=population*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) 
commAA <- commAA[!(commAA$oDeaths==0),c("comID","yearG5","sex","causeCode","oDeaths","aRate","aLCI","aUCI","aSE","YLL.adj.rate")]  

# removes rows with aRate = inf HERE there are only ALPINE 
commAA  <- commAA[!(commAA$aRate > 10000),]


# -- TRACT (AGE-ADJUSTED) ------------------------------------------------------
# ------------------------------------------------------------------------------

tA1      <- cbdDat0 %>% group_by(GEOID, yearG5, sex, ageGroup,causeCode=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA2      <- cbdDat0 %>% group_by(GEOID, yearG5, sex, ageGroup,causeCode=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 

datAA1 <- bind_rows(tA1,tA2)  %>% filter(GEOID != "")  

ageTract   <- full_join(fullMatTract,datAA1,by = c("GEOID","yearG5","sex","ageGroup","causeCode"))  %>% 
              filter(yearG5  %in%  yearGrp)                                                      %>%
              full_join(popTractSexAgeG,by = c("GEOID","yearG5","sex","ageGroup"))              %>% 
              full_join(popStandard[,c("ageGroup","US2000POP")],by="ageGroup")                      

ageTract$Ndeaths[is.na(ageTract$Ndeaths)] <- 0    
ageTract$YLL[is.na(ageTract$YLL)]         <- 0    

tractAA <- ageTract %>% group_by(GEOID,yearG5,sex,causeCode) %>%
  summarize(oDeaths = sum(Ndeaths,na.rm=TRUE),
            aRate   = ageadjust.direct.SAM(count=Ndeaths, population=population*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct.SAM(count=Ndeaths, population=population*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct.SAM(count=Ndeaths, population=population*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            YLL.adj.rate   = ageadjust.direct.SAM(count=YLL, population=population*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) 
tractAA <- tractAA[!(tractAA$oDeaths==0),c("GEOID","yearG5","sex","causeCode","oDeaths","aRate","aLCI","aUCI","YLL.adj.rate")]  

# removes rows with aRate = inf, or infinity becuase some population strata is 0 AND some other odd strata
tractAA  <- tractAA[!(tractAA$aRate > 5000),]
tractAA  <- tractAA[!(is.na(tractAA$aRate)),]






# == MERGE CRUDE AND AGJECT RATES AND CLEAN UP ====================================================
# =================================================================================================
# =================================================================================================

# COUNTY ----------------------------------------------------------------------

datCounty <- merge(datCounty,countyAA ,by = c("county","year","sex","causeCode"),all=TRUE)


# SMR Calculations --------

datState  <- datCounty  %>% 
               filter(county == STATE) %>%
               mutate(stateCrudeRate = cDeathRate,
               stateAdjustedRate = aRate) %>%
               select(year,sex,Level,causeCode,stateCrudeRate,stateAdjustedRate)

if (!subSite & whichDat == "real") saveRDS(datState, path(myCCBPlace,"myData/datState.RDS"))
if ( subSite)                      datState <- readRDS(path(myCCBPlace,"myData/datState.RDS"))

datCounty  <- merge(datCounty,datState,by = c("year","sex","Level","causeCode")) %>%
                mutate(SMRcrude = cDeathRate / stateCrudeRate,
                       SMR      = aRate      / stateAdjustedRate)
# ------------------------

datCounty <-  datCounty %>% 
               filter(!(is.na(causeCode)))                                       %>% # removes "Level3" NA (most 'causes' are NA on Level3) 
               select(-stateCrudeRate,-stateAdjustedRate)                    %>%
               mutate_if(is.numeric, signif,digits = myDigits)               %>%  # much smaller file and easier to read
               mutate(county = ifelse(county==STATE, toupper(STATE),county))      # e.g. California --> CALIFORNIA  #######################CALIFORNIA!!!!!!!!!!!!!!!!!!!!!
        

# COUNTY65 - JASPO ----------------------------------------------------------------------

datCounty65 <- merge(datCounty65,countyAA65 ,by = c("county","year","sex","causeCode"),all=TRUE)

datCounty65 <-  datCounty65 %>% 
  filter(!(is.na(causeCode)))                                    %>% # removes "Level3" NA (most 'causes' are NA on Level3) 
  # select(-stateCrudeRate,-stateAdjustedRate)                    %>%
  mutate_if(is.numeric, signif,digits = myDigits)                      %>%  # much smaller file and easier to read
  mutate(county = ifelse(county==STATE, toupper(STATE),county))      # e.g. California --> CALIFORNIA

# -- RACE65 - JASPO ---------------------------------------------------------------------

datCounty65_RE <- merge(datCounty65_RE,countyAA.RE65, by = c("county","yearG3","sex","raceCode","causeCode"),all=TRUE)

datCounty65_RE <- datCounty65_RE                                    %>% 
  filter(!(is.na(causeCode)))                                       %>%  
  select(-ageGroup)                                                 %>%
  mutate_if(is.numeric, signif,digits = myDigits)                        %>%  
  mutate(county = ifelse(county==STATE, toupper(STATE),county))


# -- Month - JASPO ---------------------------------------------------------------------

datCounty_M <- merge(datCounty_M,countyAA_M, by = c("county","year", "month", "sex","causeCode"),all=TRUE)

datCounty_M <- datCounty_M                                    %>% 
  filter(!(is.na(causeCode)))                                       %>%  
  # select(-ageGroup)                                                 %>%
  mutate_if(is.numeric, signif,digits = myDigits)                        %>%  
  mutate(county = ifelse(county==STATE, toupper(STATE),county)) 

# -- Quarter - JASPO ---------------------------------------------------------------------

datCounty_Q <- merge(datCounty_Q,countyAA_Q, by = c("county","year", "quarter", "sex","raceCode","causeCode"),all=TRUE)

datCounty_Q <- datCounty_Q                                    %>% 
  filter(!(is.na(causeCode)))                                       %>%  
  select(-ageGroup)                                                 %>%
  mutate_if(is.numeric, signif,digits = myDigits)                        %>%  
  mutate(county = ifelse(county==STATE, toupper(STATE),county)) 


# -- COUNTY 3-YEAR ------------------------------------------------------------


datCounty_3year <- merge(datCounty_3year,countyAA_3year ,by = c("county","yearG3","sex","causeCode"),all=TRUE)

# SMR Calculations --------

datState_3year  <- datCounty_3year  %>% 
                   filter(county == STATE) %>%
                   mutate(stateCrudeRate = cDeathRate,
                          stateAdjustedRate = aRate) %>%
                   select(yearG3,sex,Level,causeCode,stateCrudeRate,stateAdjustedRate)

if (!subSite & whichDat == "real") saveRDS(datState_3year, path(myCCBPlace,"myData/datState_3yr.RDS"))
if ( subSite)                      datState_3yr <- readRDS( path(myCCBPlace,"myData/datState_3yr.RDS"))

datCounty_3year  <- merge(datCounty_3year,datState_3year,by = c("yearG3","sex","Level","causeCode")) %>%
  mutate(SMRcrude = cDeathRate / stateCrudeRate,
         SMR      = aRate      / stateAdjustedRate)
# ------------------------

datCounty_3year <-  datCounty_3year %>% 
  filter(!(is.na(causeCode)))                                       %>% # removes "Level3" NA (most 'causes' are NA on Level3) 
  select(-stateCrudeRate,-stateAdjustedRate)                    %>%
  mutate_if(is.numeric, signif,digits = myDigits)                        %>%  # much smaller file and easier to read
  mutate(county = ifelse(county==STATE, toupper(STATE),county))      # e.g. California --> CALIFORNIA

# -- COUNTY 3-YEAR AGE SPECIFIC ------------------------------------------------------------



datCounty_AGE_3year <-  datCounty_AGE_3year %>% 
  filter(!(is.na(causeCode)))                                       %>% # removes "Level3" NA (most 'causes' are NA on Level3) 
  mutate_if(is.numeric, signif,digits = myDigits)                        %>%  # much smaller file and easier to read
  mutate(county = ifelse(county==STATE, toupper(STATE),county))      # e.g. California --> CALIFORNIA






# -- COUNTY 5-YEAR ------------------------------------------------------------


datCounty_5year <- merge(datCounty_5year,countyAA_5year ,by = c("county","yearG5","sex","causeCode"),all=TRUE)

# SMR Calculations --------

datState_5year  <- datCounty_5year  %>% 
  filter(county == STATE) %>%
  mutate(stateCrudeRate = cDeathRate,
         stateAdjustedRate = aRate) %>%
  select(yearG5,sex,Level,causeCode,stateCrudeRate,stateAdjustedRate)

if (!subSite & whichDat == "real") saveRDS(datState_5year, path(myCCBPlace,"myData/datState_5yr.RDS"))
if ( subSite)                      datState_5yr <- readRDS( path(myCCBPlace,"myData/datState_5yr.RDS"))

datCounty_5year  <- merge(datCounty_5year,datState_5year,by = c("yearG5","sex","Level","causeCode")) %>%
  mutate(SMRcrude = cDeathRate / stateCrudeRate,
         SMR      = aRate      / stateAdjustedRate)
# ------------------------

datCounty_5year <-  datCounty_5year %>% 
  filter(!(is.na(causeCode)))                                       %>% # removes "Level3" NA (most 'causes' are NA on Level3) 
  select(-stateCrudeRate,-stateAdjustedRate)                    %>%
  mutate_if(is.numeric, signif,digits = myDigits)                        %>%  # much smaller file and easier to read
  mutate(county = ifelse(county==STATE, toupper(STATE),county))      # e.g. California --> CALIFORNIA




# -- RACE ---------------------------------------------------------------------

datCounty_RE <- merge(datCounty_RE,countyAA.RE, by = c("county","yearG3","sex","raceCode","causeCode"),all=TRUE)

datCounty_RE <- datCounty_RE                                    %>% 
  filter(!(is.na(causeCode)))                                       %>%  
  select(-ageGroup)                                                 %>%
  mutate_if(is.numeric, signif,digits = myDigits)                        %>%  
  mutate(county = ifelse(county==STATE, toupper(STATE),county))      
  


# -- EDUCATION ------------------------------------------------------------------

datCounty_EDU <- merge(datCounty_EDU,countyAA.EDU, by = c("county","year","sex","eduCode","causeCode"),all=TRUE)

datCounty_EDU <- datCounty_EDU                                  %>% 
  filter(!(is.na(causeCode)))                                       %>%  
  mutate_if(is.numeric, signif,digits = myDigits)                        %>%  
  mutate(county = ifelse(county==STATE, toupper(STATE),county))      


# -- COMMUNITY ----------------------------------------------------------------

datComm   <- merge(datComm,    commAA ,by = c("comID","yearG5","sex","causeCode"),all=TRUE) %>%
  mutate_if(is.numeric, signif,digits = myDigits) %>%
  filter(!is.na(county)) #  as above


# -- TRACT --------------------------------------------------------------------

datTract  <- merge(datTract,  tractAA ,by = c("GEOID","yearG5","sex","causeCode"),all=TRUE) %>% 
  mutate_if(is.numeric, signif,digits = myDigits) %>%
  filter(!is.na(county))  %>%  # REMOVE ALL out-of-state GEOID and missing GEOID
  filter(!is.na(causeCode)) # removes about 130 records with bad/no GEOID and/or wrong County based on GEOID





# == CELL SUPRESSION WITH COMPLEMTARY CELL SUPRESSION==============================================
# =================================================================================================

# temp  <- mutate(datCounty,
#                     badRecord = ifelse( ( sex=="Female" & causeCode %in% c("B12") ) |          # Prostate Cancer
#                                         ( sex== "Male"  & causeCode %in% c("B09","B10","B11")), # Breast, Uterine, Ovary
#                                       1,0)
#                     )


source(path(ccbUpstream,"upstreamInfo","suppressionFunction.R"))

# COUNTY
gBy       <-  c("county","year","Level","causeCode")         # FOR MAIN
datCounty <- mutate(datCounty, supIndicator = mySuppress(datCounty,gBy,"Ndeaths"))
datCounty <- filter(datCounty, 
                    supIndicator != 1, 
                    !(causeCode=="A09" & sex %in% c("Male","Female"))
                    ) %>%
             select(-supIndicator)

# COUNTY65 - JASPO
gBy       <-  c("county","year","Level","causeCode")         # FOR MAIN
datCounty65 <- mutate(datCounty65, supIndicator = mySuppress(datCounty65,gBy,"Ndeaths"))
datCounty65 <- filter(datCounty65,
                    supIndicator != 1,
                    !(causeCode=="A09" & sex %in% c("Male","Female"))
                    ) %>%
             select(-supIndicator)


# COUNTY65 - RACE - JASPO
gBy          <- c("county","yearG3","Level","causeCode","sex")   # FOR RACE
datCounty65_RE <- mutate(datCounty65_RE, supIndicator = mySuppress(datCounty65_RE,gBy,"Ndeaths"))
datCounty65_RE <- filter(datCounty65_RE,
                       supIndicator != 1,
                       !(causeCode=="A09" & sex %in% c("Male","Female"))
                       ) %>%
                select(-supIndicator)


# COUNTY 3-YEAR
gBy       <-  c("county","yearG3","Level","causeCode")         # FOR 3 year
datCounty_3year <- mutate(datCounty_3year, supIndicator = mySuppress(datCounty_3year,gBy,"Ndeaths"))
datCounty_3year <- filter(datCounty_3year, 
                    supIndicator != 1, 
                    !(causeCode=="A09" & sex %in% c("Male","Female"))
) %>%
  select(-supIndicator)





# COUNTY 3-YEAR AGE SPECIFIC 
gBy       <-  c("county","yearG3","Level","sex","causeCode")         # FOR 3 year
datCounty_AGE_3year <- mutate(datCounty_AGE_3year, supIndicator = mySuppress(datCounty_AGE_3year,gBy,"Ndeaths"))
datCounty_AGE_3year <- filter(datCounty_AGE_3year, 
                          supIndicator != 1, 
                          !(causeCode=="A09" & sex %in% c("Male","Female"))
) %>%
  select(-supIndicator)






# COUNTY 5-YEAR
gBy       <-  c("county","yearG5","Level","causeCode")         # FOR 5 year
datCounty_5year <- mutate(datCounty_5year, supIndicator = mySuppress(datCounty_5year,gBy,"Ndeaths"))
datCounty_5year <- filter(datCounty_5year, 
                          supIndicator != 1, 
                          !(causeCode=="A09" & sex %in% c("Male","Female"))
) %>%
  select(-supIndicator)


# COUNTY - RACE
gBy          <- c("county","yearG3","Level","causeCode","sex")   # FOR RACE
datCounty_RE <- mutate(datCounty_RE, supIndicator = mySuppress(datCounty_RE,gBy,"Ndeaths"))
datCounty_RE <- filter(datCounty_RE,
                       supIndicator != 1,
                       !(causeCode=="A09" & sex %in% c("Male","Female"))
                       ) %>%
                select(-supIndicator)



# COUNTY - EDUCATION
gBy          <- c("county","year","Level","causeCode","sex")   # FOR EDUCATION
datCounty_EDU <- mutate(datCounty_EDU, supIndicator = mySuppress(datCounty_EDU,gBy,"Ndeaths"))
datCounty_EDU <- filter(datCounty_EDU,
                       supIndicator != 1,
                       !(causeCode=="A09" & sex %in% c("Male","Female"))
                       ) %>%
                 select(-supIndicator)



# COMMUNITY
datComm      <- filter(datComm, Ndeaths >= criticalNum)
datComm      <- filter(datComm, !(causeCode=="A09" & sex %in% c("Male","Female")))

# TRACT
datTract     <- filter(datTract, Ndeaths >= criticalNum)
datTract     <- filter(datTract, !(causeCode=="A09" & sex %in% c("Male","Female")))
# Quick fix to replace with Version Beta 1.1
# eliminates pop 0 and therefore infinity rates
datTract  <- filter(datTract,population>0)




# == SAVE DATA SETS FOR APPLICATION ===============================================================
# =================================================================================================



saveRDS(filter(datCounty, year < excludeYear),       file= path(ccbData,whichDat,"datCounty.RDS"))
saveRDS(datCounty_Q,                                 file= path(ccbData,whichDat,"datCounty_Q.RDS"))
saveRDS(datCounty_M,                                 file= path(ccbData,whichDat,"datCounty_M.RDS"))
saveRDS(filter(datCounty65, year < excludeYear),       file= path(ccbData,whichDat,"datCounty65.RDS"))
saveRDS(filter(datCounty65_RE, !is.na(yearG3)),    file= path(ccbData,whichDat,"datCounty65_RE.RDS"))
saveRDS(filter(datCounty_3year, !is.na(yearG3)),  file= path(ccbData,whichDat,"datCounty_3year.RDS"))


#JASPO datCounty_AGE_OLDER_3year
#saveRDS(filter(datCounty_AGE_3year, !is.na(yearG3)), file= path(ccbData,whichDat,"datCounty_AGE_3year.RDS"))

saveRDS(filter(datCounty_AGE_3year, !is.na(yearG3)), file= path(ccbData,whichDat,"datCounty_AGE_3year.RDS"))

saveRDS(filter(datCounty_5year, !is.na(yearG5)), file= path(ccbData,whichDat,"datCounty_5year.RDS"))
saveRDS(filter(datCounty_RE, !is.na(yearG3)),    file= path(ccbData,whichDat,"datCounty_RE.RDS"))
saveRDS(filter(datCounty_EDU, year < excludeYear),   file= path(ccbData,whichDat,"datCounty_EDU.RDS"))
saveRDS(filter(datComm, !is.na(yearG5)),         file= path(ccbData,whichDat,"datComm.RDS"))
saveRDS(filter(datTract, !is.na(yearG5)),        file= path(ccbData,whichDat,"datTract.RDS"))


# == SAVE AS .CSV FILES FOR AD HOC ANALYSIS =======================================================
# =================================================================================================

causeNameLink <- gbdMap0 %>% 
                  filter(!is.na(causeList)) %>%
                  select(causeCode, causeName)

dC  <- left_join(datCounty,      causeNameLink,by="causeCode")
d3  <- left_join(datCounty_3year,causeNameLink,by="causeCode")
dr  <- left_join(datCounty_RE,   causeNameLink,by="causeCode")
de  <- left_join(datCounty_EDU,  causeNameLink,by="causeCode")
dCm <- left_join(datComm,        causeNameLink,by="causeCode")
dT  <- left_join(datTract,       causeNameLink,by="causeCode")

write_csv(dC,  (paste0(ccbData,whichDat,"/analysisDatasets/County.csv")))
write_csv(d3,  (paste0(ccbData,whichDat,"/analysisDatasets/County_3year.csv")))
write_csv(dr,  (paste0(ccbData,whichDat,"/analysisDatasets/County_RE.csv")))
write_csv(de,  (paste0(ccbData,whichDat,"/analysisDatasets/datCounty_EDU.csv")))
write_csv(dCm, (paste0(ccbData,whichDat,"/analysisDatasets/Community.csv")))
write_csv(dT,  (paste0(ccbData,whichDat,"/analysisDatasets/Tract.csv")))



# END ===================================================================================================================
# =======================================================================================================================
# =======================================================================================================================


# "SMALL CELL and "RISKY causeCode" supression ----------
# xCause0 <- c(14,41,50,139,4,49,192)
# xCause1 <- c(xCause0,10)
# datTract  <- filter(datTract, !(causeCode %in% xCause1))
# datComm   <- filter(datComm,  !(causeCode %in% xCause1))
# datCounty <- filter(datCounty,!(causeCode %in% xCause0))




# saveRDS(ageCounty, file= path(ccbData,whichDat,"forZev.ageCounty.RDS"))
# githubURL <- "https://raw.githubusercontent.com/mcSamuelDataSci/CACommunityBurden/master/myCBD/myData/fake/forZev.ageCounty.RDS"
# download.file(githubURL,"temp.rds", method="curl")
# ageCounty <- readRDS("temp.rds")




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
 # current mssaLink         has 8036 (2010 data)
 # current cbddat0           has 8603! bad geocodes?        
 # something ??              has 8035 ... check...
 
 #temp <- popCensusCom$GEOID
 #junk <- cbdDat0[!(cbdDat0$GEOID %in% temp),]
 #junk <- junk[junk$GEOID != "",]
 #write.csv(junk,(paste0(ccbUpstream,"tempOutput/junk Tracts.csv")))
 
 # these records have a GEOID but not comID suggesting the GEOID is "bad"
 # junk <- filter(cbdDat0,is.na(comID) & GEOID != ""  & year > 2004)  
 # 651 records
 # length(unique(junk$GEOID))
 # 590 unique GEOID not in California (based on current link file)
 #  write.csv(table(junk$GEOID,junk$year),(paste0(ccbUpstream,"tempOutput/junk Tracts.csv")))
 
 # county missing from 3797 records     
 # junk <- filter(cbdDat0,is.na(county))   
 # 3797 records
 # countyFIPS blank=2145 and 999=1652 (but State="CA; based on "F71" only)
 #  write.csv(table(junk$year,junk$countyFIPS),(paste0(ccbUpstream,"tempOutput/missing County FIPS.csv")))
 
 # MAJOR cleaning issue!!!
 # junk <- filter(cbdDat0,is.na(gbd36))   
 # 82775 records where ICD10 does not map to gbd36 -- errors in info file!
 #  write.csv(table(junk$year,junk$countyFIPS),(paste0(ccbUpstream,"tempOutput/no ICD10 to gbd36.csv")))
 
# Potentially useful old code bits:


# could make aL and aU like this, or as below based on an input file:
# aL            <- c(   0, 5,15,25,35,45,55,65,75,85)
# aU            <- c(-1,4,14,24,34,44,54,64,74,84,999)


# "Manual' calcuation of age-adjustment
# popStandard <- readRDS(paste0(ccbUpstream,"upData/popStandard.RDS"))
# ageCounty   <- merge(ageCounty,popStandard,by = c("ageGroup"),all=TRUE)  # merge with "Standard" population

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

