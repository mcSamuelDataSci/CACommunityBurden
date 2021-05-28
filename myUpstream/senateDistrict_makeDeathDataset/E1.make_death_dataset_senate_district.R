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

# For R/E 1 year
popCountySexRACE_1year <- filter(popCounty, ageGroup == "Total")
popCountySexAgeGRACE_1year <- filter(popCounty, ageGroup != "Total")


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


# Senate District Population (2005-2020) - By Race (Inc Total), By Age Group (Inc Total), By Sex (inc Total)


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


# -- RACE-ETHNICITY COUNTY 1-YEAR ----------------------------------------------
# ------------------------------------------------------------------------------

cbdDat0_SAVE <- cbdDat0

# cbdDat0 <- filter(cbdDat0, year %in% forQuarter_selectYears)
cbdDat0_RACE <- mutate(cbdDat0, raceCode = "Total")
cbdDat0 <- bind_rows(cbdDat0, cbdDat0_RACE)

c.t1.RE      <- calculateYLLmeasures(c("county","year","sex","raceCode","lev0"),"lev0")
c.t2.RE      <- calculateYLLmeasures(c("county","year","sex","raceCode","lev1"),"lev1")
c.t3.RE      <- calculateYLLmeasures(c("county","year","sex","raceCode","lev2"),"lev2")
c.t4.RE      <- calculateYLLmeasures(c("county","year","sex","raceCode","lev3"),"lev3")
datCounty_RE_1year <- bind_rows(c.t1.RE,c.t2.RE,c.t3.RE,c.t4.RE)

s.t1.RE      <- calculateYLLmeasures(c(         "year","sex","raceCode","lev0"),"lev0")
s.t2.RE      <- calculateYLLmeasures(c(         "year","sex","raceCode","lev1"),"lev1")
s.t3.RE      <- calculateYLLmeasures(c(         "year","sex","raceCode","lev2"),"lev2")
s.t4.RE      <- calculateYLLmeasures(c(         "year","sex","raceCode","lev3"),"lev3")
datState.RE_1year  <- bind_rows(s.t1.RE,s.t2.RE,s.t3.RE,s.t4.RE)
datState.RE_1year$county = STATE

datCounty_RE_1year <- bind_rows(datCounty_RE_1year, datState.RE_1year)
datCounty_RE_1year <- merge(datCounty_RE_1year, popCountySexRACE_1year, by = c("year","county","sex","raceCode"))
datCounty_RE_1year <- calculateRates(datCounty_RE_1year,1)

# NOTE: "1" used above for number of years, because both numerators (deaths) and denominators 
# (population data) are already BOTH aggregated over THREE year 

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

fullMatCounty_RE_1year <- sqldf(" select * from  county cross join year cross join CAUSE1 cross join sex cross join ageGroup cross join raceCodeForQuarterly") %>% mutate(tester=0)

fullMatComm            <- sqldf(" select * from  comID  cross join yearG5 cross join CAUSE2 cross join sex cross join ageGroup")  %>% mutate(tester=0)
fullMatTract           <- sqldf(" select * from  GEOID  cross join yearG5 cross join CAUSE3 cross join sex cross join ageGroup")  %>% mutate(tester=0)

# -- JASPO -- RACE-ETHNICITY COUNTY 1-YEAR (age-adjusted) -------------------------------
# ------------------------------------------------------------------------------

cbdDat0_SAVE <- cbdDat0

# cbdDat0 <- filter(cbdDat0, year %in% forQuarter_selectYears)
cbdDat0_RACE <- mutate(cbdDat0, raceCode = "Total")
cbdDat0 <- bind_rows(cbdDat0, cbdDat0_RACE)


tA1      <- cbdDat0 %>% group_by(county,year, sex, ageGroup, raceCode, causeCode=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA2      <- cbdDat0 %>% group_by(county,year, sex, ageGroup, raceCode, causeCode=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA3      <- cbdDat0 %>% group_by(county,year, sex, ageGroup, raceCode, causeCode=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA4      <- cbdDat0 %>% group_by(county,year, sex, ageGroup, raceCode, causeCode=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 

tA5      <- cbdDat0 %>% group_by(       year, sex, ageGroup, raceCode, causeCode=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA6      <- cbdDat0 %>% group_by(       year, sex, ageGroup, raceCode, causeCode=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA7      <- cbdDat0 %>% group_by(       year, sex, ageGroup, raceCode, causeCode=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA8      <- cbdDat0 %>% group_by(       year, sex, ageGroup, raceCode, causeCode=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)

datAA1 <- bind_rows(tA1,tA2,tA3,tA4,tA5,tA6,tA7,tA8)  %>% ungroup()  # UNGROUP HERE!!!!

datAA1 <- filter(datAA1,!is.na(ageGroup))   # remove 403 records with missing age (0.065% of deaths)  -- impact of this?
# datAA1 <- filter(datAA1,!is.na(county)) # remove 758 records with missing county ### removed this from RACE DATA......

ageCounty.RE_1year   <- full_join(fullMatCounty_RE_1year,datAA1 ,by = c("county","year","sex","ageGroup","raceCode","causeCode"))  %>%   
  full_join(popCountySexAgeGRACE_1year, by = c("county","year","sex","ageGroup","raceCode") )           %>%   
  full_join(popStandard[,c("ageGroup","US2000POP")],          by="ageGroup")                

ageCounty.RE_1year$Ndeaths[is.na(ageCounty.RE_1year$Ndeaths)] <- 0  
ageCounty.RE_1year$YLL[is.na(ageCounty.RE_1year$YLL)]         <- 0  


# TEMPORARY - for State of Health Report
saveRDS(ageCounty.RE_1year, file= path(ccbData,whichDat,"datCounty_RACE_AGE_1year.RDS")) # JASPO - RACE AGE TEMP PRODUCED HERE



countyAA.RE_1year <- ageCounty.RE_1year %>% group_by(county,year,sex,raceCode,causeCode) %>%
  summarize(oDeaths = sum(Ndeaths,na.rm=TRUE),
            aRate   = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE     = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000, 
            YLL.adj.rate   = ageadjust.direct.SAM(count=YLL, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) 

countyAA.RE_1year <- countyAA.RE_1year[!(countyAA.RE_1year$oDeaths==0),c("county","year","sex","raceCode","causeCode","aRate","aLCI","aUCI","aSE","YLL.adj.rate")]  

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

# -- RACE 1 year - JASPO  ---------------------------------------------------------------------

datCounty_RE_1year <- merge(datCounty_RE_1year,countyAA.RE_1year, by = c("county","year","sex","raceCode","causeCode"),all=TRUE)

datCounty_RE_1year <- datCounty_RE_1year                                    %>% 
  filter(!(is.na(causeCode)))                                       %>%  
  select(-ageGroup)                                                 %>%
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
