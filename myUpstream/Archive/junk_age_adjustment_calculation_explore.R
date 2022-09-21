securePath     <- "/mnt/projects/FusionData/0.Secure.Data/"
secureDataFile <- paste0(securePath,"myData/ccb_processed_deaths.RDS") 

STATE    <- "CALIFORNIA"

myDrive <- getwd()  
myPlace <- paste0(myDrive,"/myCBD") 
upPlace <- paste0(myDrive,"/myUpstream") 

standardsPlace <- "g:/FusionData/Standards/"
standardsPlace <- "/mnt/projects/FusionData/Standards/"

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


leMap      <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/Age to Life-Expectancy Linkage.xlsx"), sheet="LifeExpLink", range = cell_cols("A:B")))
yearMap    <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/Year to Year-Group Linkage.xlsx")))
geoMap     <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/County Codes to County Names Linkage.xlsx")))
mssaLink   <- read.csv(paste0(myPlace,"/myInfo/Tract to Community Linkage.csv"),colClasses = "character")  # file linking MSSAs to census  #### WAS cbdLinkCA
comName    <- unique(mssaLink[,c("comID","comName")])                                    # dataframe linking comID and comName
ageMap     <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/Age Group Standard and US Standard 2000 Population.xlsx"),sheet = "data"))
ageMap_EDU  <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/Age Group Standard and US Standard 2000 Population.xlsx"),sheet = "dataEducation"))
raceLink <- as.data.frame(read_excel(paste0(standardsPlace,"raceLink.xlsx")))  %>% select(raceCode,CHSI)

#== LOAD AND PROCESS POPULATION DATA ==============================================================

popCounty        <- readRDS(path(upPlace,"/upData/popCounty.RDS")) %>%
                         # filter(raceCode == "Total") %>% select(-raceCode) # JASPO commented this out
                         ungroup()  

popCountySex     <- filter(popCounty,ageGroup == "Total", raceCode == "Total") %>% select(-ageGroup, -raceCode) # no need for ageGroup
popCountySexAgeG <- filter(popCounty,ageGroup != "Total", raceCode == "Total") %>% select(-raceCode)

popStandard         <- ageMap    %>% mutate(ageGroup = ageLabel)

 # == LOAD AND PROCESS DEATH DATA =================================================================

cbdDat0 <- readRDS(secureDataFile)

library(summarytools)

myCOUNTIES <- c("Contra Costa","Butte")

cbdDat0 <- filter(cbdDat0, county %in%  myCOUNTIES , year %in% 2016:2019)


# RECODES AND CALCULATIONS -------------------------------------------------

cbdDat0       <- mutate(cbdDat0,
                         sex     = c("Male","Female")[match(sex,c("M","F"))],
                         age     = as.numeric(age),                                                  # redundant...
                         ICD10   = as.character(ICD10),                                              # redundant...
                         comID   = mssaLink[match(cbdDat0$GEOID,mssaLink[,"GEOID"]),"comID"],   
                       # yll     = leMap[match(cbdDat0$age,leMap[,"Age"]),"LE"],
                         yll     = ifelse(age > 75, 0, 75-age),
                         yearG5  = yearMap[match(year,yearMap[,"year"]),"yearGroup5"], 
                         yearG3  = yearMap[match(year,yearMap[,"year"]),"yearGroup3"]
                        ) 


#cbdDat0    <- rename(cbdDat0,CHSI=raceCode) # remove this step soon by chaning in B2 to use "CHSI" as name rather than raceCode
cbdDat0    <- left_join(cbdDat0,raceLink,by="CHSI")
cbdDat0    <- select(cbdDat0,-CHSI)


########
#### CONSIDER ADDING CALIFONRIA TOTAL HERE




# -- Add Age-Group variable ---------------------------------------------------

aL            <-      ageMap$lAge     # lower age ranges
aU            <- c(-1,ageMap$uAge)    # upper age ranges, plus inital value of "-1" for lower limit
aLabs         <- ageMap$ageLabel 
aMark         <- findInterval(cbdDat0$age,aU,left.open = TRUE)  # vector indicating age RANGE value of each INDIVIDUAL age value
cbdDat0$ageGroup  <- aLabs[aMark]                                   # make new "ageGroup" variable based on two objects above 




# -- Map ICD-10 codes to GBD conditions ----------------------------------------

gbdMap0   <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/icd10_to_CAUSE.xlsx"), sheet="main"))   
allLabels <- sort(gbdMap0$causeCode[!is.na(gbdMap0$causeCode)])

mapICD    <- gbdMap0[!is.na(gbdMap0$CODE),c("CODE","regEx10")]

icdToGroup <- function(inputVectorICD10) {
  Cause   <- rep(NA,length(inputVectorICD10))
  for (i in 1:nrow(mapICD)) {Cause[grepl(mapICD[i,"regEx10"],inputVectorICD10)] <- mapICD[i,"CODE"] } 
  Cause}

cbdDat0$icdCODE  <- icdToGroup(inputVectorICD10=cbdDat0$ICD10)

cbdDat0$icdCODE[cbdDat0$ICD10 %in% c("","000","0000")] <- "cZ02"  # >3500 records have no ICD10 code -- label them as cZ for now


codeLast4      <- str_sub(cbdDat0$icdCODE,2,5)
nLast4         <- nchar(codeLast4)

cbdDat0          <- cbdDat0  %>% mutate(lev0  = "0",
                                        lev1  = str_sub(icdCODE,2,2),
                                        lev2  = str_sub(icdCODE,2,4),
                                        lev3  = ifelse(nLast4 == 4,codeLast4,NA)
                                       )


# -- MORE DATA CLEANING ISSUES (see at bottom of file) ------------------------

# -- SAVE FILE FOR AD HOC ANALYSIS AND ERROR/ISSUE INVESTIGATION --------------


# saveRDS(cbdDat0,  file= path(securePath,"/myData/cbdDat0-INVESTIGATION-FILE.RDS"))

# JK: Left off here

# DEATH MEASURES FUNCTIONS =========================================================================
# ==================================================================================================

# calculateYLLmeasures <- function(group_vars,levLab){
#   
#   dat <- cbdDat0 %>% group_by(.dots = group_vars) %>% 
#   #  dat <- cbdDat0 %>% group_by_(.dots = group_vars) %>% 
#       summarize(Ndeaths = n() , 
#               YLL     = sum(yll,   na.rm = TRUE),     # NEED TO ADD CIs
#               mean.age = mean(age,na.rm=TRUE)
#     ) %>%  ungroup 
#  
#     names(dat)[grep("lev", names(dat))] <- "CAUSE"
#     dat$Level                           <- levLab
#     dat %>%  data.frame
# 
# }
# 
# pois.approx <- function (x, pt = 1, conf.level = 0.95) 
# {
#   Z <- qnorm(0.5 * (1 + conf.level))
#   SE.R <- sqrt(x/pt^2)
#   lower <- x/pt - Z * SE.R
#   upper <- x/pt + Z * SE.R
#   data.frame(x = x, pt = pt, rate = x/pt, se = SE.R, lower = lower, upper = upper, 
#              conf.level = conf.level)
# }
# 
# 
# calculateRates <- function(inData,yearN){
#   transform(inData, 
#             YLLper      = yF*YLL/(yearN*population),
#             cDeathRate  = yF*Ndeaths/(yearN*population),
#             rateSE      = yF*pois.approx(Ndeaths,yearN*population, conf.level = 0.95)$se,
#             rateLCI     = yF*pois.approx(Ndeaths,yearN*population, conf.level = 0.95)$lower,
#             rateUCI     = yF*pois.approx(Ndeaths,yearN*population, conf.level = 0.95)$upper
#   )
# }

library(epitools)

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



# == CALCULATE AGE-ADJUSTED RATES==================================================================
# =================================================================================================

# -- makes dataframeS of all possible combinations -----------------------------
# ------------------------------------------------------------------------------

year     <- data.frame(year     = sort(unique(cbdDat0$year))) # these "vectors" need to be dataframes for the sq merge below to work
CAUSE1   <- data.frame(CAUSE    = allLabels,                                      stringsAsFactors = FALSE) 
CAUSE2   <- data.frame(CAUSE    = CAUSE1[nchar(as.character(CAUSE1$CAUSE)) < 4,], stringsAsFactors = FALSE)
sex      <- data.frame(sex      = c("Male","Female","Total"),                     stringsAsFactors = FALSE)
ageGroup     <- data.frame(ageGroup     = sort(unique(cbdDat0$ageGroup)),                     stringsAsFactors = FALSE)
county   <- data.frame(county   = sort(unique(cbdDat0$county)),                 stringsAsFactors = FALSE)        

fullMatCounty          <- sqldf(" select * from  county cross join year   cross join CAUSE1 cross join sex cross join ageGroup")  %>% mutate(tester=0)



# -- COUNTY (age-adjusted) ----------------------------------------------------
# -----------------------------------------------------------------------------

popCountySexAgeG  <- filter(popCountySexAgeG, county %in% myCOUNTIES)


tA1      <- cbdDat0 %>% group_by(county,year, sex, ageGroup,CAUSE=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA2      <- cbdDat0 %>% group_by(county,year, sex, ageGroup,CAUSE=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 

datAA1 <- bind_rows(tA1,tA2)  %>% ungroup()  # UNGROUP HERE!!!!


ageCounty   <- full_join(fullMatCounty,datAA1 ,by = c("county","year","sex","ageGroup","CAUSE"))  %>%    # merge death data and "fullMatCounty"
               full_join(popCountySexAgeG, by = c("county","year","sex","ageGroup") )             %>%    # merge population
               full_join(popStandard[,c("ageGroup","US2000POP")],          by="ageGroup")                    # merge standard population
 
ageCounty$Ndeaths[is.na(ageCounty$Ndeaths)] <- 0    # if NA deaths in strata change to "0"
ageCounty$YLL[is.na(ageCounty$YLL)]         <- 0    # if NA deaths in strata change to "0"


ageCounty <- filter(ageCounty, ! is.na(CAUSE))

countyAA <- ageCounty %>% group_by(county,year,sex,CAUSE) %>%
  summarize(oDeaths = sum(Ndeaths,na.rm=TRUE),
            aRate   = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000)

countyAA_TOM <- ageCounty %>% group_by(county,year,sex,CAUSE) %>%
  summarize(aRate   = ageadjust.direct(count=Ndeaths, pop=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct(count=Ndeaths, pop=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct(count=Ndeaths, pop=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000) %>%
             select(county,year,sex,CAUSE,aRate_TOM=aRate,l_TOM = aLCI, u_TOM = aUCI)


junk <- full_join(countyAA, countyAA_TOM, by = c("county","year","sex", "CAUSE"))  %>%
                      mutate(JUNKdiff = aRate - aRate_TOM)


#============================================================================
# NO FULLMAT -----------------------------------


ageCountyX   <- full_join(datAA1, popCountySexAgeG, by = c("county","year","sex","ageGroup") )             %>%    # merge population
                full_join(popStandard[,c("ageGroup","US2000POP")],          by="ageGroup")                    # merge standard population

ageCountyX$Ndeaths[is.na(ageCountyX$Ndeaths)] <- 0    # if NA deaths in strata change to "0"
ageCountyX$YLL[is.na(ageCountyX$YLL)]         <- 0    # if NA deaths in strata change to "0"

countyAA_FUN <- ageCountyX %>% group_by(county,year,sex,CAUSE) %>%
  summarize(oDeaths = sum(Ndeaths,na.rm=TRUE),
            aRate   = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE     = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000) %>%
            select(county,year,sex,CAUSE,aRate_NO_FULL=aRate,oDeaths_NO_FULL=oDeaths)



junk <- full_join(junk, countyAA_FUN, by = c("county","year","sex", "CAUSE")) %>%
          filter(oDeaths > 0)

#---------------------------


countyAA_FUN_TOM <- ageCountyX %>% group_by(county,year,sex,CAUSE) %>%
  summarize(aRate   = ageadjust.direct(count=Ndeaths, pop=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct(count=Ndeaths, pop=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct(count=Ndeaths, pop=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE     = ageadjust.direct(count=Ndeaths, pop=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000) %>%
  select(county,year,sex,CAUSE,aRate_NO_FULL_TOM=aRate)









junk <- full_join(junk, countyAA_FUN_TOM, by = c("county","year","sex", "CAUSE")) %>%
  filter(oDeaths > 0)




