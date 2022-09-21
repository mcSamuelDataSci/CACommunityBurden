# ============================================================================
# "makeDatasets.R" file   
#
#=============================================================================

# Generate DEMO data:
# library(dplyr)
# secureDataFile <- "g:/FusionData/0.Secure.Data/myData/ccb_processed_deaths.RDS"
# cbdDat0 <- readRDS(secureDataFile)
# cbdDat1 <- cbdDat0 %>%
#             filter(year == 2021) %>%
#             select(age, sex, county) %>%
#             slice_sample(prop = .1)
# saveRDS(cbdDat1,"ca_deaths_sample_2021.RDS")
             


#== LOAD DATA MAPPING FILES and DEFINE CONSTANTS ================================================

library(readxl)

myPath   <- "Info/"
leMap    <- as.data.frame(read_excel(paste0(myPath,"Age to Life-Expectancy Linkage.xlsx"), sheet="LifeExpLink", range = cell_cols("A:B")))
geoMap   <- as.data.frame(read_excel(paste0(myPath,"County Codes to County Names Linkage.xlsx")))
ageMap   <- as.data.frame(read_excel(paste0(myPath,"Age Group Standard and US Standard 2000 Population.xlsx"),sheet = "data"))

yF     <- 100000  # rate constant 
STATE  <- "CALIFORNIA"


#== LOAD AND PROCESS POPULATION DATA ============================================================

library(dplyr)

popCounty        <- readRDS(paste0(myPath, "popCounty.RDS")) %>%
                         ungroup()   %>% #  ungrouping important for subsequent data set merging
                         filter(year == 2021)

popCountySex     <- filter(popCounty,ageGroup == "Total", raceCode == "Total") %>% select(-ageGroup, -raceCode) # no need for ageGroup
popCountySexAgeG <- filter(popCounty,ageGroup != "Total", raceCode == "Total") %>% select(-raceCode)

popStandard      <- ageMap  %>% mutate(ageGroup = ageLabel)


# == LOAD AND PROCESS DEATH DATA =================================================================

 cbdDat0_X <- readRDS("ca_deaths_sample_2021.RDS")

  
  # -- RECODES AND CALCULATIONS -------------------------------------------------
 
 cbdDat0_D       <- mutate(cbdDat0_X,
                             sex     = c("Male","Female")[match(sex,c("M","F"))],
                             age     = as.numeric(age),                                                  
                             yll     = ifelse(age > 75, 0, 75-age)
                           # yll     = leMap[match(cbdDat0$age,leMap[,"Age"]),"LE"],  # IHME Method
                           )

# Duplicate data with sex = Total and then combine
 cbdDat0_DSex   <- mutate(cbdDat0_D, sex = "Total")
 cbdDat0_D      <- bind_rows(cbdDat0_D,cbdDat0_DSex)
  
  
    # -- Add Age-Group variable ---------------------------------------------------

 # create age group variable using one of many possible approaches
 
  aL                  <-  ageMap$lAge       # lower age ranges
  aU                  <- c(-1,ageMap$uAge)  # upper age ranges, plus inital value of "-1" for lower limit
  aLabs               <- ageMap$ageLabel 
  aMark               <- findInterval(cbdDat0_D$age,aU,left.open = TRUE)  # vector indicating age RANGE value of each INDIVIDUAL age value
  cbdDat0_D$ageGroup  <- aLabs[aMark]                                     # make new "ageGroup" variable based on two objects above 


# DEATH MEASURES FUNCTIONS =========================================================================
# ==================================================================================================

calculateYLLmeasures <- function(group_vars){
  
  dat <- cbdDat0_D %>% 
           # group_by(.dots = group_vars) %>% 
            group_by(across({{ group_vars }})) %>% 
            summarize(Ndeaths  = n() , 
                      YLL      = sum(yll,   na.rm = TRUE),     
                      mean.age = mean(age,na.rm=TRUE)    
                      ) %>%  
            ungroup %>%  data.frame
}

  
  
  
# original pois.approx is from epitools package  
pois.approx_sam <- function (x, pt = 1, conf.level = 0.95) {
  Z <- qnorm(0.5 * (1 + conf.level))
  SE.R <- sqrt(x/pt^2)
  lower <- x/pt - Z * SE.R
  upper <- x/pt + Z * SE.R
  data.frame(x = x, 
             pt = pt, 
             rate = x/pt, 
             se = SE.R, 
             lower = lower, 
             upper = upper, 
             conf.level = conf.level)
}



calculateRates <- function(inData,yearN){
  transform(inData, 
            YLLper      = yF*YLL/(yearN*population),
            cDeathRate  = yF*Ndeaths/(yearN*population),
            rateSE      = yF*pois.approx_sam(Ndeaths,yearN*population, conf.level = 0.95)$se,
            rateLCI     = yF*pois.approx_sam(Ndeaths,yearN*population, conf.level = 0.95)$lower,
            rateUCI     = yF*pois.approx_sam(Ndeaths,yearN*population, conf.level = 0.95)$upper
            )
}


# https://github.com/cran/epitools/blob/master/R/ageadjust.direct.R
ageadjust.direct.SAM <- function (count, population, rate = NULL, stdpop, conf.level = 0.95) {
  
  if (missing(count)      == TRUE & !missing(population) == TRUE & is.null(rate)        == TRUE)  count      <- rate * population
  if (missing(population) == TRUE & !missing(count)      == TRUE & is.null(rate)        == TRUE)  population <- count/rate
  if (is.null(rate)       == TRUE & !missing(count)      == TRUE & !missing(population) == TRUE)  rate       <- count/population
  
  rate[is.na(population)]          <- 0
  rate[is.null(population)]        <- 0
  population[is.na(population)]    <- 0
  population[is.null(population)]  <- 0
  
  alpha     <- 1 - conf.level
  cruderate <- sum(count,na.rm=TRUE)/sum(population,na.rm=TRUE)
  stdwt     <- stdpop/sum(stdpop,na.rm=TRUE)
  dsr       <- sum(stdwt * rate,na.rm=TRUE)
  dsr.var   <- sum((stdwt^2) * (count/population^2))
  dsr.se    <- sqrt(dsr.var)
  wm        <- max(stdwt/population)
  gamma.lci <- qgamma(    alpha/2, shape =  (dsr^2)/dsr.var,            scale =  dsr.var/dsr)
  gamma.uci <- qgamma(1 - alpha/2, shape = ((dsr+wm)^2)/(dsr.var+wm^2), scale = (dsr.var+wm^2)/(dsr+wm))
  
  c(crude.rate = cruderate, 
    adj.rate   = dsr, 
    lci        = gamma.lci, 
    uci        = gamma.uci, 
    se         = dsr.se)
}


# == CALCULATE CRUDE RATES ========================================================================
# -- COUNTY -------------------------------------------------------------------


myVector <- c("county","sex")
myVector <- c("county","sex", "year", "condition", "race") #etc.


datCounty <- calculateYLLmeasures(c("county","sex"))
datState  <- calculateYLLmeasures(c(         "sex")) %>% mutate(county = STATE)
datCounty <- bind_rows(datCounty,datState)

# MERGE Death and Population files
datCounty <-  merge(datCounty,popCountySex,by = c("county","sex"))

# CALCULATE RATES
datCounty <- calculateRates(datCounty,1)


# == CALCULATE AGE-ADJUSTED RATES==================================================================

# -- makes dataframeS of all possible combinations -----------------------------
# ------------------------------------------------------------------------------
sex      <- data.frame(sex      = c("Male","Female","Total"),       stringsAsFactors = FALSE)
ageGroup <- data.frame(ageGroup = sort(unique(cbdDat0_D$ageGroup)), stringsAsFactors = FALSE)
county   <- data.frame(county   = c(geoMap$countyName,STATE),       stringsAsFactors = FALSE) 

library(sqldf)
fullMatCounty   <- sqldf(" select * from  county cross join sex cross join ageGroup")  %>% mutate(tester=0)

# -- COUNTY (age-adjusted) ----------------------------------------------------

tA1      <- cbdDat0_D %>% group_by(county, sex, ageGroup) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA2      <- cbdDat0_D %>% group_by(        sex, ageGroup) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)

datAA1 <- bind_rows(tA1,tA2)  %>% ungroup()  

# DATA CLEANING ISSUES 
datAA1 <- filter(datAA1,!is.na(ageGroup)) # remove records with missing age
datAA1 <- filter(datAA1,!is.na(county))   # remove records with missing county

ageCounty   <- full_join(fullMatCounty,datAA1 ,by = c("county","sex","ageGroup"))   %>%    # merge death data and "fullMatCounty"
               full_join(popCountySexAgeG, by = c("county","sex","ageGroup") )      %>%    # merge population
               full_join(popStandard[,c("ageGroup","US2000POP")],  by="ageGroup")          # merge standard population
 
ageCounty$Ndeaths[is.na(ageCounty$Ndeaths)] <- 0    # if NA deaths in strata change to "0"
ageCounty$YLL[is.na(ageCounty$YLL)]         <- 0    # if NA deaths in strata change to "0"


countyAA <- ageCounty %>% group_by(county,sex) %>%
  summarize(oDeaths       = sum(Ndeaths,na.rm=TRUE),
            aRate         = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI          = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI          = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE           = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000, 
            YLL.adj.rate  = ageadjust.direct.SAM(count=YLL,     population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) # CONFIRM


countyAA <- countyAA[!(countyAA$oDeaths==0),c("county","sex","aRate","aLCI","aUCI","aSE","YLL.adj.rate")]  # remove strata with no deaths and select columns  

# == MERGE CRUDE AND AGJECT RATES AND CLEAN UP ====================================================

datCounty <- merge(datCounty,countyAA ,by = c("county","sex"),all=TRUE)

# SMR Calculations --------

datState  <- datCounty  %>% 
               filter(county == STATE) %>%
               mutate(stateCrudeRate = cDeathRate,
               stateAdjustedRate = aRate) %>%
               select(year,sex,stateCrudeRate,stateAdjustedRate)

datCounty  <- datCounty %>%
               left_join(datState,by = c("year","sex")) %>%
                mutate(SMRcrude = cDeathRate / stateCrudeRate,
                       SMR      = aRate      / stateAdjustedRate)


# == CELL SUPRESSION WITH COMPLEMENTARY CELL SUPRESSION==============================================


source(paste0("Info/","suppressionFunction.R"))


gBy       <-  c("county")         # FOR MAIN
datCounty <- mutate(datCounty, supIndicator = mySuppress(datCounty,gBy,"Ndeaths"))
datCounty <- filter(datCounty, 
                    supIndicator != 1)  # %>%   select(-supIndicator)
