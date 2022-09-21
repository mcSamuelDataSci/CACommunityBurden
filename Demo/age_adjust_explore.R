library(readxl)
library(dplyr)

 ageadjust.direct <- function (count, pop, rate = NULL, stdpop, conf.level = 0.95) {

  if (missing(count)  == TRUE & !missing(pop)   == TRUE & is.null(rate) == TRUE)    count <- rate * pop
  if (missing(pop)    == TRUE & !missing(count) == TRUE & is.null(rate) == TRUE)    pop   <- count/rate
  if (is.null(rate)   == TRUE & !missing(count) == TRUE & !missing(pop) ==  TRUE)   rate   <- count/pop
  
  alpha     <- 1 - conf.level
  cruderate <- sum(count)/sum(pop)
  stdwt     <- stdpop/sum(stdpop)
  dsr       <- sum(stdwt * rate)
  dsr.var   <- sum((stdwt^2) * (count/pop^2))
  
  wm <- max(stdwt/pop)
  gamma.lci <- qgamma(    alpha/2, shape =  (dsr^2)/dsr.var,                 scale = dsr.var/dsr)
  gamma.uci <- qgamma(1 - alpha/2, shape = ((dsr + wm)^2)/(dsr.var +  wm^2), scale = (dsr.var + wm^2)/(dsr + wm))
  c(crude.rate = cruderate, 
    adj.rate   = dsr, 
    lci        = gamma.lci, 
    uci        = gamma.uci)
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
  # alpha     <- 1 - conf.level
  # cruderate <- sum(count)/sum(population)
  # stdwt     <- stdpop/sum(stdpop)
  # dsr       <- sum(stdwt * rate)
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


myPath   <- "Info/"
ageMap   <- as.data.frame(read_excel(paste0(myPath,"Age Group Standard and US Standard 2000 Population.xlsx"),sheet = "data"))
popStandard      <- ageMap  %>% mutate(ageGroup = ageLabel)

#dat1     <- as.data.frame(read_excel(paste0("exploreAge.xlsx"),sheet = "data1"))
dat1     <- as.data.frame(read_excel(paste0("exploreAge.xlsx"),sheet = "data2"))

ageWork <- dat1 %>% full_join(popStandard[,c("ageGroup","US2000POP")],  by="ageGroup")


ageWork$Ndeaths[is.na(ageWork$Ndeaths)] <- 0    # if NA deaths in strata change to "0"   NEED THIS
ageWork$population[is.na(ageWork$population)] <- 0    # if NA deaths in strata change to "0"  #### JUST ADDED



# makes dataframes of all possible combinations
vars      <- data.frame(var1 = unique(dat1$var1),         stringsAsFactors = FALSE)
ageGroups <- data.frame(ageGroup = unique(dat1$ageGroup), stringsAsFactors = FALSE)
library(sqldf)
fullMat   <- sqldf("select * from vars cross join ageGroups")  %>% mutate(tester=0)
ageWork2  <- full_join(dat1, fullMat, by = c("var1", "ageGroup")) %>%
                 full_join(popStandard[,c("ageGroup","US2000POP")],  by="ageGroup")          # merge standard population
# ageWork2$Ndeaths[is.na(ageWork2$Ndeaths)] <- 0    # if NA deaths in strata change to "0"
# ageWork2$population[is.na(ageWork2$population)] <- 0    # if NA deaths in strata change to "0"  #### JUST ADDED



#library(epitools)
ageWorkAA1 <- ageWork %>% group_by(var1) %>%
  summarize(oDeaths       = sum(Ndeaths,na.rm=TRUE),
            aRate         = ageadjust.direct(count=Ndeaths, pop=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI          = ageadjust.direct(count=Ndeaths, pop=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI          = ageadjust.direct(count=Ndeaths, pop=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000)
            
        
  

ageWorkAA2 <- ageWork %>% group_by(var1) %>%
  summarize(oDeaths       = sum(Ndeaths,na.rm=TRUE),
            aRate         = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI          = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI          = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE           = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000)


ageWorkAA3 <- ageWork2 %>% group_by(var1) %>%
  summarize(oDeaths       = sum(Ndeaths,na.rm=TRUE),
            aRate         = ageadjust.direct(count=Ndeaths, pop=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI          = ageadjust.direct(count=Ndeaths, pop=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI          = ageadjust.direct(count=Ndeaths, pop=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000)


ageWorkAA4<- ageWork2 %>% group_by(var1) %>%
  summarize(oDeaths       = sum(Ndeaths,na.rm=TRUE),
            aRate         = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI          = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI          = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE           = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000)



ageWorkAA1
ageWorkAA2
ageWorkAA3
ageWorkAA4



# do something if N > pop!!!
# do something if N is close to pop ????

# if an age group population size is 0       for a strata can aRate be calculated for that strata??? guess so...
# if an age group population size is missing for a strata,  assume 0....
# if an age group N                  0 ... no problem....



junk <- c(1,2,3, 0/0)



junk <- c(1,2,3, 3/0)


sum(junk)
sum(junk,na.rm=TRUE)














# -- makes dataframeS of all possible combinations -----------------------------
# ------------------------------------------------------------------------------
sex      <- data.frame(sex      = c("Male","Female","Total"),       stringsAsFactors = FALSE)
ageGroup <- data.frame(ageGroup = sort(unique(cbdDat0_D$ageGroup)), stringsAsFactors = FALSE)
county   <- data.frame(county   = c(geoMap$countyName,STATE),       stringsAsFactors = FALSE) 

library(sqldf)
fullMatCounty   <- sqldf(" select * from  county cross join sex cross join ageGroup")  %>% mutate(tester=0)

# -- COUNTY (age-adjusted) ----------------------------------------------------



# DATA CLEANING ISSUES 
datAA1 <- filter(datAA1,!is.na(ageGroup)) # remove records with missing age
datAA1 <- filter(datAA1,!is.na(county))   # remove records with missing county

ageCounty   <- full_join(fullMatCounty,datAA1 ,by = c("county","sex","ageGroup"))   %>%    # merge death data and "fullMatCounty"
               full_join(popCountySexAgeG, by = c("county","sex","ageGroup") )      %>%    # merge population
               full_join(popStandard[,c("ageGroup","US2000POP")],  by="ageGroup")          # merge standard population
 
ageCounty$Ndeaths[is.na(ageCounty$Ndeaths)] <- 0    # if NA deaths in strata change to "0"
ageCounty$YLL[is.na(ageCounty$YLL)]         <- 0    # if NA deaths in strata change to "0"

