server <- F

if (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
if (!server) source("G:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")

# Load death datasets
datCounty_RE <- readRDS(path(ccbData, "real/datCounty_RE.RDS"))
datCounty_AGE_3year <- readRDS(path(ccbData, "real/datCounty_AGE_3year.RDS"))
datCounty_3year <- readRDS(path(ccbData, "real/datCounty_3year.RDS"))

# Load 1-Year Statewide death datasets
datState_Age <- readRDS(paste0(ccbData, "real/datState_AGE.RDS")) %>% 
  mutate(year = as.character(year)) %>% 
  rename(yearG3 = year)

datState_RE <- readRDS(paste0(ccbData, "real/datState_RE.RDS")) %>% 
  mutate(year = as.character(year)) %>% 
  rename(yearG3 = year)

datState_Sex <- readRDS(paste0(ccbData, "real/datCounty.RDS")) %>% 
  filter(sex != "Total", county == "CALIFORNIA") %>% 
  mutate(year = as.character(year)) %>% 
  rename(yearG3 = year)

# Combine 3-Year and 1-Year data
datCounty_RE <- bind_rows(datCounty_RE, datState_RE)
datCounty_AGE_3year <- bind_rows(datCounty_AGE_3year, datState_Age)
datCounty_3year <- bind_rows(datCounty_3year, datState_Sex)

nCut      <- 20

mainYearG3 <- "2020-2022"

# RACE --------------------------------------------------------------------------------------------------------------------------

raceTest <- datCounty_RE %>%  
  filter(raceCode != "Multi") %>% 
  filter(Ndeaths > nCut ) %>%
  select(-YLL,-mean.age,-YLLper,-cDeathRate,-rateLCI,-rateUCI,-YLL.adj.rate,LCI=aLCI,UCI=aUCI)

#LOWEST ----------------------------------------

raceTest2 <- raceTest %>% group_by(county,yearG3,sex,causeCode) %>%
  mutate(bestRate = min(aRate),  #MINIMUM RATE
         bestSE   = aSE)  %>%
  filter(bestRate == aRate) %>%
  mutate(lowRace = raceCode) %>%
  select(-(Ndeaths:aSE),-raceCode)


raceTest_LOW <- left_join(raceTest,raceTest2,by=c("county","yearG3","sex","causeCode")) %>%
  mutate(rateRatio = round(aRate/bestRate,1),
         Ztest = (aRate - bestRate) / sqrt(aSE^2 + bestSE^2),
         pValue = 1-pnorm(Ztest),
         pMark = as.factor(ifelse(aRate==bestRate,"Lowest",ifelse(pValue < .01,"Sig. Higher (p<.01)","No Difference")))
  ) 


# Execute these lines to update RACE data for LGHC MEASURES Shiny and State Health Report
if (1==2) {
  
  raceViewWork  <- raceTest_LOW %>%            
    filter(Level == "lev2" ) %>%
    #  filter(!(causeCode %in% c("A02","D04","E03") ) & Level %in% c("lev2","lev3") )
    filter(yearG3 == mainYearG3,sex=="Total")
  
  
  raceDisparityUnique   <- raceViewWork %>%
    group_by(yearG3,county,causeCode)   %>% 
    mutate(rankX=rank(-rateRatio))  %>% # ranks higher RR for each CONDITION in each County
    filter(rankX==1) %>% select(-rankX) %>%
    ungroup()
  
  
  tNames <- deathCauseLink %>% select(causeCode,causeName, causeNameShort)
  
  ccbRaceDisparity <- raceDisparityUnique %>%
    left_join(tNames,by="causeCode") %>%
    mutate(causeName = ifelse(causeCode=="Z01","Ill-Defined",causeName),
           causeNameShort = ifelse(causeCode=="Z01","Ill-Defined",causeNameShort)) 
  
  whichData <- "real"
  
  saveRDS(ccbRaceDisparity , file= path(ccbData, whichData,"ccbRaceDisparity.RDS"))
  
  
}



#HIGHEST -------------------------------------------

raceTest2 <- raceTest %>% group_by(county,yearG3,sex,causeCode) %>%
  mutate(bestRate = max(aRate),  # MAXIMUM RATE
         bestSE   = aSE)  %>%
  filter(bestRate == aRate) %>%
  mutate(lowRace = raceCode) %>%
  select(-(Ndeaths:aSE),-raceCode)


raceTest_HIGH <- left_join(raceTest,raceTest2,by=c("county","yearG3","sex","causeCode")) %>%
  mutate(rateRatio = round(aRate/bestRate,1),
         Ztest = (aRate - bestRate) / sqrt(aSE^2 + bestSE^2),
         #  pValue = 1-pnorm(Ztest),
         pValue = pnorm(Ztest),
         pMark = as.factor(ifelse(aRate==bestRate,"Highest",ifelse(pValue < .01,"Sig. Lower (p<.01)","No Difference")))
  ) 


# Age ----------------------------------------------------------------------------------------------------------------------------

ageTest <- datCounty_AGE_3year %>%
  filter(Ndeaths > nCut,!is.na(cDeathRate) ) %>%    # need !is.na becuase of tiny number missing age --> NA fix 
  select(-YLL,-mean.age,-YLLper,cDeathRate,LCI=rateLCI,UCI=rateUCI)


# LOWEST 

ageTest2 <- ageTest %>% group_by(county,yearG3,sex,causeCode) %>%
  mutate(bestRate = min(cDeathRate),
         bestSE   = rateSE) %>%
  filter(bestRate == cDeathRate)  %>%
  mutate(lowAge = ageGroup) %>%
  select(-(Ndeaths:UCI),-ageGroup)

ageTest_LOW <- left_join(ageTest,ageTest2,by=c("county","yearG3","sex","causeCode")) %>%
  mutate(rateRatio = round(cDeathRate/bestRate,1),
         Ztest = (cDeathRate - bestRate) / sqrt(rateSE^2 + bestSE^2),
         pValue = 1-pnorm(Ztest),
         pMark = as.factor(ifelse(cDeathRate==bestRate,"Lowest",ifelse(pValue < .01,"Sig. Higher (p<.01)","No Difference")))
  )

# HIGHEST

ageTest2 <- ageTest %>% group_by(county,yearG3,sex,causeCode) %>%
  mutate(bestRate = max(cDeathRate),
         bestSE   = rateSE) %>%
  filter(bestRate == cDeathRate)  %>%
  mutate(lowAge = ageGroup) %>%
  select(-(Ndeaths:UCI),-ageGroup)

ageTest_HIGH <- left_join(ageTest,ageTest2,by=c("county","yearG3","sex","causeCode")) %>%
  mutate(rateRatio = round(cDeathRate/bestRate,1),
         Ztest = (cDeathRate - bestRate) / sqrt(rateSE^2 + bestSE^2),
         pValue = pnorm(Ztest),
         pMark = as.factor(ifelse(cDeathRate==bestRate,"Highest",ifelse(pValue < .01,"Sig. Lower (p<.01)","No Difference")))
  ) 



# Sex -------------------------------------------------------------------------------------------------------------------------------

sexTest <- datCounty_3year %>%  
  filter(Ndeaths > nCut, !is.na(aRate),sex != "Total" ) %>%
  select(-YLL,-mean.age,-YLLper,-cDeathRate,-rateLCI,-rateUCI,-YLL.adj.rate,LCI=aLCI,UCI=aUCI)

# LOWEST ---

sexTest2 <- sexTest %>% group_by(county,yearG3,causeCode) %>%
  mutate(bestRate = min(aRate),
         bestSE   = aSE)  %>%
  filter(bestRate == aRate) %>%
  mutate(lowRace = sex) %>%
  select(-(Ndeaths:aSE),-sex)


sexTest_LOW <- left_join(sexTest,sexTest2,by=c("county","yearG3","causeCode")) %>%
  mutate(rateRatio = round(aRate/bestRate,1),
         Ztest = (aRate - bestRate) / sqrt(aSE^2 + bestSE^2),
         pValue = 1-pnorm(Ztest),
         pMark  = as.factor(ifelse(aRate == bestRate,"Lowest", ifelse(pValue < .01,"Sig. Higher (p<.01)","No Difference")))
  ) 



# HIGHEST ---

sexTest2 <- sexTest %>% group_by(county,yearG3,causeCode) %>%
  mutate(bestRate = max(aRate),
         bestSE   = aSE)  %>%
  filter(bestRate == aRate) %>%
  mutate(lowRace = sex) %>%
  select(-(Ndeaths:aSE),-sex)


sexTest_HIGH <- left_join(sexTest,sexTest2,by=c("county","yearG3","causeCode")) %>%
  mutate(rateRatio = round(aRate/bestRate,1),
         Ztest = (aRate - bestRate) / sqrt(aSE^2 + bestSE^2),
         pValue = pnorm(Ztest),
         pMark = as.factor(ifelse(aRate==bestRate,"Highest",ifelse(pValue < .01,"Sig. Lower (p<.01)","No Difference")))
  ) 


# Save data frame
saveRDS(ageTest_LOW, path(ccbData, "real/disparity_ageLow.RDS"))
saveRDS(ageTest_HIGH, path(ccbData, "real/disparity_ageHigh.RDS"))

saveRDS(raceTest_LOW, path(ccbData, "real/disparity_raceLow.RDS"))
saveRDS(raceTest_HIGH, path(ccbData, "real/disparity_raceHigh.RDS"))

saveRDS(sexTest_LOW, path(ccbData, "real/disparity_sexLow.RDS"))
saveRDS(sexTest_HIGH, path(ccbData, "real/disparity_sexHigh.RDS"))
