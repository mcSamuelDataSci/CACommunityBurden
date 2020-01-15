nCut      <- 20
myYearG3  <- "2016-2018"  

# RACE --------------------------------------------------------------------------------------------------------------------------

raceTest <- datCounty_RE %>%  
              filter(raceCode != "Multi-NH") %>% 
              filter(Ndeaths > nCut ) %>%
              select(-YLL,-mean.age,-YLLper,-cDeathRate,-rateLCI,-rateUCI,-YLL.adj.rate,LCI=aLCI,UCI=aUCI)

#LOWEST ----------------------------------------

raceTest2 <- raceTest %>% group_by(county,yearG3,sex,CAUSE) %>%
               mutate(bestRate = min(aRate),  #MINIMUM RATE
                      bestSE   = aSE)  %>%
               filter(bestRate == aRate) %>%
               mutate(lowRace = raceCode) %>%
               select(-(Ndeaths:aSE),-raceCode)

raceTest_LOW <- left_join(raceTest,raceTest2,by=c("county","yearG3","sex","CAUSE")) %>%
                mutate(rateRatio = round(aRate/bestRate,1),
                       Ztest = (aRate - bestRate) / sqrt(aSE^2 + bestSE^2),
                       pValue = 1-pnorm(Ztest),
                       pMark = as.factor(ifelse(aRate==bestRate,"Lowest",ifelse(pValue < .01,"Sig. Higher (p<.01)","No Difference")))
                       ) 


measures <- FALSE
if (measures) {
   measuresRace <- raceTest_LOW %>%
                      filter(yearG3==myYearG3, sex=="Total", Level == "lev2")
   
   write_csv(measuresRace, path("G:","/BurdenView/Data/CCB/raceDisparityX.csv")) 
}



#HIGHEST -------------------------------------------

raceTest2 <- raceTest %>% group_by(county,yearG3,sex,CAUSE) %>%
   mutate(bestRate = max(aRate),  # MAXIMUM RATE
          bestSE   = aSE)  %>%
   filter(bestRate == aRate) %>%
   mutate(lowRace = raceCode) %>%
   select(-(Ndeaths:aSE),-raceCode)


raceTest_HIGH <- left_join(raceTest,raceTest2,by=c("county","yearG3","sex","CAUSE")) %>%
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

 ageTest2 <- ageTest %>% group_by(county,yearG3,sex,CAUSE) %>%
    mutate(bestRate = min(cDeathRate),
           bestSE   = rateSE) %>%
    filter(bestRate == cDeathRate)  %>%
    mutate(lowAge = ageG) %>%
    select(-(Ndeaths:UCI),-ageG)

 ageTest_LOW <- left_join(ageTest,ageTest2,by=c("county","yearG3","sex","CAUSE")) %>%
    mutate(rateRatio = round(cDeathRate/bestRate,1),
           Ztest = (cDeathRate - bestRate) / sqrt(rateSE^2 + bestSE^2),
           pValue = 1-pnorm(Ztest),
           pMark = as.factor(ifelse(cDeathRate==bestRate,"Lowest",ifelse(pValue < .01,"Sig. Higher (p<.01)","No Difference")))
    )

# HIGHEST

ageTest2 <- ageTest %>% group_by(county,yearG3,sex,CAUSE) %>%
   mutate(bestRate = max(cDeathRate),
          bestSE   = rateSE) %>%
   filter(bestRate == cDeathRate)  %>%
   mutate(lowAge = ageG) %>%
   select(-(Ndeaths:UCI),-ageG)

ageTest_HIGH <- left_join(ageTest,ageTest2,by=c("county","yearG3","sex","CAUSE")) %>%
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

sexTest2 <- sexTest %>% group_by(county,yearG3,CAUSE) %>%
   mutate(bestRate = min(aRate),
          bestSE   = aSE)  %>%
   filter(bestRate == aRate) %>%
   mutate(lowRace = sex) %>%
   select(-(Ndeaths:aSE),-sex)


sexTest_LOW <- left_join(sexTest,sexTest2,by=c("county","yearG3","CAUSE")) %>%
   mutate(rateRatio = round(aRate/bestRate,1),
          Ztest = (aRate - bestRate) / sqrt(aSE^2 + bestSE^2),
          pValue = 1-pnorm(Ztest),
          pMark  = as.factor(ifelse(aRate == bestRate,"Lowest", ifelse(pValue < .01,"Sig. Higher (p<.01)","No Difference")))
   ) 



# HIGHEST ---

sexTest2 <- sexTest %>% group_by(county,yearG3,CAUSE) %>%
   mutate(bestRate = max(aRate),
          bestSE   = aSE)  %>%
   filter(bestRate == aRate) %>%
   mutate(lowRace = sex) %>%
   select(-(Ndeaths:aSE),-sex)


sexTest_HIGH <- left_join(sexTest,sexTest2,by=c("county","yearG3","CAUSE")) %>%
   mutate(rateRatio = round(aRate/bestRate,1),
          Ztest = (aRate - bestRate) / sqrt(aSE^2 + bestSE^2),
          pValue = pnorm(Ztest),
          pMark = as.factor(ifelse(aRate==bestRate,"Highest",ifelse(pValue < .01,"Sig. Lower (p<.01)","No Difference")))
) 






