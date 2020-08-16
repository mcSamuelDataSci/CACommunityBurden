
nCut      <- 20

raceTest <- datCounty_RE %>%  
  filter(raceCode != "Multi-NH") %>% 
  filter(Ndeaths > nCut ) %>%
  select(-YLL,-mean.age,-YLLper,-cDeathRate,-rateLCI,-rateUCI,-YLL.adj.rate)


raceTest2 <- raceTest %>% group_by(county,yearG3,sex,CAUSE) %>%
  mutate(bestRate = min(aRate),
         bestSE   = aSE)  %>%
  filter(bestRate == aRate) %>%
  mutate(lowRace = raceCode) %>%
  select(-(Ndeaths:aSE),-raceCode)


raceTest <- left_join(raceTest,raceTest2,by=c("county","yearG3","sex","CAUSE")) %>%
  mutate(rateRatio = round(aRate/bestRate,3),
         Ztest = (aRate - bestRate) / sqrt(aSE^2 + bestSE^2),
         pValue = 1-pnorm(Ztest),
         pMark = as.factor(ifelse(aRate==bestRate,"Lowest",ifelse(pValue < .01,"Higher","Same")))
  ) 


raceViewWork  <- raceTest %>%            
                     filter(Level == "lev2" ) %>%
                     #  filter(!(CAUSE %in% c("A02","D04","E03") ) & Level %in% c("lev2","lev3") )
                     filter(yearG3 == "2016-2018",sex=="Total")


raceDisparityUnique   <- raceViewWork %>%
  group_by(yearG3,county,CAUSE)   %>% 
  mutate(rankX=rank(-rateRatio))  %>% # ranks higher RR for each CONDITION in each County
  filter(rankX==1) %>% select(-rankX) %>%
  ungroup()


tNames <- gbdMap0 %>% select(CAUSE=LABEL,causeName=nameOnly)

ccbRaceDisparity <- raceDisparityUnique %>%
  left_join(tNames,by="CAUSE") %>%
  mutate(causeName = ifelse(CAUSE=="Z01","Ill-Defined",causeName)) 



