


vpState <- datCounty %>% filter(county=="CALIFORNIA",year==2014,sex=="Total",CAUSE %in% c("E02","E03"))




stateHa  <- vpState[vpState$CAUSE=="E03","aRate"]
stateHse <- vpState[vpState$CAUSE=="E03","aSE"]

communityHomocide <- datComm %>% filter(sex=="Total",CAUSE %in% c("E03")) %>% 
                                 select(county,comName,oDeaths,pop,aRate,aSE,aLCI,aUCI) %>%
                                 mutate(myZ    = (aRate-stateHa) / sqrt(aSE^2 + stateHse^2)) %>%
                                 mutate(sig    = ifelse(myZ>1.96, "higher", ifelse(myZ < -1.96,"lower","no dif")))




stateSa  <- vpState[vpState$CAUSE=="E02","aRate"]
stateSse <- vpState[vpState$CAUSE=="E02","aSE"]

communitySuicide  <- datComm %>% filter(sex=="Total",CAUSE %in% c("E02")) %>% 
                                 select(county,comName,oDeaths,pop,aRate,aSE,aLCI,aUCI) %>%
                                 mutate(myZ    = (aRate-stateSa) / sqrt(aSE^2 + stateSse^2)) %>%
                                 mutate(sig    = ifelse(myZ>1.96, "higher", ifelse(myZ < -1.96,"lower","no dif")))






write.csv(communityHomocide,(paste0(upPlace,"/tempOutput/communityHomocide.csv")))
write.csv(communitySuicide,(paste0(upPlace,"/tempOutput/communitySuicide.csv")))





