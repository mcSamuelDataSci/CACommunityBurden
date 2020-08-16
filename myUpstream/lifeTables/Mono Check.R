


deaths <- filter(dxcounty,GEOID %in% c("06049000000","06051000000","06091000000"),sex=="TOTAL",year > 2010)
D <- group_by(deaths,GEOID,year) %>% summarize(deaths=sum(dx))



pop <- filter(nxcounty,GEOID %in% c("06049000000","06051000000","06091000000"),sex=="TOTAL",year > 2010)
P   <- group_by(pop,GEOID,year) %>% summarize(pop=sum(nx))


test <- left_join(D,P,by=c("GEOID","year"))  %>% mutate(ratey = round(100000*deaths/pop))



test <- filter(datCounty_3year,CAUSE=="0",sex=="Total",yearG3=="2016-2018") %>%
                   select(county,Ndeaths,pop,cDeathRate,aRate) %>% mutate(dRatio = round(cDeathRate/aRate,2))
