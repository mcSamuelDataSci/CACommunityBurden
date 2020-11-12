popWork <- popCounty %>% 
              filter(sex=="Total") %>%
              group_by(year,county) %>%
              summarise(pop=sum(pop))


pop2017 <- filter(popWork,year==2017)
