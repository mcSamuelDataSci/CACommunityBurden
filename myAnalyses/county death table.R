temp1 <- filter(datCounty, CAUSE=="0", year == 2018) %>% select(county,Ndeaths,sex) %>% pivot_wider(names_from = sex,values_from = Ndeaths)

temp2 <- filter(datCounty_RE, CAUSE=="0", yearG3 == "2016-2018", sex=="Total") %>% select(county,Ndeaths,raceCode) %>% mutate(Ndeaths=round(Ndeaths/3)) %>% pivot_wider(names_from = raceCode,values_from = Ndeaths)


temp3 <- filter(datCounty_AGE_3year, CAUSE=="0", yearG3 == "2016-2018", sex=="Total") %>% select(county,Ndeaths,ageG) %>% mutate(Ndeaths=round(Ndeaths/3))%>% pivot_wider(names_from = ageG,values_from = Ndeaths)



deathTemp <- left_join(temp1,temp2,by="county") %>% left_join(temp3,by="county") %>%
               arrange(county)  %>% replace(., is.na(.), 0)



write_csv(deathTemp,"CA death table.csv")
