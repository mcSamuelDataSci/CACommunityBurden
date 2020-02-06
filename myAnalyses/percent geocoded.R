securePath     <- "g:/0.Secure.Data/"
secureDataFile <- paste0(securePath,"myData/cbdDat0FULL.R") 
load(secureDataFile)


tractWork0 <- cbdDat0FULL %>% filter(year > 2004) %>%
                          mutate(geocodeInd = ifelse(is.na(GEOID) | GEOID == "",0,1))

caTemp     <- tractWork0 %>% mutate(county     ="CALIFORNIA",
                                    countyFIPS ="")
tractWork  <- bind_rows(tractWork0,caTemp)

tractTab   <-  tractWork %>% group_by(year,county,countyFIPS) %>%
                             summarise(count=n(),
                                       percentGeocoded = mean(geocodeInd)) %>% 
                             ungroup()

StatePercentGeocoded <- filter(tractTab,county=="CALIFORNIA") %>% select(-county,-countyFIPS)
write.csv(StatePercentGeocoded,"CA_Deaths_Geocoded_Percent.csv")


geocodeFactor   <- tractWork0 %>% filter(year %in% 2014:2018, !is.na(county)) %>%
                                  group_by(county)            %>%                             
                                  summarise(count=n(),
                                  percentGeocoded = mean(geocodeInd)) %>% 
                                  ungroup()
infoPath <- paste0("f:/0.CBD/myCBD/myInfo/") 
write.csv(geocodeFactor,paste0(infoPath,"geocodeFactor.csv"),row.names = FALSE)
