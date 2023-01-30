 server <- T
 if (!server) source("g:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
 if (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")


popCounty        <- readRDS(path(ccbUpstream,"upData/popCounty.RDS")) %>%
                         ungroup()  



popCountySexAgeG <- filter(popCounty,ageGroup != "Total", raceCode == "Total", sex== "Total", county=="CALIFORNIA") %>% select(-raceCode)
popCountySexAgeGTOTAL <- filter(popCounty,ageGroup == "Total", raceCode == "Total", sex== "Total", county=="CALIFORNIA") %>% select(-raceCode)


ggplot(data=popCountySexAgeG, aes(x=year,y=population,col=ageGroup)) + geom_line(size=2)



junk <- pivot_wider(popCountySexAgeG, names_from = year, values_from = population)
