library(summarytools)

junk <- filter(lifeTableCounty,nyrs==1,year==2019,sex=="Total",raceCode=="Total")
freq(junk$county)


junk <- filter(lifeTableCounty,nyrs==3,year==2018,sex=="Total",raceCode=="Total")
freq(junk$county)

junk <- filter(lifeTableCounty,nyrs==5,year==2017,sex=="Total",raceCode=="Total")

freq(junk$county)


mssaLink     <-  read.csv(paste0(ccbInfo,"/Tract to Community Linkage.csv"))  %>% select (-year, -GEOID) %>% unique()


lifeMSSA <- readRDS(paste0(ccbData,"/e0ciMSSA.RDS")) %>% filter(sex=="TOTAL", year==2016) %>%
              left_join(mssaLink,by="comID")


lifeMSSA <- readRDS(paste0(ccbUpstream,"/lifeTables/dataOut/LTciMSSA.RDS")) 


