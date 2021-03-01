
server <- T
if (!server) source("g:/FusionData/Standards/FusionStandards.R")
if (server) source("/mnt/projects/FusionData/Standards/FusionStandards.R")




popData <- readRDS(path(ccbUpstream, "upData/popDemo_countySexAge.RDS"))
popData2 <- readRDS(path(ccbUpstream, "upData/popDemo_countyRaceAge.RDS"))

ageDF <- data.frame(lAge = seq(0, 95, by = 5), 
                    uAge = seq(4, 99, by = 5)) %>%
  mutate(ageName = paste0(lAge, " - ", uAge)) %>%
  tibble::add_row(lAge = 100, uAge = 120, ageName = "100+")

ageDF2 <- data.frame(lAge = c(0, 15, 25, 45, 75), 
                     uAge = c(14, 24, 34, 64, 120)) %>%
  mutate(ageName = paste0(lAge, " - ", uAge), 
         ageName = ifelse(ageName == "75 - 120", "75+", ageName))


popData_AgePyramid <- popData %>%
  mutate(countyName = ifelse(countyName == "California", "CALIFORNIA", countyName), 
         ageGroup = factor(ageGroup, levels = ageDF$ageName)) %>%
  left_join(select(raceLink, raceName, raceNameShort), by = "raceName")


popData_RacePie <- popData2 %>%
  mutate(countyName = ifelse(countyName == "California", "CALIFORNIA", countyName)) %>%
  group_by(year, countyName, raceName) %>%
  summarise(population = sum(population))  %>%
  left_join(select(raceLink, raceName, raceNameShort), by = "raceName")

popData_RaceAge <- popData2 %>%
  mutate(countyName = ifelse(countyName == "California", "CALIFORNIA", countyName), 
         ageGroup = factor(ageGroup, levels = ageDF2$ageName))  %>%
  left_join(select(raceLink, raceName, raceNameShort), by = "raceName")


saveRDS(popData_AgePyramid,path(ccbData, "popData_AgePyramid.RDS"))
saveRDS(popData_RacePie,path(ccbData,  "popData_RacePie.RDS"))
saveRDS(popData_RaceAge,path(ccbData,    "popData_RaceAge.RDS"))





# --Global constants and settings-----------------------------------

myYear   <-  2019
bestYear <- 2019

mySex     <-  "Total"

