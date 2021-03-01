library(dplyr)
library(tidyr)
myPath <- "/mnt/projects/fusion/AgeCauseApp/"



# Age, Race, County unique list
ageList <- readRDS(file = paste0(myPath, "data/deathAge.RDS")) %>% 
  ungroup() %>%
  distinct(ageG) %>%
  drop_na() %>%
  pull(ageG)

raceList <- readRDS(file = paste0(myPath, "data/datCounty_RE.RDS")) %>%
  ungroup() %>%
  distinct(raceCode) %>%
  drop_na() %>%
  pull(raceCode)

countyList <- readRDS(file = paste0(myPath, "data/datCounty_RE.RDS")) %>% 
  ungroup() %>%
  distinct(county) %>%
  drop_na() %>%
  pull(county)




rm(myPath)



  rename(ageGroup = ageG)

hospAge <- readRDS(file = path("data/hospAge.rds")) %>% mutate(ccsCode = as.numeric(ccsCode))

edAge <- readRDS(file = path("data/edAge.rds")) %>% mutate(ccsCode = as.numeric(ccsCode)) %>% rename(n_hosp = nED)

deathAge <- readRDS(file = path("data/deathAge.RDS")) %>% rename(ageGroup = ageG)

deathRace <- readRDS(file = path("data/deathRace.RDS"))



datCounty_RE <- readRDS(file = path("data/datCounty_RE.RDS"))

ageList <- unique(deathAge$ageGroup)

raceList <- unique(datCounty_RE$raceCode)

countyList <- unique(deathAge$county)