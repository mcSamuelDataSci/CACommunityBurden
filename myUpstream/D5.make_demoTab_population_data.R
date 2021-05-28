#### ----- Generating four population data frames for CCB Demographics Tab ------------------------------

####    Pop Data ---- Race -- Sex --------- Age --
####    Age Pyramid    F       T        Increments of 5, 100+
####    RaceAge Bar    T       F        0-14, 15-24, 25-34, 45-64, 75-120
####    Race Pie       T       F             F
####    Trend          T       T        0-14, 15-24, 25-34, 45-64, 75-120



myYear <- 2020

server <- T
#CCB <- F

if (server) {
  
  source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/populationExtract.R")
  source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
  
} else {
  
  source("G:/FusionData/0.CCB/myCCB/Standards/populationExtract.R")
  source("G:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
  
}

# --- Step 1: Generate population files using the standard populationExtract function -------------

# Age groups

ageDF <- data.frame(lAge = seq(0, 95, by = 5), 
                    uAge = seq(4, 99, by = 5)) %>%
  mutate(ageName = paste0(lAge, " - ", uAge)) %>%
  tibble::add_row(lAge = 100, uAge = 120, ageName = "100+")

ageDF2 <- data.frame(lAge = c(0, 15, 25, 35, 45, 65, 75), 
                     uAge = c(14, 24, 34, 44, 64, 74, 120)) %>%
  mutate(ageName = paste0(lAge, " - ", uAge), 
         ageName = ifelse(ageName == "75 - 120", "75+", ageName))

# 2000-most recent year population data - RDS file created in D2

fileName <- paste0(fusionPlace, "Population Data/dof_pop_2000_", myYear, ".RDS")

dof_pop_2000_myYear <- readRDS(file = fileName) %>%
  select(-county)

popData <- populationExtract(County = T, # all uppercase in arguments; explain the ageGroups better; raceCode; 
                             Race   = F,
                             Sex    = T, 
                             Age    = T,
                             Year      = 2000:myYear,
                             ageGroups = c(-1, ageDF$uAge), # Add NA to not include ageGroups
                             ageLabels = ageDF$ageName,
                             raceLabel = "raceName", 
                             CA    = T, 
                             Total = F,
                             multiYear = F, 
                             popData = dof_pop_2000_myYear, 
                             server = T)


popData2 <- populationExtract(County = T, # all uppercase in arguments; explain the ageGroups better; raceCode; 
                              Race   = T,
                              Sex    = F, 
                              Age    = T,
                              Year      = 2000:myYear,
                              ageGroups = c(-1, ageDF2$uAge), # Add NA to not include ageGroups
                              ageLabels = ageDF2$ageName,
                              raceLabel = "raceName", 
                              CA    = T, 
                              Total = F,
                              multiYear = F, 
                              popData = dof_pop_2000_myYear, 
                              server = T)

popDataTrend <- populationExtract(County = T, # all uppercase in arguments; explain the ageGroups better; raceCode; 
                              Race   = T,
                              Sex    = T, 
                              Age    = T,
                              Year      = 2000:myYear,
                              ageGroups = c(-1, ageDF2$uAge), # Add NA to not include ageGroups
                              ageLabels = ageDF2$ageName,
                              raceLabel = "raceName", 
                              CA    = T, 
                              Total = T,
                              multiYear = F, 
                              popData = dof_pop_2000_myYear, 
                              server = T)


# ----- Step 2: Prepare data frames for Demographics tab, save in myCCB/myData/ ----------------------

options(scipen = 999) # To get rid of exponential notation

popData_AgePyramid <- popData %>%
  mutate(county = ifelse(county == "California", "CALIFORNIA", county), 
         ageGroup = factor(ageGroup, levels = ageDF$ageName), 
         population = round(population, 0)) %>%
  left_join(select(raceLink, raceName, raceNameShort), by = "raceName")


popData_RacePie <- popData2 %>%
  mutate(county = ifelse(county == "California", "CALIFORNIA", county)) %>%
  group_by(year, county, raceName) %>%
  summarise(population = sum(population))  %>%
  mutate(population = round(population, 0)) %>%
  left_join(select(raceLink, raceName, raceNameShort), by = "raceName")

popData_RaceAge <- popData2 %>%
  mutate(county = ifelse(county == "California", "CALIFORNIA", county), 
         ageGroup = factor(ageGroup, levels = ageDF2$ageName), 
         population = round(population, 0))  %>%
  left_join(select(raceLink, raceName, raceNameShort), by = "raceName")

popData_trends <- popDataTrend %>%
  mutate(county = ifelse(county == "California", "CALIFORNIA", county), 
         ageGroup = factor(ageGroup, levels = c(ageDF2$ageName, "Total")), 
         population = round(population, 0)) %>%
  left_join(select(raceLink, raceName, raceNameShort), by = "raceName")


saveRDS(popData_AgePyramid,path(ccbData, "popData_AgePyramid.RDS"))
saveRDS(popData_RacePie,path(ccbData,  "popData_RacePie.RDS"))
saveRDS(popData_RaceAge,path(ccbData,    "popData_RaceAge.RDS"))
saveRDS(popData_trends,path(ccbData,    "popData_SexRaceAge_Trends.RDS"))
