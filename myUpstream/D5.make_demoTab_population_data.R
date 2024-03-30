#### ----- Generating four population data frames for CCB Demographics Tab ------------------------------

####    Pop Data ---- Race -- Sex --------- Age --
####    Age Pyramid    F       T        Increments of 5, 100+
####    RaceAge Bar    T       F        0-14, 15-24, 25-34, 45-64, 75-120
####    Race Pie       T       F             F
####    Trend          T       T        0-14, 15-24, 25-34, 45-64, 75-120


options(scipen = 999) # To get rid of exponential notation
myYear <- 2022


# Load standards =======================================================================================================
server <- F
if (server) {
  source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
  
} else {
  source("G:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
}

# Read in LHJ ARS Population Data =======================================================================================

lhjARS_demoTab1 <- readRDS(paste0(ccbUpstream, "upData/lhj-population-ars-demoTab1.RDS"))
lhjARS_demoTab2 <- readRDS(paste0(ccbUpstream, "upData/lhj-population-ars-demoTab2.RDS")) 

# Age links ===========================================================================================================

ageDF <- data.frame(lAge = seq(0, 95, by = 5), 
                    uAge = seq(4, 99, by = 5)) %>%
  mutate(ageName = paste0(lAge, " - ", uAge)) %>%
  tibble::add_row(lAge = 100, uAge = 120, ageName = "100+")

ageDF2 <- data.frame(lAge = c(0, 15, 25, 35, 45, 65, 75), 
                     uAge = c(14, 24, 34, 44, 64, 74, 120)) %>%
  mutate(ageName = paste0(lAge, " - ", uAge), 
         ageName = ifelse(ageName == "75 - 120", "75+", ageName))

# Prepare data frames for Demographics tab =========================================================================

popData_AgePyramid <- lhjARS_demoTab1 %>%
  ungroup() %>%
  filter(sex != "Total", ageGroup != "Total", raceCode == "Total") %>% 
  mutate(ageGroup = factor(ageGroup, levels = ageDF$ageName), 
         population = round(population, 0)) %>%
  left_join(select(raceLink, raceCode, raceName, raceNameShort), by = "raceCode") %>% 
  select(-raceCode)

popData_RacePie <- lhjARS_demoTab2 %>%
  ungroup() %>%
  filter(sex == "Total", ageGroup == "Total", raceCode != "Total") %>% 
  group_by(year, county, raceCode) %>%
  summarise(population = sum(population))  %>%
  mutate(population = round(population, 0)) %>%
  left_join(select(raceLink, raceCode, raceName, raceNameShort), by = "raceCode") %>% 
  select(-raceCode)

popData_RaceAge <- lhjARS_demoTab2 %>%
  ungroup() %>%
  filter(sex == "Total", ageGroup != "Total", raceCode != "Total") %>% 
  mutate(ageGroup = factor(ageGroup, levels = ageDF2$ageName), 
         population = round(population, 0))  %>%
  left_join(select(raceLink, raceCode, raceName, raceNameShort), by = "raceCode") %>% 
  select(-raceCode)

popData_trends <- lhjARS_demoTab2 %>%
  ungroup() %>%
  mutate(ageGroup = factor(ageGroup, levels = c(ageDF2$ageName, "Total")), 
         population = round(population, 0)) %>%
  left_join(select(raceLink, raceCode, raceName, raceNameShort), by = "raceCode") %>% 
  select(-raceCode)

# SAVE DATA ===========================================================================================================

saveRDS(popData_AgePyramid,path(ccbData, "popData_AgePyramid.RDS"))
saveRDS(popData_RacePie,path(ccbData,  "popData_RacePie.RDS"))
saveRDS(popData_RaceAge,path(ccbData,    "popData_RaceAge.RDS"))
saveRDS(popData_trends,path(ccbData,    "popData_SexRaceAge_Trends.RDS"))

