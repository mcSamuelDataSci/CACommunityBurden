#### ----- Generating two population data frames for CCB Demographics Tab ------------------------------

#### Pop Data -- Race -- Sex --------- Age --
####    1         F       T        Increments of 5
####    2         T       F        0-14, 15-24, 25-34, 45-64, 75-120


myYear <- 2020

server <- T

if (server) {
  
  source("/mnt/projects/FusionData/Standards/populationExtract.R")
  source("/mnt/projects/FusionData/Standards/FusionStandards.R")
  myPath <- "mnt"
  
} else {
  
  source("G:/FusionData/Standards/populationExtract.R")
  source("G:/FusionData/Standards/FusionStandards.R")
  myPath <- "G:"
  
}



ageDF <- data.frame(lAge = seq(0, 95, by = 5), 
                    uAge = seq(4, 99, by = 5)) %>%
  mutate(ageName = paste0(lAge, " - ", uAge)) %>%
  tibble::add_row(lAge = 100, uAge = 120, ageName = "100+")

ageDF2 <- data.frame(lAge = c(0, 15, 25, 45, 75), 
                     uAge = c(14, 24, 34, 64, 120)) %>%
  mutate(ageName = paste0(lAge, " - ", uAge), 
         ageName = ifelse(ageName == "75 - 120", "75+", ageName))


popData <- populationExtract(County = T, # all uppercase in arguments; explain the ageGroups better; raceCode; 
                             Race   = F,
                             Sex    = T, 
                             Age    = T,
                             Year      = myYear,
                             ageGroups = c(-1, ageDF$uAge), # Add NA to not include ageGroups
                             ageLabels = ageDF$ageName,
                             raceLabel = "raceName", 
                             CA    = T, 
                             Total = F,
                             multiYear = F, 
                             popData = NA, 
                             path = myPath)


popData2 <- populationExtract(County = T, # all uppercase in arguments; explain the ageGroups better; raceCode; 
                              Race   = T,
                              Sex    = F, 
                              Age    = T,
                              Year      = myYear,
                              ageGroups = c(-1, ageDF2$uAge), # Add NA to not include ageGroups
                              ageLabels = ageDF2$ageName,
                              raceLabel = "raceName", 
                              CA    = T, 
                              Total = F,
                              multiYear = F, 
                              popData = NA, 
                              path = myPath)


saveRDS(popData, path(ccbUpstream, "upData/popDemo_countySexAge.RDS"))
saveRDS(popData2, path(ccbUpstream, "upData/popDemo_countyRaceAge.RDS"))
