# Global Constants ============================================
gConstants <- list(mainYearG5 = "2018-2022", 
                   mainRaces = c("Asian", "Black", "Hisp", "White"), 
                   allRaces = c("AIAN", "Asian", "Black", "Hisp", "NHPI", "White"))

# Load standards -=========================================
server <- F
if (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
if (!server) source("G:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")

# Load packages ========================================
library(data.table)

# Functions ===========================================================================================

getTotals <- function(myDat) {
  
  # Add Total Race
  datDeaths_race <- copy(myDat)
  datDeaths_race[, raceCode := "Total"]
  
  rbind(myDat, datDeaths_race)
  
}

calcCrude <- function(myDat) {
  
  setnames(myDat, old = "lev2", new = "causeCode")
  
  myDat <- myDat[, by = c("yearG5", "county", "causeCode", "sex", "ageGroup", "raceCode"), 
                 list(Ndeaths = .N, YLL = sum(yll, na.rm = T))]
  
  myDat <- myDat[!is.na(ageGroup), ]
  myDat <- myDat[!is.na(sex), ]
  myDat <- myDat[!raceCode %in% c("Other", "Unknown", NA), ]
  myDat <- myDat[!yearG5 %in% c("2000-2002", NA), ]
  myDat <- myDat[!is.na(county), ]
  myDat <- merge(myDat, popCounty5, all.x = TRUE)
  myDat[, c("cDeathRate", "YLLper") := list(100000 * Ndeaths/population, 100000 * YLL / population)
  ]
}

createPopGroups <- function(myDat) {
  
  merge(myDat, popGroupLink, all.x = TRUE, by = "county")
  
}

suppress1 <- function(myDat) {
  
  myPopGroup <- unique(myDat$popGroup)
  if (myPopGroup == "pop20") {
    return(myDat[sex == "Total" & raceCode == "Total", ])
  } else if (myPopGroup == "pop50") {
    return(myDat[raceCode == "Total", ])
  } else if (myPopGroup == "pop100") {
    myDat1 <- myDat[sex == "Total" & raceCode %in% gConstants$mainRaces, ]
    myDat2 <- myDat[raceCode == "Total", ]
    return(rbind(myDat1, myDat2))
  } else if (myPopGroup == "pop250") {
    myDat1 <- myDat[sex == "Total" & raceCode %in% gConstants$allRaces, ]
    myDat2 <- myDat[raceCode == "Total", ]
    return(rbind(myDat1, myDat2))
  } else if (myPopGroup == "pop560") {
    myDat1 <- myDat[sex == "Total" & raceCode %in% gConstants$allRaces, ]
    myDat2 <- myDat[sex != "Total" & raceCode %in% gConstants$mainRaces, ]
    return(rbind(myDat1, myDat2))
  } else if (myPopGroup == "pop1M") {
    return(myDat)
  } else if (myPopGroup == "pop2M") {
    return(myDat)
  } else if (myPopGroup == "popG2M") {
    return(myDat)
  }
  
}

mySuppress <- function(tDat, critN=11)  {
  
  myDat <- copy(tDat)
  
  myDat[, suppress1 := fifelse(Ndeaths < critN, 1, 0)]
  myDat[, c("rowsSuppressedA", "nextSmallestN") := 
          list(sum(suppress1==1, na.rm=TRUE), 
               min(Ndeaths[Ndeaths >= critN], na.rm=TRUE)), 
               by = .(yearG5, county, causeCode, ageGroup, sex)]
  myDat[, suppress2 := fifelse((rowsSuppressedA == 1 & Ndeaths == nextSmallestN),1,0), 
        by = .(yearG5, county, causeCode, ageGroup, sex)]
  myDat[, supIndicator1 := fifelse(suppress1==1 | suppress2==1,1,0), 
        by = .(yearG5, county, causeCode, ageGroup, sex)]
  

  myDat[, c("rowsSuppressedA", "nextSmallestN") := 
          list(sum(suppress1==1, na.rm=TRUE), 
               min(Ndeaths[Ndeaths >= critN], na.rm=TRUE)), 
        by = .(yearG5, county, causeCode, ageGroup, raceCode)]
  myDat[, suppress2 := fifelse((rowsSuppressedA == 1 & Ndeaths == nextSmallestN),1,0), 
        by = .(yearG5, county, causeCode, ageGroup, raceCode)]
  myDat[, supIndicator2 := fifelse(suppress1==1 | suppress2==1,1,0), 
        by = .(yearG5, county, causeCode, ageGroup, raceCode)]


  myDat[, c("rowsSuppressedA", "nextSmallestN") := 
          list(sum(suppress1==1, na.rm=TRUE), 
               min(Ndeaths[Ndeaths >= critN], na.rm=TRUE)), 
        by = .(yearG5, county, causeCode, sex, raceCode)]
  myDat[, suppress2 := fifelse((rowsSuppressedA == 1 & Ndeaths == nextSmallestN),1,0), 
        by = .(yearG5, county, causeCode, sex, raceCode)]
  myDat[, supIndicator3 := fifelse(suppress1==1 | suppress2==1,1,0), 
        by = .(yearG5, county, causeCode, sex, raceCode)]
  
  myDat[, supIndicator := supIndicator1 + supIndicator2 + supIndicator3]
  
  myDat <- myDat[supIndicator == 0, ]
  myDat[, c("suppress1", "rowsSuppressedA", "nextSmallestN", "suppress2", "supIndicator1", "supIndicator2", "supIndicator3", "supIndicator") :=
          list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL)]
  
}

# Load info files ==================================================================================
yearMap <- read_excel(paste0(ccbInfo, "Year to Year-Group Linkage.xlsx")) %>% as.data.table()
yearMap[, c("year", "midYear5", "midYear3") := list(as.character(year), NULL, NULL)]
setnames(yearMap, old = c("yearGroup3", "yearGroup5"), new = c("yearG3", "yearG5"))
countyLink <- read_excel(paste0(standardsPlace, "countyLink.xlsx")) %>% as.data.table()
countyLink <- countyLink[, list(countyName, RPHO)]
setnames(countyLink, old = c("countyName", "RPHO"), new = c("county", "region"))

# Read in Population Data =====================================================================
popCounty <- readRDS(paste0(ccbUpstream, "upData/lhj-population-ars.RDS")) %>% as.data.table()
popCounty[, year := as.character(year)]

popCounty5 <- merge(popCounty, yearMap[, list(year, yearG5) ], all.x = TRUE)
popCounty5 <- popCounty5[, by = c("yearG5", "county", "sex", "raceCode", "ageGroup"), 
                         list(population = sum(population))]

popRegion5 <- merge(popCounty5, countyLink, all.x = TRUE)
popRegion5 <- popRegion5[, by = c("yearG5", "region", "sex", "raceCode", "ageGroup"), 
                         list(population = sum(population))]
setnames(popRegion5, old = "region", new = "county")

popCounty5 <- rbind(popCounty5, popRegion5)

# Create popGroups
popGroupLink <- popCounty5[yearG5 == gConstants$mainYearG5, ]
popGroupLink <- popGroupLink[raceCode == "Total" &
                               ageGroup == "Total" &
                               sex == "Total", ]
popGroupLink[, population := population/5]
popGroupLink[, popGroup := fcase(population <= 20000, "pop20", 
                          population <= 50000, "pop50", 
                          population <= 100000, "pop100", 
                          population <= 250000, "pop250", 
                          population <= 560000, "pop560", 
                          population <= 1000000, "pop1M", 
                          population <= 2000000, "pop2M", 
                          population > 2000000, "popG2M")]
popGroupLink[, c("yearG5", "sex", "ageGroup", "raceCode", "population") :=
               list(NULL, NULL, NULL, NULL, NULL)]

# Read in Investigation Data =======================================================================

cbdDat0 <- readRDS(paste0(securePlace, "myData/cbdDat0-INVESTIGATION-FILE.RDS")) %>% 
  as.data.table()

cbdDat0 <- cbdDat0[, list(yearG5, region, county, city_lhj, lev2, sex, ageGroup, raceCode, yll)]

# Prepare Data =======================================================

# Separate Data - County, STATE, city LHJ, Region - and aggregate
datDeaths_county <- getTotals(cbdDat0)

datDeaths_state <- copy(cbdDat0)
datDeaths_state[, county := "CALIFORNIA"]
datDeaths_state <- getTotals(datDeaths_state)

datDeaths_city <- cbdDat0[!is.na(city_lhj), ]
datDeaths_city[, county := city_lhj]
datDeaths_city <- getTotals(datDeaths_city)

datDeaths_region <- copy(cbdDat0)
datDeaths_region[, county := region]
datDeaths_region <- getTotals(datDeaths_region)


datDeathsList <- list(county = datDeaths_county,
                      state = datDeaths_state,
                      city = datDeaths_city, 
                      region = datDeaths_region)

# Calculate crude
datDeathsList2 <- lapply(datDeathsList, calcCrude)

# Create pop groups
datDeathsList3 <- lapply(datDeathsList2, createPopGroups)

# Combine
datLC <- rbindlist(list(datDeathsList3$county, 
                            datDeathsList3$state,
                            datDeathsList3$city, 
                            datDeathsList3$region))

# Split by popGroup
datLC_split <- split(datLC, by = "popGroup")

datLC_split2 <- lapply(datLC_split, suppress1)

datLC_split4 <- lapply(datLC_split3, mySuppress)

datLC_final <- rbindlist(datLC_split4)

saveRDS(datLC_final, paste0(ccbData, "real/datLifeCourse.RDS"))
