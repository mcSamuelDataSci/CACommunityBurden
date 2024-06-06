# Global Constants ============================================
gConstants <- list(mainYearG5 = "2018-2022",
                   mainYear = 2022,
                   years = 2020:2022,
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

calcCrude <- function(myDat, myPopDat) {
  
  setnames(myDat, old = "lev2", new = "causeCode")
  
  myDat <- myDat[, by = c("year", "county", "causeCode", "sex", "ageGroup", "raceCode"), 
                 list(Ndeaths = .N, YLL = sum(yll, na.rm = T))]
  
  myDat <- myDat[!is.na(ageGroup), ]
  myDat <- myDat[!is.na(sex), ]
  myDat <- myDat[!raceCode %in% c("Other", "Unknown", NA), ]
  myDat <- myDat[!is.na(year), ]
  myDat <- myDat[!is.na(county), ]
  myDat <- merge(myDat, myPopDat, all.x = TRUE)
  myDat[, c("cDeathRate", "YLLper") := list(100000 * Ndeaths/population, 100000 * YLL / population)
  ]
}

createPopGroupLink <- function(myData, myMainYear, myYearG) {
  
  popGroupLink <- myData[year == myMainYear, ]
  popGroupLink <- popGroupLink[raceCode == "Total" &
                                 ageGroup == "Total" &
                                 sex == "Total", ]
  popGroupLink[, population := population/myYearG]
  popGroupLink[, popGroup := fcase(population <= 20000, "pop20", 
                                   population <= 50000, "pop50", 
                                   population <= 100000, "pop100", 
                                   population <= 250000, "pop250", 
                                   population <= 560000, "pop560", 
                                   population <= 1000000, "pop1M", 
                                   population <= 2000000, "pop2M", 
                                   population > 2000000, "popG2M")]
  popGroupLink[, c("year", "sex", "ageGroup", "raceCode", "population") :=
                 list(NULL, NULL, NULL, NULL, NULL)]
}

createPopGroups <- function(myDat, myPopGroup) {
  
  merge(myDat, myPopGroup, all.x = TRUE, by = "county")
  
}

suppress1 <- function(myDat, myYearG) {
  
  myPopGroup <- unique(myDat$popGroup)
  
  if (myYearG == 1) {
    
    if (myPopGroup == "pop20") {
      return(NULL)
    } else if (myPopGroup == "pop50") {
      return(NULL)
    } else if (myPopGroup == "pop100") {
      return(NULL)
    } else if (myPopGroup == "pop250") {
      return(myDat[sex == "Total" & raceCode == "Total", ])
    } else if (myPopGroup == "pop560") {
      return(myDat[sex == "Total" & raceCode == "Total", ])
    } else if (myPopGroup == "pop1M") {
      return(myDat[raceCode == "Total", ])
    } else if (myPopGroup == "pop2M") {
      myDat1 <- myDat[sex == "Total" & raceCode %in% gConstants$allRaces, ]
      myDat2 <- myDat[raceCode == "Total", ]
      return(rbind(myDat1, myDat2))
    } else if (myPopGroup == "popG2M") {
      myDat1 <- myDat[sex == "Total" & raceCode %in% gConstants$allRaces, ]
      myDat2 <- myDat[sex != "Total" & raceCode %in% gConstants$mainRaces, ]
      myDat3 <- myDat[raceCode == "Total", ]
      return(rbind(myDat1, myDat2, myDat3))
    }
    
  } else if (myYearG == 5){
    
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
        myDat3 <- myDat[raceCode == "Total", ]
        return(rbind(myDat1, myDat2, myDat3))
      } else if (myPopGroup == "pop1M") {
        return(myDat)
      } else if (myPopGroup == "pop2M") {
        return(myDat)
      } else if (myPopGroup == "popG2M") {
        return(myDat)
      }
  }
  
  
}

mySuppress <- function(tDat, critN=11)  {
  
  if (is.null(tDat)) return(NULL)
  
  myDat <- copy(tDat)
  
  myDat[, suppress1 := fifelse(Ndeaths < critN, 1, 0)]
  myDat[, c("rowsSuppressedA", "nextSmallestN") := 
          list(sum(suppress1==1, na.rm=TRUE), 
               min(Ndeaths[Ndeaths >= critN], na.rm=TRUE)), 
               by = .(year, county, causeCode, ageGroup, sex)]
  myDat[, suppress2 := fifelse((rowsSuppressedA == 1 & Ndeaths == nextSmallestN),1,0), 
        by = .(year, county, causeCode, ageGroup, sex)]
  myDat[, supIndicator1 := fifelse(suppress1==1 | suppress2==1,1,0), 
        by = .(year, county, causeCode, ageGroup, sex)]
  

  myDat[, c("rowsSuppressedA", "nextSmallestN") := 
          list(sum(suppress1==1, na.rm=TRUE), 
               min(Ndeaths[Ndeaths >= critN], na.rm=TRUE)), 
        by = .(year, county, causeCode, ageGroup, raceCode)]
  myDat[, suppress2 := fifelse((rowsSuppressedA == 1 & Ndeaths == nextSmallestN),1,0), 
        by = .(year, county, causeCode, ageGroup, raceCode)]
  myDat[, supIndicator2 := fifelse(suppress1==1 | suppress2==1,1,0), 
        by = .(year, county, causeCode, ageGroup, raceCode)]


  myDat[, c("rowsSuppressedA", "nextSmallestN") := 
          list(sum(suppress1==1, na.rm=TRUE), 
               min(Ndeaths[Ndeaths >= critN], na.rm=TRUE)), 
        by = .(year, county, causeCode, sex, raceCode)]
  myDat[, suppress2 := fifelse((rowsSuppressedA == 1 & Ndeaths == nextSmallestN),1,0), 
        by = .(year, county, causeCode, sex, raceCode)]
  myDat[, supIndicator3 := fifelse(suppress1==1 | suppress2==1,1,0), 
        by = .(year, county, causeCode, sex, raceCode)]
  
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

# Create 5 year population
popCounty5 <- merge(popCounty, yearMap[, list(year, yearG5) ], all.x = TRUE)
popCounty5 <- popCounty5[, by = c("yearG5", "county", "sex", "raceCode", "ageGroup"), 
                         list(population = sum(population))]

popRegion5 <- merge(popCounty5, countyLink, all.x = TRUE)
popRegion5 <- popRegion5[, by = c("yearG5", "region", "sex", "raceCode", "ageGroup"), 
                         list(population = sum(population))]
setnames(popRegion5, old = "region", new = "county")
popCounty5 <- rbind(popCounty5, popRegion5)
setnames(popCounty5, old = "yearG5", new = "year")

# Create 1 year population
popRegion <- merge(popCounty, countyLink, all.x = TRUE)
popRegion <- popRegion[, by = c("year", "region", "sex", "raceCode", "ageGroup"), 
                         list(population = sum(population))]
setnames(popRegion, old = "region", new = "county")
popCounty <- rbind(popCounty, popRegion)

# Create popGroups
popGroupLink <- createPopGroupLink(popCounty, gConstants$mainYear, 1)
popGroupLink5 <- createPopGroupLink(popCounty5, gConstants$mainYearG5, 5)

# Read in Investigation Data =======================================================================

cbdDat0 <- readRDS(paste0(securePlace, "myData/cbdDat0-INVESTIGATION-FILE.RDS")) %>% 
  as.data.table()

cbdDat0 <- cbdDat0[, list(year, yearG5, region, county, city_lhj, lev2, sex, ageGroup, raceCode, yll)]
cbdDat1 <- cbdDat0[year %in% gConstants$years, ]
cbdDat1[, c("year", "yearG5") := list(as.character(year), NULL)]
cbdDat5 <- cbdDat0[yearG5 == gConstants$mainYearG5, ]
cbdDat5[, year := NULL]
setnames(cbdDat5, old = "yearG5", new = "year")

# Prepare Data =======================================================

# Separate Data - County, STATE, city LHJ, Region - and aggregate

# 1 Year
datDeaths_county <- getTotals(cbdDat1)

datDeaths_state <- copy(cbdDat1)
datDeaths_state[, county := "CALIFORNIA"]
datDeaths_state <- getTotals(datDeaths_state)

datDeaths_city <- cbdDat1[!is.na(city_lhj), ]
datDeaths_city[, county := city_lhj]
datDeaths_city <- getTotals(datDeaths_city)

datDeaths_region <- copy(cbdDat1)
datDeaths_region[, county := region]
datDeaths_region <- getTotals(datDeaths_region)


datDeathsList <- list(county = datDeaths_county,
                      state = datDeaths_state,
                      city = datDeaths_city, 
                      region = datDeaths_region)

# 5 Year
datDeaths_county <- getTotals(cbdDat5)

datDeaths_state <- copy(cbdDat5)
datDeaths_state[, county := "CALIFORNIA"]
datDeaths_state <- getTotals(datDeaths_state)

datDeaths_city <- cbdDat5[!is.na(city_lhj), ]
datDeaths_city[, county := city_lhj]
datDeaths_city <- getTotals(datDeaths_city)

datDeaths_region <- copy(cbdDat5)
datDeaths_region[, county := region]
datDeaths_region <- getTotals(datDeaths_region)


datDeathsList5 <- list(county = datDeaths_county,
                      state = datDeaths_state,
                      city = datDeaths_city, 
                      region = datDeaths_region)





# Calculate crude
datDeathsList <- lapply(datDeathsList, calcCrude, myPopDat = popCounty)
datDeathsList5 <- lapply(datDeathsList5, calcCrude, myPopDat = popCounty5)

# Create pop groups
datDeathsList <- lapply(datDeathsList, createPopGroups, myPopGroup = popGroupLink)
datDeathsList5 <- lapply(datDeathsList5, createPopGroups, myPopGroup = popGroupLink5)

# Combine
datLC <- rbindlist(list(datDeathsList$county, 
                            datDeathsList$state,
                            datDeathsList$city, 
                            datDeathsList$region))

datLC5 <- rbindlist(list(datDeathsList5$county, 
                        datDeathsList5$state,
                        datDeathsList5$city, 
                        datDeathsList5$region))

# Split by popGroup
datLC_split <- split(datLC, by = "popGroup")
datLC_split <- lapply(datLC_split, suppress1, myYearG = 1)
datLC_split <- lapply(datLC_split, mySuppress)
datLC_final <- rbindlist(datLC_split)
datLC_final[, year := as.numeric(year)]

datLC_split5 <- split(datLC5, by = "popGroup")
datLC_split5 <- lapply(datLC_split5, suppress1, myYearG = 5)
datLC_split5 <- lapply(datLC_split5, mySuppress)
datLC_final5 <- rbindlist(datLC_split5)
setnames(datLC_final5, old = "year", new = "yearG5")

colSums(is.na(datLC_final))
colSums(is.na(datLC_final5))


saveRDS(datLC_final, paste0(ccbData, "real/datLifeCourse.RDS"))
saveRDS(datLC_final5, paste0(ccbData, "real/datLifeCourse_5year.RDS"))
