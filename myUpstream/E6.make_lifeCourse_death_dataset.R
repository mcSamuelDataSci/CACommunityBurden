# Load Standards ==========================================
server <- F
if (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
if (!server) source("G:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")

# Functions ===================================================
popGroups <- function(myPop) {
  case_when(myPop <= 20000 ~ "pop20", 
            myPop <= 50000 ~ "pop50", 
            myPop <= 100000 ~ "pop100", 
            myPop <= 250000 ~ "pop250", 
            myPop <= 560000 ~ "pop560", 
            myPop <= 1000000 ~ "pop1M", 
            myPop <= 2000000 ~ "pop2M", 
            myPop > 2000000 ~ "popG2M")
}

getCrude <- function(myDeathData, yearG = 1) { # Options for yearG: 1, 5
  myYearVar <- ifelse(yearG == 1, "year", "yearG5")
  if (yearG == 1) myPopData <- popCounty else myPopData <- popCounty5
  if (yearG == 1) myDDG <- ddg1 else myDDG <- ddg5
  
  myDeathData %>% 
    group_by(!!as.symbol(myYearVar), county, causeCode, sex, raceCode, ageGroup) %>% 
    summarise(Ndeaths = n()) %>% 
    ungroup() %>% 
    left_join(myPopData) %>% 
    mutate(cDeathRate = 100000 * Ndeaths / population) %>% 
    left_join(myDDG, by = c("popGroup" = "Variable"))
}

dataSuppress1 <- function(myData, yearG = 1) {
  
  if (yearG == 1) tDDG <- ddg1 else tDDG <- ddg5
  
  tGroup <- unique(myData$popGroup)
  tDDG <- filter(tDDG, Variable == tGroup)
  
  if (nrow(tDDG) == 0) {
    myData <- myData %>% 
      filter(junk == "junk")
    
    return(myData)
  } else {
    tDDG <- unique(myData$junk)
  }
  
  mainRaces <- c("Asian", "Black", "Hisp", "White", "Total")
  allRaces <- c(mainRaces, "AIAN", "NHPI", "Multi")
  
  # Sex
  if (grepl("sexT", tDDG)) {
    myData <- myData %>% filter(sex == "Total")
  } 
  
  # Race and Ethnicity
  if (grepl("raceT", tDDG)) {
    myData <- myData %>% filter(raceCode == "Total")
  } else if (grepl("raceM", tDDG)) {
    myData <- myData %>% filter(raceCode %in% mainRaces)
  } else {
    myData <- myData %>% filter(raceCode %in% allRaces)
  }
  
  return(myData)
  
}

# Read in Info Files ==============================================
ddg <- read_excel(paste0(ccbUpstream, "upstreamInfo/DDG Scores/DDG Score - Life Course.xlsx")) %>% 
  select(1, matches("_5|_1")) %>% 
  filter(grepl("pop", Variable)) %>% 
  mutate(across(starts_with("sex"), ~as.numeric(.x)))%>% 
  pivot_longer(-Variable, names_to = "junk", values_to = "ddg_score") %>% 
  filter(ddg_score <= 12) %>% 
  mutate(yearG = ifelse(grepl("1", junk), 1, 5)) %>% 
  group_by(Variable, yearG) %>% 
  filter(ddg_score == max(ddg_score)) %>% 
  ungroup() 

ddg1 <- ddg %>% 
  filter(yearG == 1) %>% 
  select(-yearG)

ddg5 <- ddg %>% 
  filter(yearG == 5) %>% 
  select(-yearG)

yearMap <- as.data.frame(read_excel(paste0(ccbInfo,"Year to Year-Group Linkage.xlsx"), sheet = "main")) %>% 
  select(year, yearG5 = yearGroup5)

# Read and prepare population data ===================================
popCounty <- readRDS(paste0(ccbUpstream, "upData/lhj-population-ars.RDS")) %>% 
  ungroup() 

popCounty5 <- popCounty %>% 
  left_join(yearMap) %>% 
  group_by(yearG5, county, sex, raceCode, ageGroup) %>% 
  summarise(population = sum(population)) %>% 
  ungroup()

# Get residence population groups - Using 5 year average
popGroup <- popCounty5 %>% 
  filter(yearG5 == yearGrp5, sex == "Total", raceCode == "Total", ageGroup == "Total") %>% 
  mutate(popGroup = popGroups(population/5)) %>% 
  select(county, popGroup)

# Final popCounty 
popCounty <- popCounty %>% 
  left_join(popGroup)

popCounty5 <- popCounty5 %>% 
  left_join(popGroup)

# Read and Prepare Death Data =========================================
cbdDat0 <- readRDS(file= path(securePlace,"myData/cbdDat0-INVESTIGATION-FILE.RDS"))
cbdDat1 <- cbdDat0 %>% 
  select(year, yearG5, county, city_lhj, sex, raceCode, ageGroup, causeCode = lev2)

# Bring in Race Total
cbdDat1_race <- cbdDat1 %>% mutate(raceCode = "Total")
cbdDat2 <- bind_rows(cbdDat1, cbdDat1_race)

# Create data for state
cbdDat2_state <- cbdDat2 %>% mutate(county = "CALIFORNIA")

# Creadte data for city_lhj
cbdDat2_cityLHJ <- cbdDat2 %>% 
  filter(!is.na(city_lhj)) %>% 
  mutate(county = city_lhj)

# Remove large objects no longer needed
rm(cbdDat0, cbdDat1, cbdDat1_race, popGroup)

# Aggregate; Merge Death and Population Data; Calculate Crude Rates; Join DDG ============================================
lc1 <- bind_rows(
  getCrude(cbdDat2, 1), 
  getCrude(cbdDat2_state, 1), 
  getCrude(cbdDat2_cityLHJ, 1)
)

lc5 <- bind_rows(
  getCrude(cbdDat2, 5), 
  getCrude(cbdDat2_state, 5), 
  getCrude(cbdDat2_cityLHJ, 5)
)

rm(cbdDat2, cbdDat2_state, cbdDat2_cityLHJ)

# Data Suppression part 1 ===========================================

lcSplit1 <- split(lc1, ~popGroup)
lc1_sup1 <- lapply(lcSplit1, dataSuppress1, 1)

lcSplit5 <- split(lc5, ~popGroup)
lc5_sup1 <- lapply(lcSplit5, dataSuppress1, 5)

