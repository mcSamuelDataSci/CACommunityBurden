# Documentation ===========================================================================================

# Author: Jaspreet Kang
# Date: 01/26/2024

# About:
# This script needs to be ran only once (when new Decennial comes out)
# Extracts City and County age-race-sex population data from Decennial 2000, 2010, and 2020.

# Output:
# decennial-city-ars.RDS (used in myUpstream/D2..R)
# decennial-county-ars.RDS (used in myUpstream/D2..R)

# Notes:
# State level = California
# County level = 58 California counties
# City LHJ level = Berkeley, Long Beach, Pasadena
# Census has R/E group 'Other' whereas DOF does not. 'Other' is redistributed across all R/E groups

# Load standards ====================================================================================
server <- F
if (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
if (!server) source("G:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")

# Extract and process Census Decennial =============================================================================

library(tidycensus) 

# Get 2000, 2010, 2020 Decennial table IDs for Sex By Age by Race/Ethnicity ---------------------------------

## 2000 ---------------------------------
censusVars2000 <- tidycensus::load_variables(year = 2000, dataset = "sf1")

censusVars2000_total <- censusVars2000 %>% 
  filter(name == "P001001")

censusVars2000_race <- censusVars2000 %>% 
  filter(grepl("SEX BY AGE", concept)) %>% 
  filter(grepl("HISPANIC OR LATINO", concept)) %>% 
  filter(grepl("209", concept)) %>% 
  mutate(tableID = substr(name, 1, 7))

## 2010 -----------------------------------
censusVars2010 <- tidycensus::load_variables(year = 2010, dataset = "sf1")

censusVars2010_total <- censusVars2010 %>% 
  filter(name == "P001001")

censusVars2010_race <- censusVars2010 %>% 
  filter(grepl("SEX BY AGE [(]", concept)) %>% 
  filter(grepl("HISPANIC OR LATINO", concept)) %>% 
  filter(grepl("PCT", name)) %>% 
  mutate(tableID = substr(name, 1, 7))

## 2020 -----------------------------------
censusVars2020 <- tidycensus::load_variables(year = 2020, dataset = "dhc") 

censusVars2020_total <- censusVars2020 %>% 
  filter(name == "P1_001N")

censusVars2020_race <- censusVars2020 %>% 
  filter(grepl("SEX BY SINGLE-YEAR AGE [(]", concept)) %>% 
  filter(grepl("HISPANIC OR LATINO", concept)) %>% 
  mutate(tableID = substr(name, 1, 6))


# Function to create Census Linkages ---------------------
createCensusLink <- function(myData) {
  
  tDat <- myData %>% 
    mutate(raceCode = case_when(grepl("WHITE", concept) ~ "White", 
                                grepl("BLACK", concept) ~ "Black", 
                                grepl("ALASKA", concept) ~ "AIAN", 
                                grepl("ASIAN", concept) ~ "Asian", 
                                grepl("HAWAIIAN", concept) ~ "NHPI", 
                                grepl("OTHER", concept) ~ "Other", 
                                grepl("TWO", concept) ~ "Multi", 
                                TRUE ~ "Hisp"), 
           sex = case_when(grepl("Female", label) ~ "Female", 
                           grepl("Male", label) ~ "Male", 
                           TRUE ~ "Total"))
  
  if (substr(tDat$label[1], 2, 2) == "!") {
    tDat <- tDat %>% 
      mutate(label = sub(".* [!][!]", "", label), 
             label = gsub("[:]", "", label))
  }
  
  tDat %>% 
    mutate(age = case_when(grepl("Under", label) ~ "0",
                           label %in% c("Total", "Total!!Male", "Total!!Female") ~ "Total", 
                           TRUE ~ gsub(".*ale[!][!](.+) year.*", "\\1", label)), 
           age = case_when(age %in% c("100 to 104", "105 to 109", "110") ~ "100+",
                           TRUE ~ age)) %>% 
    select(tableID, name, sex, age, raceCode)
  
  
}

## Create Census Linkages --------------------------------------------------------
census2000_link <- createCensusLink(censusVars2000_race)
census2010_link <- createCensusLink(censusVars2010_race) 
census2020_link <- createCensusLink(censusVars2020_race) 


## Function to pull decennial data ----------------------------------------------
pullDecennial <- function(myYear, myGeography, mySumFile = "sf1") {
  
  if (myYear == 2000) {
    censusLink <- census2000_link
  } else if (myYear == 2010) {
    censusLink <- census2010_link
  } else if (myYear == 2020) {
    censusLink <- census2020_link
  }
  
  tableIDs <- unique(censusLink$tableID)
  
  lapply(tableIDs, function(x, myYear1 = myYear) {
    
    print(myYear1)
    
    tDat <- get_decennial(geography = myGeography, table = x, year = myYear1, state = 06, sumfile = mySumFile) %>% 
      left_join(select(censusLink, -tableID), by = c("variable" = "name")) %>% 
      mutate(year = myYear1) %>% 
      select(year, GEOID, NAME, sex, age, raceCode, population = value)
    
    
  }) %>% 
    bind_rows()
  
  
}


## Census 2000, 2010, 2020 ------------------------------
census2000_ars_city <- pullDecennial(myYear = 2000, myGeography = "place")
census2000_ars_county <- pullDecennial(myYear = 2000, myGeography = "county")

census2010_ars_city <- pullDecennial(myYear = 2010, myGeography = "place")
census2010_ars_county <- pullDecennial(myYear = 2010, myGeography = "county")

census2020_ars_city <- pullDecennial(myYear = 2020, myGeography = "place", mySumFile = "dhc")
census2020_ars_county <- pullDecennial(myYear = 2020, myGeography = "county", mySumFile = "dhc")


# Redistribute 'Other' Race ----------------------------------------------------------------------------
censusCity <- bind_rows(census2000_ars_city, census2010_ars_city, census2020_ars_city) %>% 
  group_by(year, GEOID, NAME, sex, age, raceCode) %>% 
  summarise(population = sum(population)) %>% 
  ungroup()

cityOther <- censusCity %>% 
  filter(raceCode == "Other") %>% 
  rename(raceOther = population) %>% 
  select(-raceCode)

cityRace <- censusCity %>% 
  filter(raceCode != "Other") %>% 
  group_by(year, GEOID, NAME, sex, age) %>% 
  mutate(raceTotal = sum(population)) %>% 
  ungroup() %>% 
  mutate(raceProp = population / raceTotal) %>% 
  left_join(cityOther) %>% 
  mutate(populationNew = population + (raceOther * raceProp))

censusFinal_city <- cityRace %>% 
  select(year, GEOID, NAME, sex, age, raceCode, population = populationNew) %>% 
  mutate(population = ifelse(is.nan(population), 0, population))



censusCounty <- bind_rows(census2000_ars_county, census2010_ars_county, census2020_ars_county) %>% 
  group_by(year, GEOID, NAME, sex, age, raceCode) %>% 
  summarise(population = sum(population)) %>% 
  ungroup()

countyOther <- censusCounty %>% 
  filter(raceCode == "Other") %>% 
  rename(raceOther = population) %>% 
  select(-raceCode)

countyRace <- censusCounty %>% 
  filter(raceCode != "Other") %>% 
  group_by(year, GEOID, NAME, sex, age) %>% 
  mutate(raceTotal = sum(population)) %>% 
  ungroup() %>% 
  mutate(raceProp = population / raceTotal) %>% 
  left_join(countyOther) %>% 
  mutate(populationNew = population + (raceOther * raceProp))

censusFinal_county <- countyRace %>% 
  select(year, GEOID, NAME, sex, age, raceCode, population = populationNew) %>% 
  mutate(population = ifelse(is.nan(population), 0, population))


# SAVE DATA ===================================================================================================================

saveRDS(censusFinal_city, paste0(ccbUpstream, "upData/decennial-city-ars.RDS"))
saveRDS(censusFinal_county, paste0(ccbUpstream, "upData/decennial-county-ars.RDS"))
