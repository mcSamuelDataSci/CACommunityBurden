# Documentation ===================================================================

# Global constants ================================================================
years <- 2019:2022

# Load standards ==================================================================
server <- F
if (!server) source("g:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
if (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")

countyLink <- read_excel(paste0(standardsPlace, "countyLink.xlsx")) %>% 
  select(countyName)

# Set paths =======================================================================
.sl <- securePlace

# Load packages ===================================================================
library(stringr)
library(readxl)
library(dplyr)
library(readr)
library(fuzzyjoin)
library(purrr)
library(sqldf)


# Load death data =====================================================================
cbdDat0 <- readRDS(paste0(.sl, "myData/ccb_processed_deaths.RDS"))

cbdDat1 <- cbdDat0 %>% 
  filter(year %in% years) %>% 
  select(SFN, year, ICD10, county, city_lhj)

# Read in raw data =================================================================

rawDeathData_list <- lapply(years, function(year) {
  
  fileName <- ifelse(year %in% 2005:2017, 
                     paste0(.sl, "rawDeathData/Samuel_", year, ".csv"), 
                     paste0(.sl, "rawDeathData/Samuel_CCDF_", year, ".csv"))
  
  read.csv(fileName, colClasses = "character") %>% filter(F24 == as.character(year))
  
})

death.datA  <- bind_rows(rawDeathData_list) %>% 
  select(SFN = F1, 
         F221:F240, immediate_cod = F135, a_due_to_b = F137, b_due_to_c = F139, c_due_to_d = F141)

# Join data ============================================================================================

death.datB <- cbdDat1 %>% 
  left_join(death.datA, by = "SFN")

dataCA <- death.datB %>% 
  mutate(county = "CALIFORNIA")

cities <- c("Berkeley", "Alameda HD", "Long Beach", "Pasadena", "Los Angeles HD")
dataCities <- death.datB %>% 
  filter(city_lhj %in% cities) %>% 
  mutate(county = city_lhj)

# Map ICD Codes to Cause Codes

gbdMap0       <- as.data.frame(read_excel(paste0(ccbInfo,"icd10_to_CAUSE.xlsx"), sheet="main"))   
mapICD    <- gbdMap0[!is.na(gbdMap0$CODE),c("CODE","regEx10")]



icdCols <- c("ICD10", paste("F", 222:240, sep = ""))

icdMapped <- lapply(icdCols, function(colID) {
  
  if (colID != "ICD10") tColName <- paste0("causeCode_", substr(colID, 2, 4))
  
  myColName <- ifelse(colID == "ICD10", "causeCode_primary", tColName)
  
  tDat <- death.datB %>%
    rename(tICD10 = !!as.symbol(colID)) %>%
    regex_left_join(mapICD, by = c("tICD10" = "regEx10")) 
  
  if (colID == "ICD10") {
    tDat <- tDat %>%
      mutate(CODE = ifelse(tICD10 %in% c("","000","0000"), "cZ02", CODE),
             CODE = ifelse(is.na(CODE), "cZ03", CODE))
  }
  
  tDat %>%
    mutate(causeCode = substr(CODE, 2, 4)) %>%
    rename(!!as.symbol(myColName) := causeCode) %>%
    select( {{ myColName }} )
  
})

names(icdMapped) <- icdCols

# Data Validation Check 
all(sapply(icdMapped, function(x) nrow(x) == nrow(death.datB)))

# Final dataset ----------------------------------

death.datC <- death.datB %>%
  select(year, county, city_lhj) %>%
  bind_cols(icdMapped)

dataCA <- death.datC %>% 
  mutate(county = "CALIFORNIA")

dataCities <- death.datC %>% 
  filter(city_lhj %in% cities) %>% 
  mutate(county = city_lhj)

dataFinal <- death.datC %>% 
  filter(county %in% c(countyLink$countyName, NA)) %>% 
  bind_rows(dataCA) %>% 
  bind_rows(dataCities) %>% 
  filter(!is.na(year), !is.na(county)) %>% 
  mutate(across(starts_with("causeCode_2"), ~ifelse(.x == causeCode_primary, NA, .x))) %>% 
  select(-city_lhj)

# THIS FILTERS ON RECORDS WHERE AT LEAST ONE CONTRIBUTORY CCB CAUSE CODE MATCHES THE PRIMARY CAUSE CODE.
# THIS HAS BEEN ADDRESSED IN THE CODE RIGHT ABOVE, SO THIS RETURN AN EMPTY (MEANING 0 ROWS) DATAFRAME
dataFinal %>%
  filter(county == "CALIFORNIA", year == 2021) %>% 
  filter(if_any(.cols = starts_with("causeCode_2"), .fns = ~. == causeCode_primary))


# Additional data checks
table(dataFinal$year, useNA = "ifany")
table(dataFinal$county, useNA = "ifany")
length(unique(dataFinal$county))
length(unique(dataFinal$causeCode_primary))
sort(table(dataFinal$causeCode_primary, useNA = "ifany")) / 4

# Save dataset -----------------------------------------------

saveRDS(dataFinal, paste0(securePlace, "myData/mcod_processed_deaths.RDS"))
