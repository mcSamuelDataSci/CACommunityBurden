# Documentation ===================================================================

# Global constants ================================================================
years <- 2005:2022

# Load standards ==================================================================
server <- T
if (!server) source("g:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
if (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")

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


# Load info files =====================================================================
raw.death.variable.info <- as.data.frame(read_excel(
  paste0(ccbUpstream,"/upstreamInfo/death.File.Vars.xlsx"), 
  sheet="variableNames")
)   

fipsCounty <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/County Codes to County Names Linkage.xlsx")))
ageMap     <- as.data.frame(read_excel(paste0(ccbInfo,"Age Group Standard and US Standard 2000 Population.xlsx"),sheet = "data"))
raceLink <- as.data.frame(read_excel(paste0(standardsPlace,"raceLink.xlsx")))  %>% select(raceCode,CHSI)
regionLink <- as.data.frame(readxl::read_xlsx(paste0(standardsPlace, "countyLink.xlsx"))) %>% select(county = countyName, region = FUSION)

# Read in Raw Death Data ==========================================================

rawDeathData_list <- lapply(years, function(year) {
  
  fileName <- ifelse(year %in% 2005:2017, 
                     paste0(.sl, "rawDeathData/Samuel_", year, ".csv"), 
                     paste0(.sl, "rawDeathData/Samuel_CCDF_", year, ".csv"))
  
  read.csv(fileName, colClasses = "character") %>% filter(F24 == as.character(year))
  
})

death.datA  <- bind_rows(rawDeathData_list)

# Select and rename needed columns =================================================
varNames <- filter(raw.death.variable.info, CCDF == 1, seqID1 != "F125")

death.datA <- death.datA %>%
  select(all_of(varNames$seqID1), F221:F240, immediate_cod = F135, a_due_to_b = F137, b_due_to_c = F139, c_due_to_d = F141)
names(death.datA)
varNames$seqID1

names(death.datA)[1:nrow(varNames)] <- varNames$varName
names(death.datA)

# Re-assign sex values; Change classes =============================================================
death.datA <- death.datA %>%
  mutate(sex = case_when(sex == "M" ~ "Male", 
                         sex == "F" ~ "Female",
                         TRUE ~ sex), 
         year = as.numeric(year), 
         multiraceStatus = as.numeric(multiraceStatus), 
         age = as.numeric(age))

# Cut age into age groups ===========================================================================
death.datA$age[!death.datA$age %in% 0:120] <- NA

aL            <-      ageMap$lAge     # lower age ranges
aU            <- c(-1,ageMap$uAge)    # upper age ranges, plus inital value of "-1" for lower limit
aLabs         <- ageMap$ageLabel 
aMark         <- findInterval(death.datA$age,aU,left.open = TRUE)  # vector indicating age RANGE value of each INDIVIDUAL age value
death.datA$ageGroup  <- aLabs[aMark]  

# Get race/ethnicity groups ========================================================================
rCode <- c(1:7,9,8)
vLab  <- c("White-NH","Black-NH","AIAN-NH","Asian-NH","NHPI-NH","Other-NH","Multi-NH","Unk-NH","Hisp")

death.datA          <- mutate(death.datA,
                              CHSI    = ifelse(hispanicOrigin == "Y","Hisp",vLab[match(multiraceStatus,rCode)]),
                              hispanicOrigin  = NULL,
                              multiraceStatus = NULL)

death.datA$CHSI[is.na(death.datA$CHSI)] <-"-missing"

death.datA <- death.datA %>% 
  left_join(raceLink, by = "CHSI") %>% 
  select(-CHSI)



# === Filter on CA Residents; Assign county of residence ==============================================================

# Step 1
death.datA_1 <- death.datA %>%
  filter(!is.na(GEOID), GEOID != "") %>%
  mutate(stateFIPS = substr(GEOID, 1, 2), 
         tempCountyFIPS = substr(GEOID, 3, 5)) %>%
  filter(stateFIPS == "06") %>%
  left_join(select(fipsCounty, countyName, FIPSCounty), by = c("tempCountyFIPS" = "FIPSCounty")) %>%
  rename(county = countyName) %>%
  select(-tempCountyFIPS)

# Step 2
death.datA_2 <- death.datA %>%
  filter(GEOID == "" | is.na(GEOID), state == "CA") %>%
  mutate(stateFIPS = "06") %>%
  left_join(select(fipsCounty, countyName, FIPSCounty), by = c("countyFIPS" = "FIPSCounty")) %>%
  rename(county = countyName)

# Data check - All NA counties should have a corresponding "999" (unknown) value
# Should be 59 rows (58 counties + 1 NA county)
count(death.datA_2, county, countyFIPS)

# Data check - Check NA counties. FIPS for both countyFIPS and backup column should be "999"
death.datA_2 %>%
  filter(is.na(county)) %>%
  count(countyFIPS, countyFIPS_backup) %>%
  rename(Ndeaths = n)

death.datA <- bind_rows(death.datA_1, death.datA_2)

# Alpine check
death.datA %>%
  filter(county == "Alpine") %>%
  count(county)


# Link county to region ============================================================================
death.datA <- left_join(death.datA, regionLink, by = "county")


# Select needed columns ===========================================================================
names(death.datA)

death.datB <- death.datA %>%
  select(SFN, year, county, region, sex, ageGroup, raceCode, ICD10, F222:F240, immediate_cod, a_due_to_b, b_due_to_c, c_due_to_d)

# For malnutrition analysis - intern work -------
if (FALSE) {
  tDat <- death.datB %>% select(-SFN)
  
  saveRDS(tDat, paste0(.sl, "myData/Malnutrition Analysis/processed_deaths.RDS"))
}

if (FALSE) {
  
  tDat <- death.datB %>% select(-SFN)
  
  tDat_CA <- tDat %>%
    mutate(county = "CALIFORNIA")
  
  tDat_totalSex <- tDat %>%
    mutate(sex = "Total")
  
  tDat_CA_totalSex <- tDat %>%
    mutate(county = "CALIFORNIA", sex = "Total")
  
  tDatFinal <- tDat %>%
    bind_rows(tDat_CA) %>%
    bind_rows(tDat_totalSex) %>%
    bind_rows(tDat_CA_totalSex) %>%
    filter(!is.na(year), !is.na(county), sex %in% c("Female", "Male", "Total"))
  
  saveRDS(tDatFinal, "data/mcod_processed_deaths_wICD10_codes.RDS")
  
}


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
  select(year, county, sex) %>%
  bind_cols(icdMapped)

death.datC_CA <- death.datC %>%
  mutate(county = "CALIFORNIA")

death.datC_totalSex <- death.datC %>%
  mutate(sex = "Total")

death.datC_CA_totalSex <- death.datC %>%
  mutate(county = "CALIFORNIA", sex = "Total")

death.datFinal <- death.datC %>%
  bind_rows(death.datC_CA) %>%
  bind_rows(death.datC_totalSex) %>%
  bind_rows(death.datC_CA_totalSex) %>%
  filter(!is.na(year), !is.na(county), sex %in% c("Female", "Male", "Total")) %>% 
  mutate(across(starts_with("causeCode_2"), ~ifelse(.x == causeCode_primary, NA, .x)))


# THIS FILTERS ON RECORDS WHERE AT LEAST ONE CONTRIBUTORY CCB CAUSE CODE MATCHES THE PRIMARY CAUSE CODE.
# THIS HAS BEEN ADDRESSED IN THE CODE RIGHT ABOVE, SO THIS RETURN AN EMPTY (MEANING 0 ROWS) DATAFRAME
death.datFinal %>%
  filter(county == "CALIFORNIA", sex == "Total", year == 2021) %>% 
  filter(if_any(.cols = starts_with("causeCode_2"), .fns = ~. == causeCode_primary))


# Additional data checks
table(death.datFinal$year, useNA = "ifany")
table(death.datFinal$county, useNA = "ifany")
length(unique(death.datFinal$county))
table(death.datFinal$sex, useNA = "ifany")
length(unique(death.datFinal$causeCode_primary))
sort(table(death.datFinal$causeCode_primary, useNA = "ifany")) / 4

# Save dataset -----------------------------------------------

saveRDS(death.datFinal, "data/mcod_processed_deaths.RDS")
