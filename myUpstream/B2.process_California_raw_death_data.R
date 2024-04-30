# Standards and Global Constants =================================================================

yearYTD <- 2024
yearAnnual <- 2023

server <- F
if (!server) source("g:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
if (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
.sl <- securePlace

#-- Read xlsx file that indicates the variable names (2005-current) or column 
#--  location (2000-2004), and a common naming convention,
#--  for the death data variables used in the CBD
raw.death.variable.info <- as.data.frame(read_excel(
  paste0(ccbUpstream,"/upstreamInfo/death.File.Vars.xlsx"), 
  sheet="variableNames")
) %>%
  filter(varName != "MDCP")

# Process Data ==============================================================================================================

## Compare YTD to Annual Data -----------------------------------------------------
ytd <- read.csv(paste0(.sl,"rawDeathData/Samuel_CCDF_010123_123123.csv"), colClasses = "character") 

# On most recent quarterly data, filter on prior year
ytd_annual <- ytd %>%
  filter(F24 == as.character(yearAnnual)) %>% 
  arrange(F1)

# Read recent year
annual <- read.csv(paste0(.sl,"rawDeathData/Samuel_CCDF_2022.csv"), colClasses = "character") %>% filter(F24 == as.character(yearAnnual))

# Filter on same records
annual_subset <- annual %>% 
  right_join(select(ytd_annual, F1), by = "F1") %>% 
  arrange(F1)

if (identical(annual_subset, ytd_annual)) {
  print("Records are identical")
} else {
  print("Not identical. Need to investigate")
}

# Process 2005-Recent Year of Data ==============================================================================================
# 2005-2013: Only includes in-state deaths
# 2014-recent year: Includes in-state and out-of-state deaths

addOOS <- function(myYear, exportDate = TRUE, exportDynamic = "F241", exportMaster = "F251") {
  
  dynamicFileName <- ifelse(myYear %in% 2005:2017, 
                            paste0("Samuel_", myYear, ".csv"), 
                            paste0("Samuel_CCDF_", myYear, ".csv")
                            )
  
  dynamic <- read.csv(paste0(.sl,"rawDeathData/", dynamicFileName), colClasses = "character") %>% filter(F24 == as.character(myYear))
  master <- read.csv(paste0(.sl,"rawDeathData/Samuel_CCMDF_", myYear, ".csv"), colClasses = "character") %>% filter(F24 == as.character(myYear))
  
  if (exportDate) {
    
    expDate_dynamic <- dynamic %>% 
      distinct(!!as.symbol(exportDynamic)) %>% 
      pull()
    expDate_dynamic <- as.Date(expDate_dynamic, format = "%m/%d/%Y")
    
    expDate_master <- master %>% 
      filter(substr(F1, 2, 3) == "05") %>% 
      distinct(!!as.symbol(exportMaster)) %>% 
      pull()
    expDate_master <- as.Date(expDate_master, format = "%m/%d/%Y")
    
    
    compExportDate <- expDate_dynamic >= expDate_master
    
    if (compExportDate) {
      print(paste0("For ", myYear, " data, the dynamic data was exported on the same date or more recently than the master data. No need to investigate."))
      
      oos <- master %>% 
        filter(substr(F1, 2, 3) != "05") %>% 
        mutate(oos = TRUE)
      
      ca <- dynamic %>% 
        mutate(oos = FALSE) %>%
        bind_rows(oos)
      
    } else {
      print(paste0("For ", myYear, " data, the dynamic data was exported prior to the master data. Need to investigate."))
    }
    
  } else {
    print(paste0("For ", myYear, " data, there is/are no export date(s) to compare. No need to investigate."))
    oos <- master %>% 
      filter(substr(F1, 2, 3) != "05") %>% 
      mutate(oos = TRUE)
    
    ca <- dynamic %>% 
      mutate(oos = FALSE) %>% 
      bind_rows(oos)
  }
  
  return(ca)
  
}

ca23 <- read.csv(paste0(.sl,"rawDeathData/Samuel_CCDF_2023.csv"), colClasses = "character") %>% filter(F24 == "2023") %>% 
  mutate(oos = FALSE)
ca22 <- addOOS(2022, TRUE, "F241", "F251")
ca21 <- addOOS(2021, TRUE, "F241", "F247")
ca20 <- addOOS(2020, TRUE, "F241", "F245")
ca19 <- addOOS(2019, TRUE, "F241", "F241")
ca18 <- addOOS(2018, TRUE, "F241", "F241")

ca_14_17 <- lapply(2014:2017, function(x) {
  addOOS(x, FALSE)
}) %>% 
  bind_rows()

dynamic_05_13 <- lapply(2005:2013, function(x) {
  read.csv(paste0(.sl,"rawDeathData/Samuel_", x, ".csv"), colClasses = "character") %>% filter(F24 == as.character(x)) %>% 
    mutate(oos = FALSE)
}) %>% 
  bind_rows()

death.datA <- bind_rows(ca23, ca22, ca21, ca20, ca19, ca18, ca_14_17, dynamic_05_13)

# Select and rename columns
vInfo             <- filter(raw.death.variable.info, CCDF == 1)  # CCDF 2005-current variable names 
death.datA        <- death.datA[c(vInfo$seqID1, "oos")]   # select only needed columns of 2005-2015 data!
names(death.datA) <- c(vInfo$varName, "oos")           # name columns based on varName!

# Further processing
#Notes
# AGE:
# HARMONISE with CHSI: OKAY
# Note: not using F13-F16, age in months, days, hours, minutes 
#  age in years seems to be appropriately coded (mostly 0) based on these
# CONSIDER shifting to calculated age (F17 and F18); less data entry error but
#  subject to problem of missing DOB or DOD
# age field padded with leading "0"

# RACE/ETHNICITY:
# HARMONISE with CHSI: OKAY 
# TODO: "other" race does not have correspodning denominator-redistribute?
# if one race code is classifiable and another is not, multiraceStatus is 
#   **NOT** "multirace"


rCode <- c(1:7,9,8)
vLab  <- c("White-NH","Black-NH","AIAN-NH","Asian-NH","NHPI-NH","Other-NH","Multi-NH","Unk-NH","Hisp")

death.datA <- death.datA %>% 
  mutate(across(c("year", "multiraceStatus", "education", "age"), ~as.numeric(.x)),
         age = ifelse(age %in% 0:120, age, NA), # non-numeric values # consider fixes
         raceCode    = ifelse(hispanicOrigin == "Y","Hisp",vLab[match(multiraceStatus,rCode)]),
         hispanicOrigin  = NULL,
         multiraceStatus = NULL,
         raceCode = ifelse(is.na(raceCode), "-missing", raceCode)
         )


# State and county methodology updated based on CHSI's recommendations

# New method: The VSB Assessment and Policy Section (VSB-APS) is considering transitioning to the following new methodology:
# CCB Team: Using this method as of 09/28/2022

# 1. For records assigned a Census Tract from a geocoded residence address (F192 is not blank), F192 will be used for both 
# residence state and county. A California resident will have “06” for F192 positions 1-2 and F192 positions 3-5 will be used for the county FIPS code.

# 2. For records where F192 is blank, California residents will be identified as records where F71 is CA.
#   a. For records where F62 is not blank, F62 will be used.
#   b. For records where F62 is blank, F61 will be used (out-of-state records).

fipsCounty <- as.data.frame(read_excel(paste0(standardsPlace,"countyLink.xlsx")))

# Step 1
death.datA_1 <- death.datA %>%
  filter(!is.na(GEOID), GEOID != "") %>%
  mutate(stateFIPS = substr(GEOID, 1, 2), 
         tempCountyFIPS = substr(GEOID, 3, 5)) %>%
  filter(stateFIPS == "06") %>%
  left_join(select(fipsCounty, countyName, FIPSCounty), by = c("tempCountyFIPS" = "FIPSCounty")) %>%
  rename(county = countyName) %>%
  select(-tempCountyFIPS)

count(death.datA_1, county, oos)

# Step 2 - County FIPS
death.datA_2 <- death.datA %>%
  filter(GEOID == "" | is.na(GEOID), state == "CA", countyFIPS != "") %>%
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

# Step 3 - County FIPS Backup (OOS Deaths)
death.datA_3 <- death.datA %>%
  filter(GEOID == "" | is.na(GEOID), state == "CA", countyFIPS == "") %>%
  mutate(stateFIPS = "06") %>%
  left_join(select(fipsCounty, countyName, FIPSCounty), by = c("countyFIPS_backup" = "FIPSCounty")) %>%
  rename(county = countyName)

count(death.datA_3, county, countyFIPS, countyFIPS_backup, oos)


death.datA <- bind_rows(death.datA_1, death.datA_2, death.datA_3)

# Alpine check
death.datA %>%
  filter(county == "Alpine") %>%
  count(county)

# Age in OOS using Age Type and Age Unit fields
oos <- death.datA %>% 
  filter(oos) %>% 
  mutate(ageUnit2 = as.numeric(ageUnit), 
         age = case_when(ageType %in% c("2", "4", "5", "6") ~ 0, 
                         ageType == "1" ~ ageUnit2, 
                         ageType == "9" ~ NA_integer_)) %>% 
  select(-ageUnit2)

death.datA <- death.datA %>% 
  filter(!oos) %>% 
  bind_rows(oos)


# Process 2000 - 2004 files ==========================================================================================================================

# note: 2000-2004 files are "flat" ASCII files not .csv so need to be processed differently
vInfo  <- filter(raw.death.variable.info, DSMF_00_04 == 1) # 2000-2004 variable column locations
vInfo  <- vInfo[order(vInfo$mStart),]   # columns need to be read in order with read_fwf function !!  
  
vInfo2 <- vInfo
vInfo2[vInfo$varName=="education","mEnd"] <- 202 # for 2000-2002 education is 2 column character
# year of education

.f0 <- paste0(.sl,"rawDeathData/Death2000.txt")
.f1 <- paste0(.sl,"rawDeathData/Death2001.txt")
.f2 <- paste0(.sl,"rawDeathData/Death2002.txt")
.f3 <- paste0(.sl,"rawDeathData/Death2003.txt")
.f4 <- paste0(.sl,"rawDeathData/Death2004.txt")

# reading from flat files based on start and end positions and names as defined in vInfo  
ca00 <- read_fwf(file=.f0,col_positions=fwf_positions(start=vInfo2$mStart, end=vInfo2$mEnd, col_names = vInfo$varName),col_types="cccccccccccc",skip=0)
ca01 <- read_fwf(file=.f1,col_positions=fwf_positions(start=vInfo2$mStart, end=vInfo2$mEnd, col_names = vInfo$varName),col_types="cccccccccccc",skip=0)
ca02 <- read_fwf(file=.f2,col_positions=fwf_positions(start=vInfo2$mStart, end=vInfo2$mEnd, col_names = vInfo$varName),col_types="cccccccccccc",skip=0)
ca03 <- read_fwf(file=.f3,col_positions=fwf_positions(start=vInfo$mStart,  end=vInfo$mEnd,  col_names = vInfo$varName),col_types="cccccccccccc",skip=0)
ca04 <- read_fwf(file=.f4,col_positions=fwf_positions(start=vInfo$mStart,  end=vInfo$mEnd,  col_names = vInfo$varName),col_types="cccccccccccc",skip=0)

death.datB <- rbind(ca00,ca01,ca02,ca03,ca04)   %>%
  mutate(education=ifelse(year %in% 2000:2002,NA,education))  #remove 2001-2002 education data for now

death.datB$year               <- as.numeric(death.datB$year)
death.datB$education          <- as.numeric(death.datB$education)


# AGE -----
# HARMONISE with CHSI: OKAY (except for possible tiny issue related to ageUnit=9, which here assumes age is in years)
death.datB$age                              <- as.numeric(death.datB$age)              # some non-numeric values become NA
death.datB$age[death.datB$ageUnit==0]          <- death.datB$age[death.datB$ageUnit==0]+100  # ageUnit = 0 are > 99 years of age 
death.datB$age[death.datB$ageUnit %in% c(2:5)] <- 0                                    # ageUnit = 2-5 are < 1 (months, days, hours, minutes) 
# ageUnit = 9 is unknown

# STATE -----
# HARMONISE with CHSI: OKAY
# "State" based on stateCode=05 only (98.7%)
death.datB           <- subset(death.datB, stateCode=="05") # restricts data to CA state of residence code
death.datB$stateFIPS <-"06"                              # create stateFIPS variable with standard 06 CA code
death.datB$stateCode <- NULL                             # remove stateCode variable

# COUNTY -----
# note: same process as 2005-2015, but using diffent code standard from death files, so different mapping column
death.datB$county     <- fipsCounty$countyName[match(death.datB$countyCode,fipsCounty$cdphcaCountyTxt)]  
death.datB$countyCode <- NULL

# SEX -----
death.datB$sex[death.datB$sex==1] <- "M"  # could use mutate here and elsewhere, but this whole file uses basic R indexing approach
death.datB$sex[death.datB$sex==2] <- "F"

# GEOID ----
# no census tract data currently used for 2000-2004 data
# 4 character tractCode variable is availabe, 
# but unclear if this can reliably be combined with county and state to generate geoid??
# generated by counties, but not by counties
death.datB$tractCode        <- NULL   # for now remove this variable


# RACE/ETHNICITY -----
# CHIS harmonize: OKAY
# multiraceStatus does not include Hispanics one way or other in these years
# if one race code is classifiable and another is not, multiraceStatus **IS** "multirace"

death.datB$raceCode                                                <-"-missing"

death.datB$raceCode[death.datB$multiraceStatus==1]                 <-"White-NH"
death.datB$raceCode[death.datB$multiraceStatus==2]                 <-"Black-NH"
death.datB$raceCode[death.datB$multiraceStatus==3]                 <-"AIAN-NH"
death.datB$raceCode[death.datB$multiraceStatus==4]                 <-"Asian-NH"
death.datB$raceCode[death.datB$multiraceStatus==5]                 <-"NHPI-NH"
death.datB$raceCode[death.datB$multiraceStatus==6]                 <-"Other-NH"
death.datB$raceCode[death.datB$multiraceStatus==7]                 <-"Multi-NH"
death.datB$raceCode[death.datB$multiraceStatus==9]                 <-"Unk-NH"

death.datB$raceCode[death.datB$hispanicOrigin %in% c(2,3,4,5,6,8)] <-"Hisp"

death.datB$hispanicOrigin   <- NULL 
death.datB$multiraceStatus  <- NULL

death.datB <- death.datB %>% 
  mutate(oos = NA)

# Process DMSF 2005-2013 =================================================================================================================================================

dsmfCols <- raw.death.variable.info %>% 
  filter(y2005to2013 == 1) %>% 
  pull(varName)

getValues <- function(myData) {
  
  for (dsmfCol in dsmfCols) {
    tDat <- raw.death.variable.info %>% 
      filter(varName == dsmfCol)
    
    mStart <- tDat %>% pull(mStart2)
    mEnd <- tDat %>% pull(mEnd2)
    
    myData <- myData %>% 
      mutate(!!as.symbol(dsmfCol) := substr(V1, mStart, mEnd))
  }
  
  myData <- myData %>% 
    select(-V1) %>% 
    filter(stateCode == "0", stateCode_placeOfDeath != "0") 
  # State of Decedent's Residence; 0 = CA
  # State of Decedent's Place of Death; 0 = CA
  
  return(myData)
}


### 2013 ------------------------------
tDat <- readLines(paste0(.sl,"rawDeathData/DSMF_2005_2013/Deaths_2013.dat"), n = 300000)
tDat2 <- data.frame(V1 = tDat, stringsAsFactors = F)
m13 <- getValues(tDat2)

### 2012 ------------------------------
tDat <- readLines(paste0(.sl,"rawDeathData/DSMF_2005_2013/Deaths_2012.dat"), n = 300000)
tDat2 <- data.frame(V1 = tDat, stringsAsFactors = F)
m12 <- getValues(tDat2)

### 2011 ------------------------------
tDat <- readLines(paste0(.sl,"rawDeathData/DSMF_2005_2013/Deaths_2011.dat"), n = 300000)
tDat2 <- data.frame(V1 = tDat, stringsAsFactors = F)
m11 <- getValues(tDat2)

### 2010 ------------------------------
tDat <- readLines(paste0(.sl,"rawDeathData/DSMF_2005_2013/Deaths_2010.dat"), n = 300000)

tDat[185432]
tDat[185432] <- "185472                                                   201010161  20101016517                                                    031                     034  P011 274     21020     59444      2010  100                                      795670                                       10108 MALAGA WAY #2                              RANCHO CORDOVA                                                  "
tDat2 <- data.frame(V1 = tDat, stringsAsFactors = F)
m10 <- getValues(tDat2)

# Error in `mutate()`:
#   ℹ In argument: `streetName = substr(V1, mStart, mEnd)`.
# Caused by error in `substr()`:
#   ! invalid multibyte string at '<d7>#2 '
# Run `rlang::last_trace()` to see where the error occurred.

### 2009 ------------------------------
tDat <- readLines(paste0(.sl,"rawDeathData/DSMF_2005_2013/Deaths_2009.dat"), n = 300000)
tDat2 <- data.frame(V1 = tDat, stringsAsFactors = F)
m09 <- getValues(tDat2)

### 2008 ------------------------------
tDat <- readLines(paste0(.sl,"rawDeathData/DSMF_2005_2013/Deaths_2008.dat"), n = 300000)
tDat2 <- data.frame(V1 = tDat, stringsAsFactors = F)
m08 <- getValues(tDat2)

### 2007 ------------------------------
tDat <- readLines(paste0(.sl,"rawDeathData/DSMF_2005_2013/Deaths_2007.dat"), n = 300000)
tDat2 <- data.frame(V1 = tDat, stringsAsFactors = F)
m07 <- getValues(tDat2)

### 2006 ------------------------------
tDat <- readLines(paste0(.sl,"rawDeathData/DSMF_2005_2013/Deaths_2006.dat"), n = 300000)
tDat2 <- data.frame(V1 = tDat, stringsAsFactors = F)
m06 <- getValues(tDat2)

### 2005 ------------------------------
tDat <- readLines(paste0(.sl,"rawDeathData/DSMF_2005_2013/Deaths_2005.dat"), n = 300000)
tDat2 <- data.frame(V1 = tDat, stringsAsFactors = F)
m05 <- getValues(tDat2)

death.datC <- bind_rows(m13, m12, m11, m10, m09, m08, m07, m06, m05) %>% 
  mutate(oos = TRUE, 
         sex = ifelse(sex == 1, "M", "F"), 
         across(c("year", "education", "age"), ~as.numeric(.x)), 
         age = case_when(ageUnit == "0" ~ age + 100, # HARMONISE with CHSI: OKAY (except for possible tiny issue related to ageUnit=9, which here assumes age is in years)
                         ageUnit %in% as.character(2:5) ~ 0, 
                         ageUnit == "9" ~ NA_integer_, 
                         TRUE ~ age), 
         stateFIPS = "06", 
         hispanicGroup = case_when(hispanicOrigin == "1" ~ "N", 
                                   hispanicOrigin == "2" ~ "M", 
                                   hispanicOrigin == "3" ~ "P", 
                                   hispanicOrigin == "4" ~ "C", 
                                   hispanicOrigin == "5" ~ "Central/South American", 
                                   hispanicOrigin == "6" ~ "Other Hispanic (Born Outside the U.S.)", 
                                   hispanicOrigin == "8" ~ "Other Hispanic (Born in the U.S.)", 
                                   hispanicOrigin == "9" ~ "U")) %>% 
  left_join(select(fipsCounty, county = countyName, cdphcaCountyTxt), by = c("countyCode" = "cdphcaCountyTxt")) 

# RACE/ETHNICITY -----
# CHIS harmonize: OKAY
# multiraceStatus does not include Hispanics one way or other in these years
# if one race code is classifiable and another is not, multiraceStatus **IS** "multirace"

death.datC$raceCode                                                <-"-missing"

death.datC$raceCode[death.datC$multiraceStatus==1]                 <-"White-NH"
death.datC$raceCode[death.datC$multiraceStatus==2]                 <-"Black-NH"
death.datC$raceCode[death.datC$multiraceStatus==3]                 <-"AIAN-NH"
death.datC$raceCode[death.datC$multiraceStatus==4]                 <-"Asian-NH"
death.datC$raceCode[death.datC$multiraceStatus==5]                 <-"NHPI-NH"
death.datC$raceCode[death.datC$multiraceStatus==6]                 <-"Other-NH"
death.datC$raceCode[death.datC$multiraceStatus==7]                 <-"Multi-NH"
death.datC$raceCode[death.datC$multiraceStatus==9]                 <-"Unk-NH"

death.datC$raceCode[death.datC$hispanicOrigin %in% c(2,3,4,5,6,8)] <-"Hisp"

death.datC <- death.datC %>% 
  select(SFN, year, month, streetName, cityText, cityCode, zip, ageUnit, age, sex, race1, race2, race3, hispanicGroup, raceCode, education, ICD10, oos, stateFIPS, county)


# Combine all data ===========================================================================================================================================

cbdDat0FULL  <- bind_rows(death.datA,death.datB, death.datC) %>% 
    arrange(desc(year), desc(month))# "When row-binding using bind_rows, columns are matched by name,
  #   and any values that don't match will be filled with NA."

cbdDat0FULL <- cbdDat0FULL %>%
  rename(CHSI = raceCode)

# Check for duplicate SFNs =========================================================================================

dupSFNs <- cbdDat0FULL %>% 
  count(SFN) %>% 
  filter(n > 1) %>%
  distinct(SFN) %>% 
  pull()

# Investigate SFNs ============
if (F) {
  cbdDat0FULL %>% 
    filter(SFN == "3112021081573") %>% 
    View()
  
  dedup <- cbdDat0FULL %>% 
    filter(SFN == "3112021081573", CHSI == "Multi-NH")
  
  # One duplicate SFN. Choose one
  cbdDat0FULL <- cbdDat0FULL %>%
    filter(is.na(SFN)|SFN != "3112021081573") %>% 
    bind_rows(dedup) %>% 
    arrange(desc(year), desc(month))
}

saveRDS(cbdDat0FULL, file= paste0(.sl,"/myData/ccb_processed_deaths.RDS"))   # ccb_processed_deaths.RDS

