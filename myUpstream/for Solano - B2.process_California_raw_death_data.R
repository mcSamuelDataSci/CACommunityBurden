# =============================================================================
# process_raw_death_data.R
#  Inputs:
#   - multiple raw California death data files
#   - key variable mapping file             
#                                                                                 
#  Outputs:
#   - cbdDat0FULL: cleaned (but not aggreated) death data file   
#   - cbdDat0SAMP: identical structure to FULL, but a distorted sample
#
#  What is does:
#   - data merging, cleaning, and other processing  
#
#  Michael Samuel
#  2017 and 2018, 2021
# =============================================================================

#-- Set Locations Etc-----------------------------------------------------------------------


# PROVIDE PATHS AND NAMES FOR FILES HERE

secure.location   <-  "g:/FusionData/0.Secure.Data/"  
localFileName     <-  "YOUR_DEATH_DATA_FILE_NAME_HERE.csv"
other.location    <-  "g:/other place here/"  


#-- Load Packages -------------------------------------------------------------

library(stringr)
library(readxl)
library(dplyr)
library(readr)

#-- Read xlsx file that indicates the variable names (2005-current) or column 
#--  location (2000-2004), and a common naming convention,
#--  for the death data variables used in the CBD

raw.death.variable.info <- as.data.frame(read_excel(
                             paste0(other.location,"/death.File.Vars.xlsx"), 
                             sheet="variableNames")
                             ) %>%
                           filter(!varName %in% c("placeOfDeath", "MDCP"))


# === Process VRBIS file ==============================================================


death.datA   <- read.csv(paste0(secure.location,localFileName), colClasses = "character")
# Raw County VRBIS download files have no header row or local header row
# This line assures column names are "F1", "F2"...."F192" consistant with
# names used in "vInfo" file

names(death.datA) <- paste0("F",1:ncol(death.datA))

# FIPS code (F62) should be three character string with leading 0 (if needed)
# This line assures this format.
death.datA$F62  <- str_pad(death.datA$F62,3,pad="0")  


vInfo             <- filter(raw.death.variable.info, CCDF == 1)  # CCDF 2005-current variable names 
death.datA        <- death.datA[vInfo$seqID1]                    # select only needed columns of 2005-2015 data!
names(death.datA) <- vInfo$varName                               # name columns based on varName!



death.datA     <- death.datA %>%
                     mutate(year             = as.numeric(year),
                            multiraceStatus  = as.numeric(multiraceStatus),
                            education        = as.numeric(education),
                            age              = as.numeric(age)
                     )

# AGE -----
# Note: not using F13-F16, age in months, days, hours, minutes 
#  age in years seems to be appropriately coded (mostly 0) based on these
# CONSIDER shifting to calculated age (F17 and F18); less data entry error but
#  subject to problem of missing DOB or DOD
# age field padded with leading "0"
death.datA$age[!death.datA$age %in% 0:120] <- NA

# STATE -----
# select Califonia cases only
# "state" here based on "F71" only -- use additional variables?
#   10 blank, 4593 XX (not US or Canada), 6685 ZZ (unknown) 
death.datA <- subset(death.datA, state=="CA")   # NOTE: subset function excludes NAs 
death.datA$stateFIPS  <-"06" 


# COUNTY -----
# County name based strictly on (mapping to) FIPS code (F62)
# HARMONISE with CHSI: OKAY
fipsCounty          <- as.data.frame(read_excel(paste0(other.location,"/myInfo/County Codes to County Names Linkage.xlsx"))) 
death.datA$county   <- fipsCounty$countyName[match(death.datA$countyFIPS,fipsCounty$FIPSCounty)]        

# code no longer needed since FIPS code is read in as character, but valuable "snipit" for similar purposes:
# death.datA$countyFIPS  <- str_pad(death.datA$countyFIPS,3,pad="0")  

# -----------------------------------------------------------------------------------------------------------------------


# RACE/ETHNICITY -----
rCode <- c(1:7,9,8)
vLab  <- c("White-NH","Black-NH","AIAN-NH","Asian-NH","NHPI-NH","Other-NH","Multi-NH","Unk-NH","Hisp")

death.datA          <- mutate(death.datA,
                          raceCode        = ifelse(hispanicOrigin == "Y","Hisp",vLab[match(multiraceStatus,rCode)]),
                          hispanicOrigin  = NULL,
                          multiraceStatus = NULL) %>%
                       rename(CHSI = raceCode)    %>%
                       mutate(CHSI = ifelse(is.na(CHSI), "-missing", CHSI))


saveRDS(death.datA, file= paste0(secure.location,"/ccb_processed_deaths.RDS"))   # ccb_processed_deaths.RDS

