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
#  2017 and 2018
# =============================================================================

#-- Set Locations Etc----------------------------------------------------------

# MUST EDIT HERE to provide path for secure data
secure.location  <- "G:/CCB/0.Secure.Data"  # two possible locations for
secure.location  <- "H:/0.Secure.Data"      #   secure data


.sl              <- secure.location  # short name to shorten lines of code below

myDrive    <- "E:"  # ROOT Location of CBD Project

myPlace    <- paste0(myDrive,"/0.CBD/myCBD")
upPlace    <- paste0(myDrive,"/0.CBD/myUpstream")


#-- EDIT for Local versus State installation -------------------------------------------------

local.installation <- FALSE
state.installation <- TRUE

#-- Load Packages -------------------------------------------------------------

library(stringr)
library(readxl)
library(dplyr)
library(readr)

#-- Read xlsx file that indicates the variable names (2005-current) or column 
#--  location (2000-2004), and a common naming convention,
#--  for the death data variables used in the CBD

raw.death.variable.info <- as.data.frame(read_excel(
                             paste0(upPlace,"/upstreamInfo/death.File.Vars.xlsx"), 
                             sheet="variableNames")
                             )   

# === Process 2005 - 2015 files ==============================================================


if (state.installation) {

 ca17    <- read.csv(paste0(.sl,"/rawDeathData/Samuel_2017.csv"), colClasses = "character") 
 ca16    <- read.csv(paste0(.sl,"/rawDeathData/Samuel_2016.csv"), colClasses = "character") 
 ca15    <- read.csv(paste0(.sl,"/rawDeathData/Samuel_2015.csv"), colClasses = "character") 
 ca14    <- read.csv(paste0(.sl,"/rawDeathData/Samuel_2014.csv"), colClasses = "character") 
 ca13    <- read.csv(paste0(.sl,"/rawDeathData/Samuel_2013.csv"), colClasses = "character")
 ca12    <- read.csv(paste0(.sl,"/rawDeathData/Samuel_2012.csv"), colClasses = "character")
 ca11    <- read.csv(paste0(.sl,"/rawDeathData/Samuel_2011.csv"), colClasses = "character")
 ca10    <- read.csv(paste0(.sl,"/rawDeathData/Samuel_2010.csv"), colClasses = "character")
 ca09    <- read.csv(paste0(.sl,"/rawDeathData/Samuel_2009.csv"), colClasses = "character")
 ca08    <- read.csv(paste0(.sl,"/rawDeathData/Samuel_2008.csv"), colClasses = "character")
 ca07    <- read.csv(paste0(.sl,"/rawDeathData/Samuel_2007.csv"), colClasses = "character")
 ca06    <- read.csv(paste0(.sl,"/rawDeathData/Samuel_2006.csv"), colClasses = "character")
 ca05    <- read.csv(paste0(.sl,"/rawDeathData/Samuel_2005.csv"), colClasses = "character")
 
death.datA  <- bind_rows(ca17,ca16,ca15,ca14,ca13,ca12,ca11,ca10,ca09,ca08,ca07,ca06,ca05)

}


if (local.installation) {
death.datA   <- read.csv(paste0(.sl,"YOUR_DEATH_DATA_FILE_NAME_HERE.csv"), colClasses = "character")
}

# END LOCAL HEALTH JURISDICTION SECTION -------------------------------------------------------


vInfo             <- filter(raw.death.variable.info,y2005onward == 1)  # 2005-current variable names 
death.datA        <- death.datA[vInfo$seqID1]   # select only needed columns of 2005-2015 data!
names(death.datA) <- vInfo$varName           # name columns based on varName!

death.datA$year               <- as.numeric(death.datA$year)
death.datA$multiraceStatus    <- as.numeric(death.datA$multiraceStatus)

# AGE -----
# HARMONISE with CHSI: OKAY
# Note: not using F13-F16, age in months, days, hours, minutes 
#  age in years seems to be appropriately coded (mostly 0) based on these
# CONSIDER shifting to calculated age (F17 and F18); less data entry error but
#  subject to problem of missing DOB or DOD
# age field padded with leading "0"
death.datA$age  <- as.numeric(death.datA$age) # missing values generated from
                                                  #   non-numeric values # consider fixes
death.datA$age[!death.datA$age %in% 0:120] <- NA

# STATE -----
# Consider investigation/comparison with State based on gecoding
# HARMONIZE with CHSI: OKAY
# select Califonia cases only
# "state" here based on "F71" only -- use additional variables?
#   10 blank, 4593 XX (not US or Canada), 6685 ZZ (unknown) 
death.datA <- subset(death.datA, state=="CA")   # NOTE: subset function excludes NAs 
death.datA$stateFIPS  <-"06" 

# 2005-2013 & 2015 data DOES NOT include reallocates (deaths of California residents that occured out-of-state)
# 2014             data DOES     include reallocates
# what about 2001-2004?
# is this correct?
# can we fix it?

# COUNTY -----
# County name based strictly on (mapping to) FIPS code (F62)
# HARMONISE with CHSI: OKAY
fipsCounty          <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/County Codes to County Names Linkage.xlsx"))) 
death.datA$county      <- fipsCounty$countyName[match(death.datA$countyFIPS,fipsCounty$FIPSCounty)]        

# code no longer needed since FIPS code is read in as character, but valuable "snipit" for similar purposes:
# death.datA$countyFIPS  <- str_pad(death.datA$countyFIPS,3,pad="0")  

# -----------------------------------------------------------------------------------------------------------------------

# ******************
# ADD/FIX  IF 192 is missing and 73 is not missing, use that (and talk to Matt Beyers about this) 
# ******************

# several prior issues with the geoid variable (now using F192) have been resolved by CHSI, making code
#  below unnecessary, but keeping some for now becuase potentially useful for other/related purposes
#  F192 is now an 11 character string for between 95% and 97% for all years 2005-2015, and is blank otherwise -- good/clean

# geoid checking 1
# death.datA$geoLength <- nchar(death.datA$geoid,type="chars")
# write.csv(table(death.datA$year, death.datA$geoLength), file= paste0(upPlace,"/tempOutput/geoLengthExplore.csv"))

# geoid processing
# death.datA$geoid[death.datA$year %in% c(2013,2014,2015)] <- paste0("0",death.datA$geoid[death.datA$year %in% c(2013,2014,2015)])
# death.datA$geoid[death.datA$geoid == "0NA" | death.datA$geoid == "0" | death.datA$geoid == "" ]  <- NA
# death.datA$geoid[death.datA$geoLength != 11 | !(death.datA$year %in% 2011:2015) ]             <- NA
# death.datA$geoid <- str_pad(death.datA,side="left",pad="0")

# geoid checking 2
# death.datA$geoLength <- nchar(death.datA$geoid,type="chars")
# write.csv(table(death.datA$year, death.datA$geoLength), file= paste0(upPlace,"/tempOutput/geoidWidth2.csv"))

# death.datA$geoLength  <- NULL    # drops geoLength "variable"
# -----------------------------------------------------------------------------

# RACE/ETHNICITY -----
# HARMONISE with CHSI: OKAY 
# TODO: "other" race does not have correspodning denominator-redistribute?
# if one race code is classifiable and another is not, multiraceStatus is 
#   **NOT** "multirace"

rCode <- c(1:7,9,8)
vLab  <- c("White-NH","Black-NH","AIAN-NH","Asian-NH","NHPI-NH","Other-NH","Multi-NH","Unk-NH","Hisp")

death.datA          <- mutate(death.datA,
                        raceCode    = ifelse(hispanicOrigin == "Y","Hisp",vLab[match(multiraceStatus,rCode)]),
                        hispanicOrigin  = NULL,
                        multiraceStatus = NULL)

death.datA$raceCode[is.na(death.datA$raceCode)] <-"-missing"

# === Process 2000 - 2014 files ==============================================================

# note: 2000-2004 files are "flat" ASCII files not .csv so need to be processed differently


if (state.installation) {


vInfo <- filter(raw.death.variable.info,y2000to2004 == 1) # 2000-2004 variable column locations
vInfo <- vInfo[order(vInfo$mStart),]   # columns need to be read in order with read_fwf function !!  

.f0 <- paste0(.sl,"/rawDeathData/Death2000.txt")
.f1 <- paste0(.sl,"/rawDeathData/Death2001.txt")
.f2 <- paste0(.sl,"/rawDeathData/Death2002.txt")
.f3 <- paste0(.sl,"/rawDeathData/Death2003.txt")
.f4 <- paste0(.sl,"/rawDeathData/Death2004.txt")

# reading from flat files based on start and end positions and names as defined in vInfo  
ca00 <- read_fwf(file=.f0,col_positions=fwf_positions(start=vInfo$mStart, end=vInfo$mEnd, col_names = vInfo$varName),skip=0)
ca01 <- read_fwf(file=.f1,col_positions=fwf_positions(start=vInfo$mStart, end=vInfo$mEnd, col_names = vInfo$varName),skip=0)
ca02 <- read_fwf(file=.f2,col_positions=fwf_positions(start=vInfo$mStart, end=vInfo$mEnd, col_names = vInfo$varName),skip=0)
ca03 <- read_fwf(file=.f3,col_positions=fwf_positions(start=vInfo$mStart, end=vInfo$mEnd, col_names = vInfo$varName),skip=0)
ca04 <- read_fwf(file=.f4,col_positions=fwf_positions(start=vInfo$mStart, end=vInfo$mEnd, col_names = vInfo$varName),skip=0)

death.datB <- rbind(ca00,ca01,ca02,ca03,ca04)

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

death.datB$raceCode                                             <-"-missing"

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

}


# === Combine 2001-2004 and 2005-2015 files ============================================================================

if (state.installation){
cbdDat0FULL  <- bind_rows(death.datA,death.datB)  # "When row-binding using bind_rows, columns are matched by name,
                                            #   and any values that don't match will be filled with NA."
}


if (local.installation) cbdDat0FULL <- death.datA


save(cbdDat0FULL, file= paste0(.sl,"/myData/cbdDat0FULL.R"))


# === Create Random Data Set =============================================================================================
  
# CAUTION
load(paste0(.sl,"/myData/cbdDat0FULL.R"))

#cbdDat0FULL$ageUnit  <- NULL
work <- cbdDat0FULL
work <- work[,c("year","state","county","zip","GEOID","countyFIPS","stateFIPS","age","sex","raceCode","ICD10")]

sampN1 <- 400000  
half1  <- sample_n(work,sampN1)  # sample function from dplyr

sampN2       <- 600000
p1           <- sample_n(work[,1:7],  sampN2)
p2           <- sample_n(work[,8:10], sampN2)
p3           <- sample_n(work[,10:11], sampN2)
p3$raceCode  <- NULL
half2        <- cbind(p1,p2,p3)

cbdDat0SAMP <- rbind(half1,half2)

save(cbdDat0SAMP, file= paste0(upPlace,"/upData/cbdDat0SAMP.R"))

