# 0 Script Documentation ------------------------------------------------------------------------------------------------------------------------------

# Authors: Michael Samuel, DrPH, Jaspreet Kang (Fusion Center/Office of Policy and Planning, CDPH)
# Contact information for any questions: ccb@cdph.ca.gov

# About this script:
# This script produces the necessary state, county, and MSSA-level population data which are then used in "ltmaker.R" to calculate life tables

# Script dependencies:
# R packages:
# - pacman
# - readxl
# - stringr
# - readstata13

# Inputs:
# FusionStandards.R - loads in standard paths and objects
# countyLink.xlsx - standard linkage file which links county names to county fips
# dof_pop_2000plus.RDS - DOF population data dating back to 2000
# populationExtract.R - standard custom function for processing population data into desired output
# nxMSSA.RDS - Last year's MSSA-level population data file

# Outputs:
# 1. nxCounty.RDS - County-level population data - Contains every year(2000-)-county-sex(including total)-race(including total)-ageGroup combination
# 2. nxState.RDS - State-level population data - Contains every year(2000-)-state-sex(including total)-race(including total)-ageGroup combination
# 3. nxMSSA.RDS - MSSA-level population data - Contains every year(2007-)-MSSA-sex-race(including total)-ageGroup combination

# Script structure:
# 1. Setup - Load required R packages, set global constants, source standard and geography linkage files
# 2. Read in and prepare state, county-level population data
# 3. Read in and prepare MSSA-level population data
# 4. Save data

# Notes:
# - State and county population data are from the California Department Department of Finance (DOF). 
#   - 2000-2009 data are California and Counties Population by Age, Race/Hispanics, and Gender: 2000-2010 - https://dof.ca.gov/forecasting/Demographics/estimates/
#   - 2010- data are Complete State and County Projections (Table P-3) - https://dof.ca.gov/forecasting/demographics/Projections/
# - MSSA population data are aggregations of Census Tract pop data pulled from ACS 5-year surveys, using Table B01001 - https://data.census.gov/cedsci/table?q=b01001&tid=ACSDT5Y2019.B01001
#   - Medical Service Study Areas (MSSAs) is a unique California geographic designation based on aggregation of census tracts, constructed by the California Health Care Access and Information (HCAI)
#   - There are 542 MSSAs in California
#   - 2010 Census Tract boundaries are used for all years; Therefore, 2019 ACS 5-Year estimates are used for 2020-



# 1 Setup -----------------------------------------------------------------------------------------------------------------------------------

## 1.1 Load packages -------------------------------------

# install.packages("pacman") # Uncomment line if pacman is not installed on your system
pacman::p_load("readxl", "stringr", "readstata13")


## 1.2 Set global constant - most recent year -------------

myYear <- 2021

## 1.3 Set paths, source standards, read in linkage file -------------

CCB         <- TRUE
server      <- TRUE
myDrive     <- getwd()
myPlace     <- paste0(myDrive,"/myCCB/") 

source(paste0(myPlace,"/Standards/FusionStandards.R")) # Loads in standard paths and objects

countyLink <- readxl::read_xlsx(paste0(standardsPlace, "countyLink.xlsx")) %>%
  select(countyName, FIPSCounty) %>%
  mutate(fips = as.numeric(paste0("6", FIPSCounty)))


# 2 Read in and process state, county population data ------------------------------------------------------

## 2.1 Read in 2000- DOF's county-level population data -----------------

dofData <- readRDS(paste0(fusionPlace, "Population Data/dof_pop_2000plus.RDS")) %>% # File created further upstream
  select(-county) # Data file has county fips, so county names are not needed here


## 2.2 Process state, county-level population by passing through standard custom populationExtract function ----------


# Add comments about the function (JASPO - CLEAN UP POP EXTRACT)
source(paste0(standardsPlace,"populationExtract.R"))

# - Grabbing county + California
# - Grabbing race (using raceCode naming standard) + Total
# - Grabbing sex + Total
# - Using life expectancy age groups which can be found in ageLink.xlsx + grabbing Total (which will be filtered out)
# - Grabbing years 2000-myYear
popCounty <- populationExtract(County = T, 
                               Race   = T,
                               Sex    = T, 
                               Age    = T,
                               Year      = 2000:myYear,
                               ageGroups = "ageLE", 
                               ageLabels,
                               raceLabel = "raceCode", 
                               CA    = T, 
                               Total = T,
                               multiYear = F, 
                               popData = dofData, 
                               server = server)

# Further processing
popCounty1 <- popCounty %>%
  ungroup() %>%
  filter(ageGroup != "Total") %>%
  left_join(countyLink, by = c("county" = "countyName")) %>%
  mutate(GEOID = ifelse(county == "CALIFORNIA", "06000000000", 
                        paste0("0", fips, "000000")), 
         agell = as.integer(sub("\\-.*", "", ageGroup)), 
         ageul = as.integer(sub(".*-", "", ageGroup))) %>%
  select(GEOID, sex, year, agell, ageul, Nx = population, raceCode)

## 2.3 Finalize county-level population data ------------
nxCounty <- popCounty1 %>%
  filter(GEOID != "06000000000")

# Data Quality check: Ensure all combinations exist

dqCheck <- c(table(nxCounty$year, nxCounty$sex, nxCounty$agell, nxCounty$raceCode) == 58)

print(paste0("Data Quality Check: There are population data for all 58 counties per year-sex-ageGroup-race combination: ", all(dqCheck)))

## 2.4 Finalize state-level population data ---------------
nxState <- popCounty1 %>%
  filter(GEOID == "06000000000")


# 3 Read in and process MSSA population data ------------------------------------------------------

# Notes about nxMSSA.RDS:
# 1. 2007-2017 MSSA Pop pulled by Ethan (0.CCB/myUpstream/lifeTables/dataIn/acs5_mssa.dta)
# 2. 2018-2019 pulled by CCB Data Team using the standard pullACS function
# 3. 2020- data are actually 2019 ACS 5Y estimates since we have not yet switched to the new 2020 census tract boundaries
# - Because of this, the code below simply reads in nxMSSA.RDS and uses the prior year's pop estimates (which are 2019 ACS 5Y estimates) for current year


nxMSSA <- readRDS(paste0(ccbUpstream, "lifeTables/dataIn/nxMSSA.RDS"))

# Uses last year's pop estimates for current year
t_nxMSSA <- nxMSSA %>%
  filter(year == (myYear - 1)) %>%
  mutate(year = myYear)

# Bind current year to nxMSSA
nxMSSA_final <- bind_rows(nxMSSA, t_nxMSSA)


# 4 Save data ------------------------------------------------------------------------------------

saveRDS(nxCounty1, paste0(ccbUpstream, "lifeTables/dataIn/nxCounty.RDS"))
saveRDS(nxState, paste0(ccbUpstream, "lifeTables/dataIn/nxState.RDS"))
saveRDS(nxMSSA_final, paste0(ccbUpstream, "lifeTables/dataIn/nxMSSA.RDS"))
