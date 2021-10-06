##### ----------------------------------------------------------------------------------------------------

# Creating population files (nxCounty & nxState) for life expectancy calculation

#### ----------------------------------------------------------------------------------------------------

library(readxl)
library(dplyr)
library(stringr)
library(readstata13)

# --- Set most recent year ------------------------------------------------------------------------------

myYear <- 2020

# --- Set necessary paths ---------------------------------------------------------------------------------

server <- T
if (server) {
  source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
} else {
  source("G:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
}

countyLink <- readxl::read_xlsx(paste0(standardsPlace, "countyLink.xlsx")) %>%
  select(countyName, FIPSCounty) %>%
  # mutate(GEOID = paste0("06", FIPSCounty, "000000"))
  mutate(fips = as.numeric(paste0("6", FIPSCounty)))

# raceLink <- read_xlsx(paste0(myStandards, "raceLink.xlsx")) 

# 1) Read in DCDC population file since it goes back to 2000. Cleaning up data frame to prepare it for populationExtract standard function.

tDat0 <- readRDS(paste0(fusionPlace, "Population Data/dof_pop_2000plus.RDS"))

# tDat0 <- readRDS(file= paste0(myPath, "Population Data/Archive DCDC-STD Data and Documentation/tDat_2000_2020.rds")) %>%
#   filter(!(COUNTY %in% c("California", "Alameda HD", "Berkeley", "Pasadena", "Long Beach", "Los Angeles HD"))) %>%  # NOTE: California totals already included with county = "California
#   rename(year=YEAR, county= COUNTY, sex = SEX, agerc = AGE, raceE = RE, perwt = POP) %>%
#   mutate(sex = c("MALE","FEMALE")[match(sex,c("M","F"))]) %>%
#   filter(year %in% 2000:2020) %>%
#   left_join(select(raceLink, race7, DCDC), by = c("raceE" = "DCDC")) %>% # JASPO EDIT - TAKE OUT, AND UNCOMMENT BELOW IF NEW RACECODES CAUSES MAJOR ISSUES
#   select(-raceE)
# 
# # 1a) FIPS is required in populationExtract standard function
# 
# countyLink <- readxl::read_xlsx(paste0(myPath, "Standards/countyLink.xlsx")) %>%
#   select(countyName, FIPSCounty) %>%
#   # mutate(GEOID = paste0("06", FIPSCounty, "000000"))
#   mutate(fips = as.numeric(paste0("6", FIPSCounty)))
# 
# tDat0  <- tDat0 %>%  left_join(countyLink, by = c("county" = "countyName"))


# -----------------------------------------------------------------------------
source(paste0(standardsPlace,"populationExtract.R"))

# For the function, we can't have county column since the function links fips code to county
tDat0 <- select(tDat0, -county)


# 2) Run tDat0 through populationExtract standard function:
# - Grabbing county + California
# - Grabbing race (using raceCode naming standard) + Total
# - Grabbing sex + Total
# - Using life expectancy age groups which can be found in ageLink.xlsx + grabbing Total (which will be filtered out)
# - Grabbing years 2000-2020
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
                               popData = tDat0, 
                               server = F)


# 2a) Preparing popCounty in the desired format to make it easy to create nxCounty & nxState
# - Filtering out Total ageGroup
# - Replacing countyName with GEOID using countyFIPS
# - ageGroups are formatted, for example, 5 - 9. Splitting up this column into agell, and ageul

popCounty1 <- popCounty %>%
  ungroup() %>%
  filter(ageGroup != "Total") %>%
  left_join(countyLink, by = c("county" = "countyName")) %>%
  mutate(GEOID = ifelse(county == "CALIFORNIA", "06000000000", 
         paste0("0", fips, "000000")), 
         agell = as.integer(sub("\\-.*", "", ageGroup)), 
         ageul = as.integer(sub(".*-", "", ageGroup))) %>%
  select(GEOID, sex, year, agell, ageul, Nx = population, raceCode)

# 2b) Create nxCounty
nxCounty <- popCounty1 %>%
  filter(GEOID != "06000000000")

# Data Check --- 

table(nxCounty$GEOID, nxCounty$agell)

nxCounty1 <- complete(nxCounty, GEOID, year, sex, raceCode, agell) %>%
  mutate(ageul = ifelse(is.na(ageul) & agell == 0, 0, ageul)) %>%
  replace_na(list(Nx = 0))

table(nxCounty1$GEOID, nxCounty1$agell)

# 2c) Create nxState
nxState <- popCounty1 %>%
  filter(GEOID == "06000000000")

# --- MSSA --------------------------------------------------------------------------

# 0.CCB/myUpstream/lifeTables/dataIn/nxMSSA.RDS is the MSSA pop data (dating back to 2007) used for LE estimates, containing:

# 1) 2007-2017 MSSA Pop pulled by Ethan (0.CCB/myUpstream/lifeTables/dataIn/acs5_mssa.dta)
# 2) 2018- pulled by CCB Data Team using the standard pullACS function


# 2020 ACS-5 year is released in December 2021, so will use 2019 MSSA as 2020

nxMSSA <- readRDS(paste0(ccbUpstream, "lifeTables/dataIn/nxMSSA.RDS"))

t_nxMSSA <- nxMSSA %>%
  filter(year == (myYear - 1)) %>%
  mutate(year = myYear)

nxMSSA_final <- bind_rows(nxMSSA, t_nxMSSA)


# SAVE

saveRDS(nxCounty1, paste0(ccbUpstream, "lifeTables/dataIn/nxCounty.RDS"))
saveRDS(nxState, paste0(ccbUpstream, "lifeTables/dataIn/nxState.RDS"))
saveRDS(nxMSSA_final, paste0(ccbUpstream, "lifeTables/dataIn/nxMSSA.RDS"))








# NEED TO AUTOMATE # JASPO - DONT RUN CODE BELOW


# Read in nxMSSA.RDS
nxMSSA <- readRDS(paste0(ccbUpstream, "lifeTables/dataIn/nxMSSA.RDS"))


if (isRecent_acsSurvey) {
  
  source(paste0(fusionPlace, "SDOH/pullACS_function.R")) # Standard pullACS function
  
  # ACS IDs: 5 year age groups
  sdohVars <- readxl::read_xlsx(path = paste0(fusionPlace, "SDOH/linkageSDOH.xlsx"), 
                                sheet = "Age 5 Year Groups") %>%
    pull(measureLabel)
  
  # Pull ACS data
  mssaPop <- pullACS(State=06, # 06 = California
                      Geography="mssa", # Options: county, mssa, tract, zcta... for acs1, puma
                      Survey="acs5", # Options: "acs1", "acs5"
                      Year=myYear,
                      moeLevel=95, # margin of error level
                      asList = F, # Want as a list, or as one data frame?
                      server = server, # Set to T if in R Studio Pro; otherwise set to F
                      sdohVars = sdohVars) %>% # Character value or vector of sdoh variables (measureLabel)
    mutate(year = myYear) 
  
  # Prepare data frame to bind with nxMSSA
  t_nxMSSA <- mssaPop %>%
    mutate(raceCode = "Total",
           sex = str_to_title(sub("\\ .*", "", measure)), 
           ageGroup = sub(".*ale", "", measureLabel), 
           agell = as.numeric(sub("\\-.*", "", ageGroup)), 
           ageul = as.numeric(sub(".*-", "", ageGroup))) %>%
    select(year, agell, ageul, sex, comID, nx = numerator, raceCode)
  
  nxMSSA_final <- bind_rows(nxMSSA, t_nxMSSA)
  
  
}


if (!isRecent_acsSurvey) {
  
  previousYear <- myYear - 1
  
  nxMSSA
  
  
}

year1 <- 2018
year2 <- 2019
year3 <- 2020

mssaPop1 <- pullACS(State=06, # 06 = California
                    Geography="mssa", # Options: county, mssa, tract, zcta... for acs1, puma
                    Survey="acs5", # Options: "acs1", "acs5"
                    Year=year1,
                    moeLevel=95, # margin of error level
                    asList = F, # Want as a list, or as one data frame?
                    server = server, # Set to T if in R Studio Pro; otherwise set to F
                    sdohVars = sdohVars) %>% # Character value or vector of sdoh variables (measureLabel)
  mutate(year = year1)  

mssaPop2 <- pullACS(State=06, # 06 = California
                    Geography="mssa", # Options: county, mssa, tract, zcta... for acs1, puma
                    Survey="acs5", # Options: "acs1", "acs5"
                    Year=year2,
                    moeLevel=95, # margin of error level
                    asList = F, # Want as a list, or as one data frame?
                    server = server, # Set to T if in R Studio Pro; otherwise set to F
                    sdohVars = sdohVars) %>% # Character value or vector of sdoh variables (measureLabel)
  mutate(year = year2) 


# TEMP - Using 2019 ACS for 2020 data since 2020 ACS is not out yet

mssaPopTemp <- mssaPop2 %>% mutate(year = year3)


mssaPop <- bind_rows(mssaPop1, mssaPop2)

# commLinkage <- read.csv("/mnt/projects/FusionData/Standards/Tract to Community Linkage.csv", header = T) %>%
#   select(-year) %>%
#   mutate(GEOID = paste0("0", GEOID))

# not-controlled

nxMSSA <- mssaPop %>%
  mutate(raceCode = "Total",
         sex = str_to_title(sub("\\ .*", "", measure)), 
         ageGroup = sub(".*ale", "", measureLabel), 
         agell = as.numeric(sub("\\-.*", "", ageGroup)), 
         ageul = as.numeric(sub(".*-", "", ageGroup))) %>%
  select(year, agell, ageul, sex, comID, nx = numerator, raceCode)

nxMSSA <- commLinkage %>%
  left_join(mssaPop, by = "GEOID") %>%
  group_by(year, comID, comName, county, Variable, Indicator, Group) %>%
  summarise(Numerator = sum(Numerator, na.rm = T), 
            Denominator = sum(Denominator, na.rm = T)) %>%
  mutate(estimate = Numerator/Denominator) %>%
  ungroup() %>%
  mutate(raceCode = "Total",
         sex = str_to_title(sub("\\ .*", "", Variable)), 
         ageGroup = sub(".*ale", "", Indicator), 
         agell = as.numeric(sub("\\-.*", "", ageGroup)), 
         ageul = as.numeric(sub(".*-", "", ageGroup))) %>%
  select(year, agell, ageul, sex, comID, nx = Numerator, raceCode)

nxACS <- read.dta13(paste0(myPath,"0.CCB/myUpstream/lifeTables/dataIn/acs5_mssa.dta")) %>% # ACS tract pop collapsed to MSSA
  rename(Ethan = race7) %>%
  left_join(select(raceLink, raceCode, Ethan), by = "Ethan") %>%
  select(-Ethan) %>%
  filter(year != 2018) %>%
  bind_rows(nxMSSA)

# Controlled

# popCounty_forMSSA <- populationExtract(County = T, 
#                                Race   = F,
#                                Sex    = T, 
#                                Age    = T,
#                                Year      = 2017,
#                                ageGroups = c(-1, seq(4, 84, by = 5), 199), 
#                                ageLabels = seq(0, 85, by = 5),
#                                raceLabel = "raceCode", 
#                                CA    = F, 
#                                Total = F,
#                                multiYear = F, 
#                                popData = tDat0, 
#                                path = "mnt")
# 
# popCounty_forMSSA <- popCounty_forMSSA %>%
#   ungroup() %>%
#   mutate(agell = as.numeric(ageGroup), 
#          ageul = ifelse(agell < 85, agell + 4, 199))  %>%
#   select(year, county = countyName, sex, agell, population)
# 
# 
# popControl <- check %>%
#   left_join(popCounty_forMSSA, by = c("year", "county", "sex", "agell")) %>%
#   group_by(agell, sex, county) %>%
#   mutate(countyNx = sum(nx)) %>%
#   ungroup() %>%
#   mutate(mult = population/countyNx)
# 
# 
# nxACS_c <- read.dta13(paste0(upPlace,"/lifeTables/dataIn/acs5_mssa_adj.dta")) %>% # ACS tract pop, collapsed to MSSA and controlled to DOF county
#     rename(Ethan = race7) %>%
#     left_join(select(raceLink, raceCode, Ethan), by = "Ethan") %>%
#     select(-Ethan) %>%
#   filter(year == 2017)



# 4) Save nxCounty & nxState
saveRDS(nxCounty1, paste0(myPath, "0.CCB/myUpstream/lifeTables/dataIn/nxCounty.RDS"))
saveRDS(nxState, paste0(myPath, "0.CCB/myUpstream/lifeTables/dataIn/nxState.RDS"))
saveRDS(nxACS, paste0(myPath, "0.CCB/myUpstream/lifeTables/dataIn/nxMSSA.RDS"))

check <- readRDS(paste0(ccbUpstream, "lifeTables/dataIn/nxCounty.RDS"))
