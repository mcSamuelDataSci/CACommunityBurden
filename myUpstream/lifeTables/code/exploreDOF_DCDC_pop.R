##### ----------------------------------------------------------------------------------------------------

# Creating population files (nxCounty & nxState) for life expectancy calculation

#### ----------------------------------------------------------------------------------------------------

library(readxl)
library(dplyr)
library(stringr)
library(readstata13)

options(scipen = 999) # Gets rid of scientific notation

# --- Set most recent year ------------------------------------------------------------------------------

myYear <- 2020

# --- Set necessary paths ---------------------------------------------------------------------------------

server <- T
if (server) {
  myPath <- "/mnt/projects/FusionData/"
  myStandards <- "/mnt/projects/FusionData/Standards/"
} else {
  myPath <- "G:/FusionData/"
  myStandards <- "G:/FusionData/Standards/"
}



raceLink <- read_xlsx(paste0(myStandards, "raceLink.xlsx")) 

# 1) Read in DCDC population file since it goes back to 2000. Cleaning up data frame to prepare it for populationExtract standard function.

dof <- readRDS(paste0(myPath, "Population Data/dof_pop_2000_", myYear, ".RDS"))

tDat0 <- readRDS(file= paste0(myPath, "Population Data/Archive DCDC-STD Data and Documentation/tDat_2000_2020.rds")) %>%
  filter(!(COUNTY %in% c("California", "Alameda HD", "Berkeley", "Pasadena", "Long Beach", "Los Angeles HD"))) %>%  # NOTE: California totals already included with county = "California
  rename(year=YEAR, county= COUNTY, sex = SEX, agerc = AGE, raceE = RE, perwt = POP) %>%
  mutate(sex = c("MALE","FEMALE")[match(sex,c("M","F"))]) %>%
  filter(year %in% 2000:2020) %>%
  left_join(select(raceLink, race7, DCDC), by = c("raceE" = "DCDC")) %>% # JASPO EDIT - TAKE OUT, AND UNCOMMENT BELOW IF NEW RACECODES CAUSES MAJOR ISSUES
  select(-raceE)

# 1a) FIPS is required in populationExtract standard function

countyLink <- readxl::read_xlsx(paste0(myPath, "Standards/countyLink.xlsx")) %>%
  select(countyName, FIPSCounty) %>%
  # mutate(GEOID = paste0("06", FIPSCounty, "000000"))
  mutate(fips = as.numeric(paste0("6", FIPSCounty)))

tDat0  <- tDat0 %>%  left_join(countyLink, by = c("county" = "countyName"))


checkDOF <- dof %>%
  group_by(year) %>%
  summarise(popDOF = sum(population))

checkDCDC <- tDat0 %>%
  group_by(year) %>%
  summarise(popDCDC = sum(perwt))

check <- checkDOF %>%
  full_join(checkDCDC, by = "year") %>%
  mutate(diff = as.integer(popDOF - popDCDC))
