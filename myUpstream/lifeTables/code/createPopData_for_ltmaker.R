##### ----------------------------------------------------------------------------------------------------

# Creating population files (nxCounty & nxState) for life expectancy calculation

#### ----------------------------------------------------------------------------------------------------


server <- F
if (server) {
  myPath <- "/mnt/projects/FusionData"
  myStandards <- "/mnt/projects/FusionData/Standards/"
} else {
  myPath <- "G:/FusionData/"
  myStandards <- "G:/FusionData/Standards/"
}



raceLink <- read_xlsx(paste0(myStandards, "raceLink.xlsx")) 

# 1) Read in DCDC population file since it goes back to 2000. Cleaning up data frame to prepare it for populationExtract standard function.

tDat0 <- readRDS(file= paste0(myPath, "Population Data/DCDC/tDat_2000_2020.rds")) %>%
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


# -----------------------------------------------------------------------------
source(paste0(myPath,"Standards/populationExtract.R"))


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
                               Year      = 2000:2020,
                               ageGroups = "ageLE", 
                               ageLabels,
                               raceLabel = "raceCode", 
                               CA    = T, 
                               Total = T,
                               multiYear = F, 
                               popData = tDat0, 
                               path = "g")


# 2a) Preparing popCounty in the desired format to make it easy to create nxCounty & nxState
# - Filtering out Total ageGroup
# - Replacing countyName with GEOID using countyFIPS
# - ageGroups are formatted, for example, 5 - 9. Splitting up this column into agell, and ageul
popCounty1 <- popCounty %>%
  ungroup() %>%
  filter(ageGroup != "Total") %>%
  left_join(countyLink, by = "countyName") %>%
  mutate(GEOID = ifelse(countyName == "California", "06000000000", 
         paste0("0", fips, "000000")), 
         agell = as.integer(sub("\\-.*", "", ageGroup)), 
         ageul = as.integer(sub(".*-", "", ageGroup)), 
         sex = str_to_upper(sex)) %>%
  select(GEOID, sex, year, agell, ageul, Nx = population, raceCode)

# 2b) Create nxCounty
nxCounty <- popCounty1 %>%
  filter(GEOID != "06000000000")

# 2c) Create nxState
nxState <- popCounty1 %>%
  filter(GEOID == "06000000000")

# -----------------------------------------------------------------------------

# 3) Save nxCounty & nxState
saveRDS(nxCounty, paste0(myPath, "CCB Project/0.CCB/myUpstream/lifeTables/dataIn/nxCounty.RDS"))
saveRDS(nxState, paste0(myPath, "CCB Project/0.CCB/myUpstream/lifeTables/dataIn/nxState.RDS"))

