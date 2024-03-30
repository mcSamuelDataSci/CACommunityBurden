# Documentation ===========================================================================================

# Author: Jaspreet Kang
# Date: 01/26/2024

# About:
# New script for creating state level, county level, city lhj level, and health department level age-race-sex population denominator data

# Steps:
# 1. Read in and process DOF P3 2000-2009 data and 2010-2060 data (pre-Decennial 2020 file release July 2021)
# 2. Extract City and County age-race-sex population data from Decennial 2000, 2010, and 2020.
# 3. Get Census City ARS Strata Proportion:
#   - Census City ARS Strata Proportion = (Census City ARS Strata Population) / (Census County ARS Strata Population)
# 4. Calculate City ARS Strata Population 
#   - City ARS Strata Population = (Census City ARS Strata Proportion) * (DOF P3 County ARS Strata Population)
# 5. Calculate Health Department ARS Strata Population
#   - Health Department ARS Strata Population = (DOF P3 County ARS Strata Population) - (City ARS Strata Population)

# Notes:
# State level = California
# County level = 58 California counties
# City LHJ level = Berkeley, Long Beach, Pasadena
# Health Department level = Alameda HD, Los Angeles HD
# Census has R/E group 'Other' whereas DOF does not. 'Other' is combined with 'White'

# Global constants ==================================================================================
recentYear <- 2023 
useRecentDOF <- F # TRUE to use DOF's recent P3 informed by Decennial 2023 for 2020+ denominators; FALSE to use DOF P3 pre-decennial for 2020+ denominators

# Load standards ====================================================================================
server <- F
if (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
if (!server) source("G:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")

# Set paths ===================================================================================
popPlace <- paste0(fusionPlace, "Population Data/")

## linkage files =================================================================================
countyLink1 <- readxl::read_xlsx(paste0(standardsPlace, "countyLink.xlsx")) %>% 
  select(countyName, CountyCode=cdphcaCountyTxt,FIPSCounty)%>%
  mutate(fips = as.numeric(paste0("6", FIPSCounty))) %>% 
  select(-FIPSCounty)

countyLink2 <- readxl::read_xlsx(paste0(standardsPlace, "countyLink.xlsx")) %>% 
  select(county = countyName, fips = FIPSCounty) %>% 
  mutate(fips = as.numeric(paste0("6", fips)))

ageLink <- read_xlsx(paste0(standardsPlace, "ageLink.xlsx"), sheet = "standard")
ageLinkLE <- read_xlsx(paste0(standardsPlace, "ageLink.xlsx"), sheet = "ageLE")
raceLink <- read_xlsx(paste0(standardsPlace, "raceLink.xlsx"))
raceLink_dof <- raceLink %>% 
  select(raceCode, race7) %>% 
  filter(!is.na(race7))

yearLink <- readxl::read_xlsx(paste0(ccbInfo, "Year to Year-Group Linkage.xlsx")) %>% 
  select(year, yearGroup3, midYear3)

# Read in and process DOF data ===============================================================================

## 2000-2009 ----------------------
p3_2000_2009 <- read_csv(paste0(popPlace, "Intercensal_2000-2010_DBInput.csv")) %>% 
  mutate(year  = as.numeric(str_sub(Year,5,9)),
         month = as.numeric(str_sub(Year,1,1))) %>%    # 2000 has both April and July estimates; 2010 only April; all others only July
  filter(month == 7)  %>%
  select(CountyCode, year, sex=Gender, race7=RaceCode, age=Age, population=Population) %>% # CountyCode - 2 digit character
  full_join(countyLink1,by="CountyCode") %>%                                                # "01", "02" ... "58", "59"
  filter(CountyCode != "59") %>% select(-CountyCode) %>%                                   #  58 - Yuba
  mutate(race7 = ifelse(race7== 6,99,race7))  %>%                                          #  59 - California  -- CHECK THIS
  mutate(race7 = ifelse(race7== 7, 6,race7))  %>%
  mutate(race7 = ifelse(race7==99, 7,race7))  %>% 
  select(county = countyName, year, sex, race7, age, population) %>% 
  filter(year != 2010)

## 2010- ---------------------
# Vintage 2020 (2021.7.14) version
p3_10_ <- read_csv(paste0(popPlace, "P3_2010-2060.csv")) %>%
  left_join(countyLink2, by = "fips") %>% 
  mutate(sex = str_to_title(sex)) %>% 
  select(county, year, sex, race7, age = agerc, population = perwt)

if (useRecentDOF) {
  p3_10_ <- p3_10_ %>% 
    filter(year %in% 2010:2019)
  
  p3_20_ <- read_csv(paste0(popPlace, "p3_2020-2060.csv")) %>% 
    filter(year %in% 2020:recentYear) %>% 
    left_join(countyLink2, by = "fips") %>% 
    mutate(sex = str_to_title(sex)) %>% 
    select(county, year, sex, race7, age = agerc, population = perwt)
  
  p3_10_ <- bind_rows(p3_10_, p3_20_)
} else {
  p3_10_ <- p3_10_ %>% 
    filter(year %in% 2010:recentYear)
}

## P3 Final ----------------------
p3_final <- bind_rows(p3_2000_2009, p3_10_) %>% 
  left_join(raceLink_dof) %>% 
  select(-race7) %>% 
  mutate(age = case_when(age >= 100 ~ "100+", 
                         TRUE ~ as.character(age))) %>% 
  group_by(county, year, sex, age, raceCode) %>% 
  summarise(population = sum(population)) %>% 
  ungroup()

# Read in Census Decennial data =============================================================================

censusFinal_city <- readRDS(paste0(ccbUpstream, "upData/decennial-city-ars.RDS"))
censusFinal_county <- readRDS(paste0(ccbUpstream, "upData/decennial-county-ars.RDS"))

# Make County LHJ Population Data ===========================================================================================

# County estimates ----------------------------------
countyARS <- p3_final

# City estimates ----------------------------------
censusCityARS <- censusFinal_city %>%  
  filter(NAME %in% c("Berkeley city, California", "Pasadena city, California", "Long Beach city, California"), 
         raceCode != "Total", sex != "Total", age != "Total") %>% 
  mutate(NAME = sub(" city, California", "", NAME), 
         county = ifelse(NAME == "Berkeley", "Alameda", "Los Angeles") 
         # raceCode = ifelse(raceCode == "Other", "White", raceCode) # Discuss with Michael.
         ) %>% 
  group_by(year, NAME, county, sex, age, raceCode) %>% 
  summarise(populationCity = sum(population)) %>% 
  ungroup()


censusCountyARS <- censusFinal_county %>% 
  filter(NAME %in% c("Alameda County, California", "Los Angeles County, California"), 
         raceCode != "Total", sex != "Total", age != "Total") %>% 
  mutate(NAME = sub(" County, California", "", NAME) 
         # raceCode = ifelse(raceCode == "Other", "White", raceCode) # Discuss with Michael.
         ) %>% 
  group_by(year, county = NAME, sex, age, raceCode) %>% 
  summarise(populationCounty = sum(population)) %>% 
  ungroup()

censusCityCountyARS <- censusCityARS %>% 
  left_join(censusCountyARS, by = c("year", "county", "sex", "age", "raceCode")) %>% 
  mutate(cityCountyProp = populationCity / populationCounty, 
         cityCountyProp = ifelse(is.nan(cityCountyProp), 0, cityCountyProp)) # Discuss with Michael. All NANs are 0/0

joinCityCounty <- function(myYear) {
  myYears <- myYear:(myYear+9)
  
  tCity <- censusCityCountyARS %>% filter(year == myYear) %>% select(-year)
  tCounty <- countyARS %>% filter(year %in% myYears)
  tCity %>% 
    left_join(tCounty, by = c("county", "sex", "age", "raceCode"), relationship = "many-to-many") 
}

cityARS_final <- bind_rows(
  joinCityCounty(2000),
  joinCityCounty(2010),
  joinCityCounty(2020)
) %>% 
  mutate(population = cityCountyProp * population) %>%
  select(county = NAME, year, sex, age, raceCode, population)

table(cityARS_final$county, cityARS_final$year, useNA = "ifany")
length(unique(cityARS_final$sex)) * length(unique(cityARS_final$age)) * length(unique(cityARS_final$raceCode))

# Health Department estimates ----------------------
hdARS <- cityARS_final %>% 
  mutate(county = ifelse(county == "Berkeley", "Berkeley", "Pasadena/Long Beach")) %>% 
  group_by(city = county, year, sex, age, raceCode) %>% 
  summarise(city_population = sum(population)) %>% 
  ungroup() %>% 
  mutate(county = ifelse(city == "Berkeley", "Alameda", "Los Angeles")) %>% 
  left_join(rename(countyARS, county_population = population)) %>% 
  mutate(population = county_population - city_population,
         # POP = ifelse(POP < 0, 0, POP),
         county = ifelse(city == "Berkeley", "Alameda HD", "Los Angeles HD"))%>% 
  select(county, year, sex, age, raceCode, population)


# Append and save --------------------------------
lhjARS <- bind_rows(countyARS, cityARS_final, hdARS) %>% 
  select(county, year, sex, age, raceCode, population) %>% 
  arrange(county, year, sex, age, raceCode)

# Checks -----------------------------------------

colSums(is.na(lhjARS))

# count(lhjARS, county, year) %>% 
#   View()
table(lhjARS$county, lhjARS$year, useNA = "ifany")

## Convert age into age groups =================================

getAgeGroups <- function(myCuts, myLabels) {
  lhjARS %>% 
    mutate(age = case_when(age == "100+" ~ 100, 
                           TRUE ~ as.numeric(age)), 
           ageGroup = cut(age, myCuts, labels = myLabels, include.lowest = T, right = F)) %>% 
    group_by(county, year, sex, ageGroup, raceCode) %>% 
    summarise(population = sum(population)) %>% 
    ungroup()
}

lhjARS_final <- getAgeGroups(c(ageLink$lAge, 999), 
                             ageLink$ageName)

lhjARS_le <- getAgeGroups(c(ageLinkLE$lAge, 999), 
                             ageLinkLE$ageName)


# For DEMOGRAPHICS Tab
if (F) {
  ageDF <- data.frame(lAge = seq(0, 95, by = 5), 
                      uAge = seq(4, 99, by = 5)) %>%
    mutate(ageName = paste0(lAge, " - ", uAge)) %>%
    tibble::add_row(lAge = 100, uAge = 120, ageName = "100+")
  
  ageDF2 <- data.frame(lAge = c(0, 15, 25, 35, 45, 65, 75), 
                       uAge = c(14, 24, 34, 44, 64, 74, 120)) %>%
    mutate(ageName = paste0(lAge, " - ", uAge), 
           ageName = ifelse(ageName == "75 - 120", "75+", ageName))
  
  lhjARS_demoTab1 <- getAgeGroups(c(ageDF$lAge, 999), 
                                  ageDF$ageName)
  
  lhjARS_demoTab2 <- getAgeGroups(c(ageDF2$lAge, 999), 
                                  ageDF2$ageName)
}



# Get totals ===================================================

getTotals <- function(myData) {
  
  lhjCA <- myData %>% 
    filter(!county %in% c("Berkeley", "Long Beach", "Pasadena", "Alameda HD", "Los Angeles HD")) %>% 
    mutate(county = "CALIFORNIA") %>% 
    group_by(county, year, sex, ageGroup, raceCode) %>% 
    summarise(population = sum(population)) %>% 
    ungroup()
  
  myData <- bind_rows(myData, lhjCA)
  
  lhjSex <- myData %>% 
    mutate(sex = "Total") %>% 
    group_by(county, year, sex, ageGroup, raceCode) %>% 
    summarise(population = sum(population)) %>% 
    ungroup()
  
  myData <- bind_rows(myData, lhjSex)
  
  lhjRace <- myData %>% 
    mutate(raceCode = "Total") %>% 
    group_by(county, year, sex, ageGroup, raceCode) %>% 
    summarise(population = sum(population)) %>% 
    ungroup()
  
  myData <- bind_rows(myData, lhjRace)
  
  lhjAge <- myData %>% 
    mutate(ageGroup = "Total") %>% 
    group_by(county, year, sex, ageGroup, raceCode) %>% 
    summarise(population = sum(population)) %>% 
    ungroup()
  
  myData <- bind_rows(myData, lhjAge)
  
  return(myData)
  
}

lhjARS_final <- getTotals(lhjARS_final)
lhjARS_le <- getTotals(lhjARS_le)

if (F) {
  lhjARS_demoTab1 <- getTotals(lhjARS_demoTab1)
  lhjARS_demoTab2 <- getTotals(lhjARS_demoTab2)
}

# Save data - 1 year ==============================================
saveRDS(lhjARS_final, paste0(ccbUpstream, "upData/lhj-population-ars.RDS"))
saveRDS(lhjARS_le, paste0(ccbUpstream, "upData/lhj-population-ars-le.RDS"))

if (F) {
  saveRDS(lhjARS_demoTab1, paste0(ccbUpstream, "upData/lhj-population-ars-demoTab1.RDS"))
  saveRDS(lhjARS_demoTab2, paste0(ccbUpstream, "upData/lhj-population-ars-demoTab2.RDS")) 
}


# Produce 3 year estimates ========================================
lhjARS_final <- readRDS(paste0(ccbUpstream, "upData/lhj-population-ars.RDS"))
lhjARS_le <- readRDS(paste0(ccbUpstream, "upData/lhj-population-ars-le.RDS"))

lhjARS_final3 <- lhjARS_final %>% 
  left_join(yearLink) %>% 
  group_by(county, yearG3 = yearGroup3, sex, ageGroup, raceCode) %>% 
  summarise(population = sum(population)) %>% 
  ungroup()

lhjARS_le3 <- lhjARS_le %>% 
  left_join(yearLink) %>% 
  group_by(county, yearG3 = yearGroup3, sex, ageGroup, raceCode) %>% 
  summarise(population = sum(population)) %>% 
  ungroup()

# Save data - 3 year ==============================================
saveRDS(lhjARS_final3, paste0(ccbUpstream, "upData/lhj-population-ars-3year.RDS"))
saveRDS(lhjARS_le3, paste0(ccbUpstream, "upData/lhj-population-ars-le-3year.RDS"))


# Region ==========================================================

regionLink <- readxl::read_xlsx(paste0(standardsPlace, "countyLink.xlsx"))%>%
  select(county = countyName, region = RPHO)

popRegion <- lhjARS_final %>%
  ungroup() %>%
  left_join(regionLink, by = "county") %>%
  mutate(region = ifelse(county == "CALIFORNIA", "CALIFORNIA", region))

table(popRegion$county, popRegion$region, useNA = "ifany") # NAs should only be present in the 3 city jurisdictions and 2 HDs

popRegion <- popRegion %>%
  filter(!is.na(region)) %>% 
  group_by(year, region, raceCode, ageGroup, sex) %>%
  summarise(population = sum(population, na.rm = T))

colSums(is.na(popRegion))
table(popRegion$region)

saveRDS(popRegion,          file = paste0(ccbUpstream,"upData/popRegion.RDS"))
