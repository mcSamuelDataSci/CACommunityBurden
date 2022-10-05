# ====================================================================================================
# "countyPopulationProcessor.R" file                                                                 |
#                                                                                                    |
#            Subsets and processes data, and saves as County by year total population file           |
#            Adds stratification by age group, and saves another file (for age adjustment)           |
#            Generates total 2015 CA pop by age to use as "Standard Population" for age adjustment   |
#                                                                                                    |
#                                                                                                    |   
# ====================================================================================================

# -- Set most recent year --------------------------------------------------------------------------------------------------------------

myYear <- 2022
isRecent_multiYear <- T # T if using recent multi-year groups; F if not

# -- Set locations and load packages ---------------------------------------------------------------------------------------------------

server <- F
# CCB <- F

if (!server) source("g:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
if (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")


# -- Get links -------------------------------------------------------------------------------------------------------------------------

# raceLink 

countyLink <- readxl::read_xlsx(paste0(standardsPlace, "countyLink.xlsx")) %>%
               select(countyName, CountyCode=cdphcaCountyTxt,FIPSCounty)%>%
               mutate(fips = as.numeric(paste0("6", FIPSCounty))) %>% 
               select(-FIPSCounty)

if (isRecent_multiYear) yearMap   <- as.data.frame(read_excel(paste0(ccbInfo,"Year to Year-Group Linkage.xlsx"), sheet = "main"))
if (!isRecent_multiYear) yearMap   <- as.data.frame(read_excel(paste0(ccbInfo,"Year to Year-Group Linkage.xlsx"), sheet = "old"))

# -- Get raw popualtion data  --------------------------------------------------------------------------------------------------------


# from: https://www.dof.ca.gov/forecasting/demographics/Estimates/Race-Ethnic/2000-2010/

dof_pop_2000_2009 <- read_csv(paste0(fusionPlace, "Population Data/Intercensal_2000-2010_DBInput.csv")) %>%
                       mutate(year  = as.numeric(str_sub(Year,5,9)),
                              month = as.numeric(str_sub(Year,1,1))) %>%    # 2000 has both April and July estimates; 2010 only April; all others only July
                       filter(month == 7)  %>%
                       select(CountyCode, year, sex=Gender, race7=RaceCode, age=Age, population=Population) %>% # CountyCode - 2 digit character
                       full_join(countyLink,by="CountyCode") %>%                                                # "01", "02" ... "58", "59"
                       filter(CountyCode != "59") %>% select(-CountyCode) %>%                                   #  58 - Yuba
                       mutate(race7 = ifelse(race7== 6,99,race7))  %>%                                          #  59 - California  -- CHECK THIS
                       mutate(race7 = ifelse(race7== 7, 6,race7))  %>%
                       mutate(race7 = ifelse(race7==99, 7,race7))  

# from: https://www.dof.ca.gov/forecasting/demographics/Projections/  - P3.complete
dof_pop_2010_myYear <- read_csv(paste0(fusionPlace, "Population Data/P3_Complete.csv")) %>%
                       filter(year <= myYear) %>%
                       select(fips, year, sex, race7, age= agerc, population=perwt)   %>%     # fips - 4 digit character 
                       full_join(countyLink,by="fips") %>%                                    # "6001" - "6115"
                       mutate(sex = str_to_title(sex))

dof_pop_2000_myYear <- bind_rows(dof_pop_2000_2009,dof_pop_2010_myYear) %>% select(-CountyCode)  %>% rename(county = countyName)

#check totals  
checkPop <- dof_pop_2000_myYear %>% group_by(year) %>% summarise(totPop=sum(population))

# -- Save 2000-most recent year DOF pop file -------------------------------------

fileName <- paste0(fusionPlace, "Population Data/dof_pop_2000plus.RDS")

saveRDS(dof_pop_2000_myYear, file = fileName)

# junk1 <- dof_pop_2000_2009 %>% filter(year==2010) %>% rename(pop2010_2000_2010_file = population)
# junk2 <- dof_pop_2010_2020 %>% filter(year==2010) %>% rename(pop2010_2010_2020_file = population)
# junk3 <- full_join(junk1,junk2,by=c("countyName","fips","year", "sex", "race7", "age"))
# ggplot(data=junk3,aes(x=pop2010_2000_2010_file,y=pop2010_2010_2020_file)) + geom_point()
# junk4 <- junk3 %>% filter(abs(pop2010_2000_2010_file - pop2010_2010_2020_file) > 500)
# sum(junk1$pop2010_2000_2010_file)
# sum(junk2$pop2010_2010_2020_file)





# -- Process and save data ------------------------------------------------------------------------------------------------------------

# Bring in function for aggregating population data
source(paste0(standardsPlace,"populationExtract.R"))

# For the function, we can't have county column since the function links fips code to county
dof_pop_2000_myYear <- select(dof_pop_2000_myYear, -county)

popCounty <- populationExtract(County    = T, 
                               Race      = T,
                               Sex       = T, 
                               Age       = T,
                               Year      = 2000:myYear,
                               ageGroups = "standard", 
                               ageLabels,
                               raceLabel = "raceCode", 
                               CA        = T, 
                               Total     = T,
                               multiYear = F, 
                               popData   = dof_pop_2000_myYear, 
                               server = server)


popCounty_RE_3year  <- popCounty %>% 
                         mutate( yearG3 = yearMap[match(year,yearMap[,"year"]),"yearGroup3"]) %>%
                         group_by(yearG3, county, sex, ageGroup, raceCode) %>%    
                         summarize(population  = sum(population)) %>%
                         ungroup() %>% filter(!is.na(yearG3))

# -----------------------------------------------------------------------------

popCounty65 <- populationExtract(County  = T, 
                               Race      = T,
                               Sex       = T, 
                               Age       = T,
                               Year      = 2000:myYear,
                               ageGroups = "standard", 
                               ageLabels,
                               raceLabel = "raceCode", 
                               CA        = T, 
                               Total     = T,
                               multiYear = F, 
                               popData   = filter(dof_pop_2000_myYear, age < 65), 
                               server = server)  



popCounty65_RE_3year     <- popCounty65 %>%  
                              mutate( yearG3 = yearMap[match(year,yearMap[,"year"]),"yearGroup3"]) %>% 
                              group_by(yearG3, county, sex, ageGroup, raceCode) %>%   
                              summarize(population  = sum(population)) %>%
                              ungroup() %>%  filter(!is.na(yearG3))


saveRDS(popCounty,          file = paste0(ccbUpstream,"upData/popCounty.RDS"))
saveRDS(popCounty_RE_3year, file = paste0(ccbUpstream,"/upData/popCounty_RE_3year.RDS"))

saveRDS(popCounty65,          file = paste0(ccbUpstream,"/upData/popCounty65.RDS"))
saveRDS(popCounty65_RE_3year, file = paste0(ccbUpstream,"/upData/popCounty65_RE_3year.RDS"))



# Region
regionLink <- readxl::read_xlsx(paste0(standardsPlace, "countyLink.xlsx")) %>%
  select(county = countyName, region = FUSION)

popRegion <- popCounty %>%
  ungroup() %>%
  left_join(regionLink, by = "county") %>%
  mutate(region = ifelse(county == "CALIFORNIA", "CALIFORNIA", region)) %>%
  group_by(year, region, raceCode, ageGroup, sex) %>%
  summarise(population = sum(population, na.rm = T))

# options(scipen = 99)

saveRDS(popRegion,          file = paste0(ccbUpstream,"upData/popRegion.RDS"))

# = DCDC ============================================================================================================

# Prior to January 2021, county population data was based on a data set complied by CID/DCDC/STD Control
#   these data, code, and documentation are in the "Archive DCDC-STD Data and Documentation" folder.
#   Key aspects of our processing code for that is kept here:

# special file location becuase file is too big for GitHub
# tDat        <- read.delim(file=paste0(myDrive,"/0.CBD.Other/Resources/populationData/1980_2025.txt"),sep="\t", header = TRUE, stringsAsFactors = FALSE)
# tDat        <- filter(tDat, YEAR >=2000 & YEAR <= 2020)
# saveRDS(tDat, file= paste0(upPlace,"/upData/tDat_2000_2020.rds"))


# tDat0 <- readRDS(file= paste0(myPath, "Population Data/DCDC/tDat_2000_2020.rds")) %>%
#   filter(!(COUNTY %in% c("California", "Alameda HD", "Berkeley", "Pasadena", "Long Beach", "Los Angeles HD"))) %>%  # NOTE: California totals already included with county = "California
#   rename(year=YEAR, county= COUNTY, sex = SEX, agerc = AGE, raceE = RE, perwt = POP) %>%
#   mutate(sex = c("Male","Female")[match(sex,c("M","F"))]) %>%
#   filter(year %in% 2000:2020) %>%
#   left_join(select(raceLink, race7, DCDC), by = c("raceE" = "DCDC")) %>% # JASPO EDIT - TAKE OUT, AND UNCOMMENT BELOW IF NEW RACECODES CAUSES MAJOR ISSUES
#   select(-raceE)
# 
# tDat0  <- tDat0 %>%  left_join(countyLink, by = c("county" = "countyName"))


# === National Institutes of Health - SEER Data ==========================================================================================
# U.S. Population Data - 1969-2017
# county population estimates by age, sex, race, and Hispanic origin
# https://seer.cancer.gov/popdata/


# === California DEPARTMENT OF FINANCE DATA ==========================================================================================


# temp <- read_csv("F:/0.CBD.Other/Resources/populationData/DOF/P3_complete.csv")
# http://www.dof.ca.gov/Forecasting/Demographics/Projections/
# http://www.dof.ca.gov/Forecasting/Demographics/Projections/P3_Dictionary.txt

# tDatDOF  <- read_csv("https://data.ca.gov/sites/default/files/dof_dru_pop_1970_2050_csya_wide.csv",col_types="_ciiiii") 



