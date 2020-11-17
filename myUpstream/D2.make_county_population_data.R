# ====================================================================================================
# "countyPopulationProcessor.R" file                                                                 |
#                                                                                                    |
#            Reads in age-race-sex-1980-2015 text file provided by CDPH-CID-DCDC-STDCB               |
#            Subsets and processes data, and saves as County by year total population file           |
#            Adds stratification by age group, and saves another file (for age adjustment)           |
#            Generates total 2015 CA pop by age to use as "Standard Population" for age adjustment   |
#                                                                                                    |
#                                                                                                    |   
# ====================================================================================================

# -- Set locations and load packages ---------------------------------------------------------------------------------------------------

myDrive    <- getwd()
myPlace    <- paste0(myDrive,"/myCBD")
upPlace    <- paste0(myDrive,"/myUpstream")

local <- F

if (local) {
  myPath <- "G:/CCB/"
} else {
  myPath <- "/mnt/projects/CCB/"
}


library(dplyr)
library(readxl)
library(tidyr)
library(readr)
# library(stringr)

# source(paste0(myPlace,"/myFunctions/helperFunctions/capwords.R"))

# ageMap  <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/Age Group Standard and US Standard 2000 Population.xlsx"),sheet = "data"))
# aL      <-      ageMap$lAge
# aU      <- c(-1,ageMap$uAge)


# === National Institutes of Health - SEER Data ==========================================================================================
# U.S. Population Data - 1969-2017
# county population estimates by age, sex, race, and Hispanic origin
# https://seer.cancer.gov/popdata/
  

# === California DEPARTMENT OF FINANCE DATA ==========================================================================================


# temp <- read_csv("F:/0.CBD.Other/Resources/populationData/DOF/P3_complete.csv")
# http://www.dof.ca.gov/Forecasting/Demographics/Projections/
# http://www.dof.ca.gov/Forecasting/Demographics/Projections/P3_Dictionary.txt

# tDatDOF  <- read_csv("https://data.ca.gov/sites/default/files/dof_dru_pop_1970_2050_csya_wide.csv",col_types="_ciiiii") 


# tDatDOF <- read_csv("/mnt/projects/CCB/Population Data/DOF/2c217b79-4625-4ab2-86b3-6fc5d66f0409.csv") %>%
#               select(-fips)
# 
# names(tDatDOF)
# # [1] "county"     "year"       "age"        "pop_female" "pop_male"   "pop_total" 
# 
# tDat     <- tDatDOF  %>% gather(key="sex",value="pop",-county,-year,-age) %>%
#                          filter(year %in% 2000:2018)
# 
# aMark       <- findInterval(tDat$age,aU,left.open = TRUE)
# aLabs       <- paste(aL,"-",aU[-1])
# tDat$ageGroup   <- aLabs[aMark]
# 
# tDat    <- tDat  %>% mutate(sex    = c("Male","Female","Total")[match(sex,c("pop_male","pop_female","pop_total"))],
#                             county =  capwords(tDat$county,strict=TRUE)
#                            )
# 
# tDatState <- mutate(tDat,county="California")
# 
# tDat <- bind_rows(tDat,tDatState)
# 
# popCounty      <- tDat %>% group_by(year, county,sex,ageGroup) %>%  summarize(pop  = sum(pop)) 
# popCountytemp  <- tDat %>% group_by(year, county,sex     ) %>%  summarize(pop  = sum(pop)) %>%  mutate(ageGroup = "Total")
# popCounty      <- bind_rows(popCounty,popCountytemp) %>% ungroup()
#                             
#                     
# tDat65           <- filter(tDat, age < 65)
# popCounty65      <- tDat65 %>% group_by(year, county,sex,ageGroup) %>%  summarize(pop  = sum(pop)) 
# popCountytemp65  <- tDat65 %>% group_by(year, county,sex     ) %>%  summarize(pop  = sum(pop)) %>%  mutate(ageGroup = "Total")
# popCounty65      <- bind_rows(popCounty65,popCountytemp65) %>% ungroup()



# == CDPH DIVISION OF COMMINIABLE DISEASE CONTROL DATA ================================================================================

# special file location becuase file is too big for GitHub
# tDat        <- read.delim(file=paste0(myDrive,"/0.CBD.Other/Resources/populationData/1980_2025.txt"),sep="\t", header = TRUE, stringsAsFactors = FALSE)
# tDat        <- filter(tDat, YEAR >=2000 & YEAR <= 2020)
# saveRDS(tDat, file= paste0(upPlace,"/upData/tDat_2000_2020.rds"))



tDat0 <- readRDS(file= paste0(myPath, "Population Data/DCDC/tDat_2000_2020.rds")) %>%
            filter(!(COUNTY %in% c("California", "Alameda HD", "Berkeley", "Pasadena", "Long Beach", "Los Angeles HD"))) %>%  # NOTE: California totals already included with county = "California
            rename(year=YEAR, county= COUNTY, sex = SEX, agerc = AGE, raceE = RE, perwt = POP) %>%
            mutate(sex = c("Male","Female")[match(sex,c("M","F"))]) %>%
            filter(year %in% 2000:2020)


# IN DEATH DATA
# vLab  <- c("White-NH","Black-NH","AIAN-NH","Asian-NH","NHPI-NH","Other-NH","Multi-NH","Unk-NH","Hisp")
  vLab  <- c("White-NH","Black-NH","AIAN-NH","Asian-NH","NHPI-NH",           "Multi-NH",         "Hisp")
  vLab  <- c("f",       "b",       "a",      "c",       "e",                 "g",                "d")
  vLab  <- c(  1,         2,        3,        4,         5,                   6,                  7)
  
# NO OTHER
# NO UNKNOWN
rCode    <- c("W","B","I","A","P","M","H")
tDat0    <-  tDat0 %>%  mutate( race7   = vLab[match(raceE,rCode)])

countyLink <- readxl::read_xlsx(paste0(myPath, "Standards/countyLink.xlsx")) %>%
  select(countyName, FIPSCounty) %>%
  mutate(fips = as.numeric(paste0("6", FIPSCounty)))

tDat0  <- tDat0 %>%  left_join(countyLink, by = c("county" = "countyName"))


# -----------------------------------------------------------------------------

# if/when we get P3_complete.csv for *1990* onward, then delete ALL of above and start here

# -----------------------------------------------------------------------------

source(paste0(myPath,"Standards/populationExtract.R"))

# -----------------------------------------------------------------------------

popCounty <- populationExtract(County = T, 
                               Race   = T,
                               Sex    = T, 
                               Age    = T,
                               Year      = 2000:2020,
                               ageGroups = "standard", 
                               ageLabels,
                               raceLabel = "raceCode", 
                               CA    = T, 
                               Total = T,
                               multiYear = F, 
                               popData = tDat0)

popCounty <- popCounty %>% rename(county = countyName) %>% mutate(county = ifelse(county=="California","CALIFORNIA",county))

yearMap   <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/Year to Year-Group Linkage.xlsx")))
tDat1     <- popCounty %>%  mutate( yearG3 = yearMap[match(year,yearMap[,"year"]),"yearGroup3"]) 
                     
popCounty_RE_3year <- tDat1 %>% group_by(yearG3, county, sex, ageGroup, raceCode) %>%    ### was ageGroup !!!!!
                         summarize(population  = sum(population)) %>%
                         ungroup() %>%
                         filter(!is.na(yearG3))


# -----------------------------------------------------------------------------

tDat65      <- filter(tDat0, agerc < 65)

popCounty65 <- populationExtract(County = T, 
                               Race   = T,
                               Sex    = T, 
                               Age    = T,
                               Year      = 2000:2020,
                               ageGroups = "standard", 
                               ageLabels,
                               raceLabel = "raceCode", 
                               CA    = T, 
                               Total = T,
                               multiYear = F, 
                               popData = tDat65)

popCounty65 <- popCounty65 %>% rename(county = countyName) %>% mutate(county = ifelse(county=="California","CALIFORNIA",county))

tDat1     <- popCounty65 %>%  mutate( yearG3 = yearMap[match(year,yearMap[,"year"]),"yearGroup3"]) 

popCounty65_RE_3year <- tDat1 %>% group_by(yearG3, county, sex, ageGroup, raceCode) %>%    ### was ageGroup !!!!!
                         summarize(population  = sum(population)) %>%
                         ungroup() %>%
                         filter(!is.na(yearG3))

# ======================================================================================================================================

saveRDS(popCounty,          file = paste0(upPlace,"/upData/popCounty.RDS"))
saveRDS(popCounty_RE_3year, file = paste0(upPlace,"/upData/popCounty_RE_3year.RDS"))

saveRDS(popCounty65,          file = paste0(upPlace,"/upData/popCounty65.RDS"))
saveRDS(popCounty65_RE_3year, file = paste0(upPlace,"/upData/popCounty65_RE_3year.RDS"))



