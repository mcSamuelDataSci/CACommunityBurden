# ====================================================================================================
# "countyPopulationProcessor.R" file                                                                 |
#                                                                                                    |
#            Reads in age-race-sex-1980-2015 text file provided by CDPH-CID-DCDC-STDCB               |
#            Subsets and processes data, and saves as County by year total population file           |
#            Adds stratification by age group, and saves another file (for age adjustement)          |
#            Generates total 2015 CA pop by age to use as "Standard Population" for age adjustment   |
#                                                                                                    |
#                                                                                                    |   
# ====================================================================================================

# -- Set locations and load packages ---------------------------------------------------------------------------------------------------

myDrive    <- "E:"
myPlace    <- paste0(myDrive,"/0.CBD/myCBD")
upPlace    <- paste0(myDrive,"/0.CBD/myUpstream")

library(dplyr)
library(readxl)
library(tidyr)
library(readr)

source(paste0(myPlace,"/myFunctions/helperFunctions/capwords.R"))

ageMap  <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/Age Group Standard and US Standard 2000 Population.xlsx"),sheet = "data"))
aL      <-      ageMap$lAge
aU      <- c(-1,ageMap$uAge)

# === California DEPARTMENT OF FINANCE DATA ==========================================================================================

tDatDOF  <- read_csv("https://data.ca.gov/sites/default/files/dof_dru_pop_1970_2050_csya_wide.csv",col_types="_ciiiii") 

tDat     <- tDatDOF  %>% gather(key="sex",value="pop",-county,-year,-age) %>%
                         filter(year %in% 2000:2017)

aMark       <- findInterval(tDat$age,aU,left.open = TRUE)
aLabs       <- paste(aL,"-",aU[-1])
tDat$ageG   <- aLabs[aMark]

tDat    <- tDat  %>% mutate(sex    = c("Male","Female","Total")[match(sex,c("pop_male","pop_female","pop_total"))],
                            county =  capwords(tDat$county,strict=TRUE)
                           )

tDatState <- mutate(tDat,county="California")

tDat <- bind_rows(tDat,tDatState)

popCounty      <- tDat %>% group_by(year, county,sex,ageG) %>%  summarize(pop  = sum(pop)) 
popCountytemp  <- tDat %>% group_by(year, county,sex     ) %>%  summarize(pop  = sum(pop)) %>%  mutate(ageG = "Total")
popCounty      <- bind_rows(popCounty,popCountytemp) %>% ungroup()
                            
                    
# == CDPH DIVISION OF COMMINIABLE DISEASE CONTROL DATA ================================================================================


# special file location becuase file is too big for GitHub
# tDat        <- read.delim(file=paste0(myDrive,"/0.CBD.Other/Resources/populationData/1980_2025.txt"),sep="\t", header = TRUE, stringsAsFactors = FALSE)
# tDat        <- filter(tDat, YEAR >=2000 & YEAR <= 2020)
# saveRDS(tDat, file= paste0(upPlace,"/upData/tDat_2000_2020.rds"))
tDat <- readRDS(file= paste0(upPlace,"/upData/tDat_2000_2020.rds"))

names(tDat) <- c("county","year","sex","age","raceE","OrigPop","pop" )

tDat        <- filter(tDat, year %in% 2000:2017 & !(county %in% c("Alameda HD","Berkeley","Pasadena","Long Beach","Los Angeles HD")))
# NOTE: California totals already included with county = "California" 


aMark       <- findInterval(tDat$age,aU,left.open = TRUE)
aLabs       <- paste(aL,"-",aU[-1])
tDat$ageG   <- aLabs[aMark]

tDatTotAge  <-  tDat %>% mutate(ageG = "Total")
tDat        <-  bind_rows(tDat,tDatTotAge) 

tDatTotSex  <- tDat %>% mutate(sex = "T")
tDat        <- bind_rows(tDat,tDatTotSex) %>% mutate(sex = c("Male","Female","Total")[match(sex,c("M","F","T"))])


# IN DEATH DATA
# vLab  <- c("White-NH","Black-NH","AIAN-NH","Asian-NH","NHPI-NH","Other-NH","Multi-NH","Unk-NH","Hisp")
  vLab  <- c("White-NH","Black-NH","AIAN-NH","Asian-NH","NHPI-NH",           "Multi-NH",         "Hisp")
# NO OTHER
# NO UNKNOWN
rCode    <- c("W","B","I","A","P","M","H")
tDat     <-  tDat %>%  mutate( raceCode   = vLab[match(raceE,rCode)])

# RECONSIDER AT SOME POINT
# tDatTot  <-  tDat %>% mutate(raceCode = "Total")
# tDat     <-  bind_rows(tDat,tDatTot) 

yearMap   <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/Year to Year-Group Linkage.xlsx")))
tDat      <- tDat %>%  mutate( yearG3 = yearMap[match(year,yearMap[,"year"]),"yearGroup3"]) 
                               
popCounty_RE <- tDat %>% group_by(yearG3, county, sex, ageG, raceCode) %>% 
                         summarize(pop  = sum(pop)) %>%
                         ungroup()

# table(popCounty_RE$sex,useNA = "ifany")
# table(popCounty_RE$ageG,useNA = "ifany")
# table(popCounty_RE$raceCode,useNA = "ifany")

# ======================================================================================================================================

saveRDS(popCounty,    file = paste0(upPlace,"/upData/popCounty.RDS"))
saveRDS(popCounty_RE, file = paste0(upPlace,"/upData/popCounty_RE.RDS"))


# popCounty_RE %>%
#   filter(year==2017,ageG=="Total",sex=="Total",county != "California",raceCode=="Total") %>%
#   group_by(sex) %>%
#   mutate(newPop=sum(pop)) %>% ungroup()
