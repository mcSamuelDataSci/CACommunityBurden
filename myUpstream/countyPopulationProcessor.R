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

source(paste0(myPlace,"/myFunctions/helperFunctions/capwords.R"))

# special file location becuase file is too big for GitHub
# tDat        <- read.delim(file=paste0(myDrive,"/0.CBD.Other/Resources/populationData/1980_2025.txt"),sep="\t", header = TRUE, stringsAsFactors = FALSE)
# tDat        <- filter(tDat, YEAR >=2000 & YEAR <= 2020)
# saveRDS(tDat, file= paste0(upPlace,"/upData/tDat_2000_2020.rds"))
tDat <- readRDS(file= paste0(upPlace,"/upData/tDat_2000_2020.rds"))

names(tDat) <- c("county","year","sex","age","raceE","OrigPop","pop" )


# TEMP
# tDatX     <- filter(tDat, year %in% c(2010,2017) & county == "California")
# popX      <- tDatX %>% group_by(year, raceE) %>%  summarize(pop  = sum(pop))  
# write_csv(popX,"dofpop.csv")
# tDat        <- filter(tDat, year %in% 2000:2015 & !(county %in% c("Alameda HD","Berkeley","Pasadena","Long Beach","Los Angeles HD")))


tDat        <- filter(tDat, year %in% 2000:2017 & !(county %in% c("Alameda HD","Berkeley","Pasadena","Long Beach","Los Angeles HD")))

library(readr)
# could use DOF data instead, downloaded from their Open Data Site
# does not contain race/ethnicity which will be needed soon
# "col types" excludes FIPS and male pop and female pop
tDatDOF        <- read_csv("https://data.ca.gov/sites/default/files/dof_dru_pop_1970_2050_csya_wide.csv",col_types="_cii__i")
tDatDOF        <- filter(tDatDOF, year %in% 2000:2015)
tDatDOF$county <- capwords(tDatDOF$county,strict=TRUE)


# -- Make and save file with County (and California Total) Pop by year 2000-2015 -----------------------------------------------------------

popCountyTot2000to2015 <- tDat %>% group_by(year, county) %>% summarize(pop  = sum(pop))
saveRDS(popCountyTot2000to2015, file= paste0(upPlace,"/upData/popCountyTot2000to2017.RDS"))

# -- Construct age groups, stratify pop file by age group and save ---------------------------------------------------------------------------

ageMap  <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/acsCensus.Map.xlsx"),sheet = "ageList"))
ageMap  <- ageMap[!is.na(ageMap$inAgeG),c("lAge","uAge")]
aL      <-      ageMap$lAge
aU      <- c(-1,ageMap$uAge)

aMark       <- findInterval(tDat$age,aU,left.open = TRUE)
aLabs       <- paste(aL,"-",aU[-1])
tDat$ageG   <- aLabs[aMark]

popCountyAgeG2000to2015 <- tDat %>% group_by(year, county,ageG) %>% 
                                    summarize(pop  = sum(pop))  %>% 
                                    filter(county != "California")

saveRDS(popCountyAgeG2000to2015, file= paste0(upPlace,"/upData/popCountyAgeG2000to2015.RDS"))

# Life Table Thinking...
# aLLT <- c(0,1,5,...90,100)
# aULt <- 

# -- Construct and save file with total state popuation by age groups to use as "Standard Population"------------------------------------------

popStandard     <-  tDat %>% filter(year == 2015, county == "California") %>%
                             group_by(ageG) %>%  
                             summarize(popStandard  = sum(pop))

saveRDS(popStandard, file= paste0(upPlace,"/upData/popStandard.RDS"))