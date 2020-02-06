

# EDIT SECURE DATA LOCATION AS NEEDED
securePath     <- "g:/CCB/0.Secure.Data/"
securePath     <- "h:/0.Secure.Data/"

secureDataFile <- paste0(securePath,"myData/cbdDat0FULL.R") 

STATE    <- "California"

myDrive <- getwd()  
myPlace <- paste0(myDrive,"/myCBD") 
upPlace <- paste0(myDrive,"/myUpstream") 

library(readr)
library(stringr)
library(dplyr)
library(epitools)
library(sqldf)
library(readxl)
library(fs)
library(summarytools)
library(tidyr)


moreAlpine <- read_rds(paste0(myPlace,"/myData/noSupression/datCounty_AGE_3year.RDS"))

alpineJunk <- filter(moreAlpine,county=="Alpine",sex=="Total",CAUSE=="0") %>%
                select(yearG3,ageG,Ndeaths) %>% 
                 pivot_wider(names_from = ageG,values_from = Ndeaths)


write.csv(alpineJunk,"alpineDeaths.csv")



moreAlpine <- read_csv(file= path(myPlace,"/myData/noSupression/analysisDataSets/County.csv"))





alpineJunk <- filter(moreAlpine,county=="Alpine",Level=="lev2",year > 2014) %>%
                         select(causeName,year,sex,Ndeaths) %>% 
                         pivot_wider(names_from = sex,values_from = Ndeaths,values_fill=list(Ndeaths = 0))
write.csv(alpineJunk,"alpineDeaths2015_2018.csv")



ctable(alpineJunk$CAUSE,alpineJunk$year)


alpineJunk <- filter(cbdDat0FULL,county=="Alpine")
table(alpineJunk$year)

bigDat <- readRDS(file= path(securePath,"/myData/cbdDat0-INVESTIGATION-FILE.RDS"))


yuck <- filter(bigDat,icdCODE == "cD04b",sex=="Total")

freq(yuck$ICD10)



# https://www.cdc.gov/nchs/data/nvsr/nvsr67/nvsr67_09-508.pdf