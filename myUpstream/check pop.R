

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


popCounty        <- readRDS(path(upPlace,"/upData/popCounty.RDS")) %>% ungroup() 
popCountyCheck   <- filter(popCounty,raceCode=="Total", year < 2019) %>%
                      mutate(population=round(population))

popCountyOLD     <- readRDS(path(upPlace,"/upData/SAVEDforCOMAPRE/popCounty.RDS")) %>% ungroup() 



sum(popCountyCheck$population)
sum(popCountyOLD$pop)


t1      <- filter(popCountyCheck, year == 2018)
t1OLD   <- filter(popCountyOLD, year == 2018)


freq(t1$ageGroup)

