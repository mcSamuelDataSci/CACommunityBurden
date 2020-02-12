
library(sqldf)
library(dplyr)
library(readxl)
library(shiny)
library(fs)
library(stringr)
library(ggplot2)
library(sqldf)
library(RColorBrewer)

myMeasure = "n_hosp"
mySex     = "Total"
myN       = 10
myYearG3 = "2016-2018"



myTextSize = 18

tempPlace <- "tempPlace"

oshpd_PDD_AGE  <- readRDS(file = path(tempPlace, "oshpd_PDD_AGE.rds")) 

oshpd_ED_AGE   <- readRDS(file = path(tempPlace, "oshpd_ED_AGE.rds"))

ccsMap  <- as.data.frame(read_excel( path(tempPlace,"CCS Code and Names Linkage.xlsx"))) %>%
  mutate(ccsCode = str_pad(ccsCode, 5,"left",pad="o")) %>%
  select("ccsCode","ccsName","birth")

datCounty_AGE_3year   <- readRDS(file = path(tempPlace, "datCounty_AGE_3year.rds"))
datCounty_RE          <- readRDS(file = path(tempPlace, "datCounty_RE.RDS"))


gbdMap0 <- as.data.frame(read_excel( path(tempPlace,"gbd.ICD.Map.xlsx"), sheet="main"))

fullCauseList     <- gbdMap0[!is.na(gbdMap0$causeList),] %>% arrange(LABEL)  %>%
  filter(!is.na(PH),is.na(DETAIL)) %>%
  select(LABEL,causeList,nameOnly)

ageList    <- unique(oshpd_PDD_AGE$ageG) 
raceList   <- unique(datCounty_RE$raceCode)
countyList <- unique(datCounty_AGE_3year$county)





source(path(tempPlace, "make_rank_STRATA_AGE_chart.R"))
source(path(tempPlace, "make_rank_STRATA_RACE_chart.R"))


#(for testing code outside of the app)
if(1==2) {
  myCounty = "CALIFORNIA"
  mySex = "Total"
  myN = 10
  myYearG3 = "2016-2018"
  myAgeG = "75 - 84"
  myAgeG = "0 - 4"
  myAgeG = "25 - 34"
  myAgeG = "15 - 24"
  myMeasure = "n_hosp"
  myData = "PDD"
  myData = "ED"
  #cDeathRate"
}



