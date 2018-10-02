
whichDat <- "real"

myDrive <- "E:"  
myPlace <- paste0(myDrive,"/0.CBD/myCBD") 
upPlace <- paste0(myDrive,"/0.CBD/myUpstream") 

library(dplyr)
library(readxl)

if (whichDat == "real") {
  # CAUTION --- if using REAL DATA INCLUDE these two lines below and edit the first one with your secure location
  load("G:/CCB/0.Secure.Data/myData/cbdDat0FULL.R")     
  #load("H:/0.Secure.Data/myData/cbdDat0FULL.R")      
  cbdDat0 <- cbdDat0FULL    
}

if (whichDat == "fake") { 
  # Load FAKE Data --- COMMENT OUT these two lines if using REAL DATA
  load(paste0(upPlace,"/upData/cbdDat0SAMP.R"))      
  cbdDat0 <- cbdDat0SAMP
}

cbdDat0       <- mutate(cbdDat0, ICD10  = as.character(ICD10)   )

# Map ICD-10 codes to GBD conditions ----------------------------

gbdMap0   <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/gbd.ICD.Map.xlsx"), sheet="main"))   # also have e.g. range="A1:J167"

mapICD    <- gbdMap0[!is.na(gbdMap0$CODE),c("CODE","regEx10")]

icdToGroup <- function(myIn) {
  Cause   <- rep(NA,length(myIn))
  for (i in 1:nrow(mapICD)) {Cause[grepl(mapICD[i,"regEx10"],myIn)] <- mapICD[i,"CODE"] } 
  Cause}

cbdDat0$icdCODE  <- icdToGroup(myIn=cbdDat0$ICD10)

cbdWhat <- filter(cbdDat0, is.na(icdCODE))  #3453 --> 378
table(cbdWhat$ICD10)


r95 <- filter(cbdDat0,ICD10=="R95")
table(r95$year)

y350 <- filter(cbdDat0,ICD10=="Y350")
table(y350$sex)
table(y350$year)
# Y350 --> WAR

# X85	Assault by drugs, medicaments and biological substances

xMiss <- filter(cbdDat0,ICD10=="")
table(xMiss$year)

#2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 
# 137  117  131  178   52  337  391  244  131   19    2    5   13

