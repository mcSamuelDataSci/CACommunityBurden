myDrive <- "E:"  
myPlace <- paste0(myDrive,"/0.CBD/myCBD") 
upPlace <- paste0(myDrive,"/0.CBD/myUpstream") 


myPlace <- "G:/CCB/tempCode"     


library(tidyverse)
library(epitools)
library(sqldf)
library(readxl)

#-- LOAD MAIN DATA SET, AND "INFO FILES", BEGIN KEY TRANSFORMATIONS------------------------

# CAUTION --- if using REAL DATA INCLUDE these two lines below and edit the first one with your secure location
 load("G:/CCB/0.Secure.Data/myData/cbdDat0FULL.R")     
# load("F:/0.Secure.Data/myData/cbdDat0FULL.R")      
 cbdDat0 <- cbdDat0FULL    

# Load FAKE Data --- COMMENT OUT these two lines if using REAL DATA
   load(paste0(upPlace,"/upData/cbdDat0SAMP.R"))      
   cbdDat0 <- cbdDat0SAMP

#                                                              JUNK   
gbdMap0    <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/JUNK.gbd.ICD.Map.xlsx"), sheet="main"))   # also have e.g. range="A1:J167"


# function to map ICD-10 codes to GBD conditions
icdToGroup <- function(myIn,myInMap) {
  Cause   <- rep(NA,length(myIn))
  for (i in 1:nrow(myInMap)) {Cause[grepl(myInMap[i,2],myIn)] <- myInMap[i,1] } 
  Cause}

cbdDat0$gbd36   <- icdToGroup(myIn=cbdDat0$ICD10,gbdMap0[!is.na(gbdMap0$list36),c("gbdCode","regEx10")])      


#----------------

cbdDat0$CauseOld  <- gbdMap0[match(cbdDat0$gbd36,gbdMap0[,"gbdCode"]),"nameOnly"]


gbdMapNew         <-  as.data.frame(read_excel(paste0(myPlace,"/myInfo/ICD10.xlsx"))) 
gbdMapNew$ICD     <-  paste0(substr(gbdMapNew$cause_code,1,3),substr(gbdMapNew$cause_code,5,5))
cbdDat0$CauseNew  <-  gbdMapNew[match(cbdDat0$ICD10,gbdMapNew[,"ICD"]),"acause"]
cbdDat0$CauseNewID  <-  gbdMapNew[match(cbdDat0$ICD10,gbdMapNew[,"ICD"]),"cause_id"]


sN <- 100000

cbdDatSAMP  <- sample_n(cbdDat0,sN)[,c("year","county","ICD10","gbd36","CauseOld","CauseNew","CauseNewID")]

cbdDatWORK  <- cbdDat0[,c("year","county","ICD10","gbd36","CauseOld","CauseNew","CauseNewID")]

write.csv(cbdDatWORK,(paste0("G:/CCB/tempCode" ,"/ICD WORK Investigate.csv")))
