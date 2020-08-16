
securePath     <- "g:/CCB/0.Secure.Data/"
securePath     <- "h:/0.Secure.Data/"
secureDataFile <- paste0(securePath,"myData/cbdDat0FULL.R") 
load(secureDataFile)

myDrive <- "f:/0.CBD"  
myPlace <- paste0(myDrive,"/myCBD") 
upPlace <- paste0(myDrive,"/myUpstream") 

library(readr)
library(stringr)
library(dplyr)
library(readxl)
library(fs)


#== LOAD STANDARDS AND DATA MAPPING FILES =========================================================

geoMap     <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/County Codes to County Names Linkage.xlsx")))

# -- RECODES AND CALCULATIONS -------------------------------------------------

cbdDat0       <- mutate(cbdDat0FULL,
                         sex     = c("Male","Female")[match(sex,c("M","F"))],
                         ICD10   = as.character(ICD10),                                              # redundant...
                         )


# -- Map ICD-10 codes to GBD conditions ----------------------------------------

gbdMap0   <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/gbd.ICD.Map.xlsx"), sheet="main"))   

mapICD    <- gbdMap0[!is.na(gbdMap0$CODE),c("CODE","regEx10")]

icdToGroup <- function(inputVectorICD10) {
  Cause   <- rep(NA,length(inputVectorICD10))
  for (i in 1:nrow(mapICD)) {Cause[grepl(mapICD[i,"regEx10"],inputVectorICD10)] <- mapICD[i,"CODE"] } 
  Cause}

cbdDat0$icdCODE  <- icdToGroup(inputVectorICD10=cbdDat0$ICD10) 
ccbLink          <- select(gbdMap0,LABEL,causeList) %>% filter(!is.na(LABEL)) %>% mutate(LABEL = paste0("c",LABEL))
cbdDat0          <- left_join(cbdDat0,ccbLink,by=c("icdCODE"="LABEL"))


ccsLink   <- as.data.frame(read_excel(paste0(upPlace,"/upData/ccs_dx_icd10cm_2019_1.xlsx"))) %>%
                       select(`ICD-10-CM CODE`, `ICD-10-CM CODE DESCRIPTION`,`CCS CATEGORY DESCRIPTION`) %>%
                       mutate(ICD10 = str_sub(`ICD-10-CM CODE`,1,4))


workDat0 <-  left_join(cbdDat0,ccsLink,by="ICD10") 
workDat  <-  workDat0 %>% select(ICD10,`ICD-10-CM CODE`,`ICD-10-CM CODE DESCRIPTION`,`CCS CATEGORY DESCRIPTION`,causeList)

workExam <- unique(workDat) %>% arrange(ICD10)








freqExam  <- workDat %>% group_by(causeList,`CCS CATEGORY DESCRIPTION`) %>% summarise(Count=n())




junk2 <- workDat2 %>% filter(causeList == "...D.04. - Substance use",CCSR1_DESCRIPTION=="Schizophrenia spectrum and other psychotic disorders")

junk3 <- workDat  %>% filter(causeList == "...C.02. - Ischaemic heart disease",	`CCS CATEGORY DESCRIPTION` == "Complication of device; implant or graft")


junk4 <- unique(junk3)



icdCODE





library(summarytools)
freq(workDat$CCSR1_DESCRIPTION)

freq(cbdDat0$icdCODE)






