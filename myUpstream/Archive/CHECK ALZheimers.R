
standardsPlace <- "/mnt/projects/FusionData/Standards/"
server <- TRUE
source(paste0(standardsPlace, "FusionStandards.R"))

myCCBPlace <- paste0(ccbPlace, "myCCB/")


secureDataFile <- paste0(securePlace,"myData/ccb_processed_deaths.RDS") 



library(readr)
library(stringr)
library(dplyr)
library(epitools)
library(sqldf)
library(readxl)
library(fs)

  cbdDat0 <- readRDS(secureDataFile)

  
  gbdMap0       <- as.data.frame(read_excel(paste0(ccbInfo,"icd10_to_CAUSE.xlsx"), sheet="main"))   
  
  allCauseCodes <- sort(gbdMap0$causeCode[!is.na(gbdMap0$causeCode)])
  mapICD    <- gbdMap0[!is.na(gbdMap0$CODE),c("CODE","regEx10")]
  
  
  icdToGroup <- function(inputVectorICD10) {
    Cause   <- rep(NA,length(inputVectorICD10))
    for (i in 1:nrow(mapICD)) {Cause[grepl(mapICD[i,"regEx10"],inputVectorICD10)] <- mapICD[i,"CODE"] } 
    Cause}
  
  cbdDat0$icdCODE  <- icdToGroup(inputVectorICD10=cbdDat0$ICD10)
  
  
  tDat <- filter(cbdDat0,year==2020)
  freq(tDat$icdCODE)
  
  tDatX <- filter(tDat,is.na(icdCODE))
  freq(tDatX$ICD10)
  
  table(cbdDat0$icdCODE,useNA = "ifany")
  tDat$icdCODE[tDat$ICD10 %in% c("")] <- "cZ02"  
  
  
  freq(tDat$icdCODE)
  
  
  
  tDatXX <- filter(tDat,icdCODE == "cZ02")
  freq(tDatXX$age)
  
  
  
  
  
  
  
  
  
  workData <- cbdDat0 %>%
    #  mutate(azTest = str_detect(ICD10,"F0[1-3]|G3[0-1]"))
    mutate(covid = str_detect(ICD10,"U07"))
  workData2 <- filter(workData,covid)
  
  
  
  workData <- cbdDat0 %>%
            #  mutate(azTest = str_detect(ICD10,"F0[1-3]|G3[0-1]"))
              mutate(azTest = str_detect(ICD10,"F0[0-3]|G3[0-1]"))
  
  
  # there do not appear to be any F00 at all in the data, so expressions above result in same data...
                     
  ctable(workData$year,workData$azTest)                                       
                                         
  
  workData2 <- filter(workData,azTest)
  
  freq(workData2$ICD10)
  
#   F00	Dementia in Alzheimer disease
#   F00.0	Dementia in Alzheimers disease with early onset
#   F00.1	Dementia in Alzheimer disease
#   F00.2	Dementia in Alzheimer disease
#   F00.9	Dementia in Alzheimer disease
#   F01	Vascular dementia
#   F01.0	Vascular dementia of acute onset
#   F01.1	Multi-infarct dementia
#   F01.2	Subcortical vascular dementia
#   F01.3	Mixed cortical and subcortical vascular dementia
#   F01.5	Vascular dementia
#   F01.50	Vascular dementia without behavioral disturbance
#   F01.51	Vascular dementia with behavioral disturbance
#   F01.8	Other vascular dementia                  
#   F01.9	Vascular dementia, unspecified                    <--- 7%
#   F02	Dementia in other diseases classified elsewhere
#   F02.0	Dementia in other diseases classified elsewhere
#   F02.1	Dementia in other diseases classified elsewhere
#   F02.2	Dementia in other diseases classified elsewhere
#   F02.3	Dementia in other diseases classified elsewhere
#   F02.4	Dementia in other diseases classified elsewhere
#   F02.8	Dementia in other diseases classified elsewhere
#   F02.80	Dementia in other diseases classified elsewhere without behavioral disturbance
#   F02.81	Dementia in other diseases classified elsewhere with behavioral disturbance
#   F03	Unspecified dementia                              <---- 23%  
#   F03.0	
#   F03.9	Unspecified dementia
#   F03.90	Unspecified dementia without behavioral disturbance
#   F03.91	Unspecified dementia with behavioral disturbance
#   
#   
#   
# G30	  Alzheimer's disease
# G30.0	Alzheimer's disease with early onset
# G30.1	Alzheimer's disease with late onset
# G30.8	Other Alzheimer's disease
# G30.9	Alzheimer's disease, unspecified               <-- 62%

  
  
  
  