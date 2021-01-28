######  CELL SUPRESSION

# in SAS file consider standardizing approach to "year" in PDD and ED data 

# OSHPD sheet on coding of charges:
# https://oshpd.ca.gov/ml/v1/resources/document?rs:path=/Data-And-Reports/Documents/Submit/Patient-Level-Administrative/IP/IP-Total-Charges.pdf

# ====================================================================================================
# "E3.Process_OSHPD_data.R" file                                                                     |
#                                                                                                    |
#            Reads in OSHPD 2016 PDD sas file, saves as rds file (random sample)                     |
#            Used to assess hospitalizations for diabetes--primary diagnoses and all diagnoses.      |
#                                                                                                    |
# ====================================================================================================


#---SET LOCATIONS-----------------------------------------------------------------------
  
server  <- TRUE

 if (!server) source("g:/FusionData/Standards/FusionStandards.R")
 if (server) source("/mnt/projects/FusionData/Standards/FusionStandards.R")


whichData <- "real"   # "real" or "fake"

yF    <- 100000  # rate constant 
STATE <- "CALIFORNIA"
myYearG.t <- "2016-2018"

#-------------------------------------------------LOAD PACKAGES -------------------------


pdd0    <- readRDS(paste0(securePlace,"/myData/oshpd_pdd_small.RDS")) 
ctable(pdd0$patcnty, pdd0$year, prop="c")


secureDataFile <- paste0(securePlace,"myData/ccb_processed_deaths.RDS") 
cbdDat0 <- readRDS(secureDataFile)

ctable(cbdDat0$year, cbdDat0$CHSI, prop="r")


