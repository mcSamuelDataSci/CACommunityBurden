######  CELL SUPRESSION

# OSHPD sheet on coding of charges:
# https://oshpd.ca.gov/ml/v1/resources/document?rs:path=/Data-And-Reports/Documents/Submit/Patient-Level-Administrative/IP/IP-Total-Charges.pdf

# ====================================================================================================
# "E3.Process_OSHPD_data.R" file                                                                     |
#                                                                                                    |
#            Reads in OSHPD 2016 PDD sas file, saves as rds file (random sample)                     |
#            Used to assess hospitalizations for diabetes--primary diagnoses and all diagnoses.      |
#                                                                                                    |
# ====================================================================================================



## TODO could/should standardize all OSHPD names in this step,  eg  countyCode = patco

#---SET LOCATIONS-----------------------------------------------------------------------
  
# PROVIDE PATH FOR SECURE DATA HERE
secure.location   <- "G:/CCB/0.Secure.Data/"
secure.location   <- "/mnt/projects/CCB/0.Secure.Data/"

myDrive <- getwd()  #Root location of CBD project
myPlace <- paste0(myDrive,"/myCBD") 
upPlace <- paste0(myDrive,"/myUpstream")

whichData <- "real"   # "real" or "fake"

yF    <- 100000  # rate constant 
STATE <- "CALIFORNIA"

#-------------------------------------------------LOAD PACKAGES -------------------------

library(dplyr)
library(readr)
library(haven)
library(fs)

#-----------------------------LOADING/CREATING OSHPD DATASET FROM ORIGINAL DATA FILE-----

# PDD - Patient Discharge Data ---------------------------------

  # Now done in SAS--------------------------------------------------------------------------------------------------
  # ourVars  <- c("diag_p", "ccs_diagP", "mdc", "msdrg", "charge", 
  #               "pay_cat", "pay_type", "admtyr","patcnty", "patzip", "sex", "agyrdsch", "race_grp", "oshpd_id", 
  #               "los_adj", "los") # dschdate,
  # 
  # oshpd.PDD.16.0  <- read_sas(paste0(secure.location,"rawOSHPD/PDD/cdph_pdd_rln2016.sas7bdat") ) 
  # oshpd.PDD.16    <- oshpd.PDD.16.0 %>% select(ourVars,contains("ccs_odiag"),contains("odiag")) %>% mutate(year=2016)
  # etc
  # oshpd_subset  <- bind_rows(oshpd.PDD.16, oshpd.PDD.17,oshpd.PDD.18)
  #-------------------------------------------------------------------------------------------------------------------
  
  oshpd.pdd.work1  <- read_sas(paste0(secure.location,"rawOSHPD/PDD/pdd_work1.sas7bdat") )
  saveRDS(oshpd.pdd.work1, file = path(secure.location, "myData/oshpd_pdd.RDS"))

  # FOR AGE/RACE Focus charts
  oshpd.pdd.work2  <- read_sas(paste0(secure.location,"rawOSHPD/PDD/pdd_work2.sas7bdat") )
  oshpd.pdd.work2  <- oshpd.pdd.work2 %>% rename(year = admtyr, age = agyrdsch, CCS=ccs_diagP)
  saveRDS(oshpd.pdd.work2, file = path(secure.location, "myData/oshpd_pdd_small.RDS"))

  
# ED - Emergency Department Data ---------------------------------

  # Now in SAS--------------------------------------------------------------------------------------------------------
  # etc.
  # ourVars <- c("dx_prin", "ccs_dx_prin", "patco", "sex", "race_grp",  "agyrserv", "dispn", "payer") # odx1 : odx24, 
  # etc.
  #-------------------------------------------------------------------------------------------------------------------
  

oshpd.ed.work  <- read_sas(paste0(secure.location,"rawOSHPD/ED/ed_work.sas7bdat") )
saveRDS(oshpd.ed.work, file = path(secure.location, "myData/oshpd_ed.RDS"))




# ------------------------------------------------------------------------------------------------
# -SAMPLE-----------------------------------------------------------------------------------------


#3% random sample, randomly permuted------------------------------------------------
set.seed(4)
#oshpd_sample <- sample_n(oshpd_subset, size = 0.01*nrow(oshpd_subset), replace = F)

sampN1 <- 0.01*nrow(oshpd.pdd.work1 )  
sampN2 <- sampN1*2

half1  <- sample_n(oshpd.pdd.work1 ,sampN1)  # sample function from dplyr

p1           <- sample_n(oshpd.pdd.work1 [,1:8],  sampN2)
p2           <- sample_n(oshpd.pdd.work1 [,9:10], sampN2)
p3           <- sample_n(oshpd.pdd.work1 [,11:16], sampN2); p3$race_grp  <- NA
p4           <- sample_n(oshpd.pdd.work1 [,17:41], sampN2)
p5           <- sample_n(oshpd.pdd.work1 [,42:65], sampN2)

half2        <- cbind(p1,p2,p3,p4,p5)

oshpd_sample <- rbind(half1,half2)

# Saving random sample as RDS file
saveRDS(oshpd_sample, file = path(upPlace, "upData/oshpd_pdd_SAMPLE.rds")) 



