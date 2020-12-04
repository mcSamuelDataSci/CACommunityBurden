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



#-----------------------------------------------------------------------------------------

oshpd.ED.18  <- read_sas(paste0(secure.location,"rawOSHPD/ED/ed_work.sas7bdat") ) 

oshpd.ED.18  <- read_sas(paste0(secure.location,"rawOSHPD/ED/ed_2018.sas7bdat") ) 

write_rds(oshpd.ED.18, path = path(secure.location, "myData/test1.rds"))
saveRDS(oshpd.ED.18, file = path(secure.location, "myData/test2.rds"))
save(oshpd.ED.18, file = path(secure.location, "myData/test3.RData"))






#-----------------------------LOADING/CREATING OSHPD DATASET FROM ORIGINAL DATA FILE-----

# PDD - Patient Discharge Data ---------------------------------

  # Now done in SAS
  #------------------------------------------------------------------------------------------------------------------
  # variables to use
  # ourVars  <- c("diag_p", "ccs_diagP", "mdc", "msdrg", "charge", 
  #               "pay_cat", "pay_type", "admtyr","patcnty", "patzip", "sex", "agyrdsch", "race_grp", "oshpd_id", 
  #               "los_adj", "los") # dschdate,
  # 
  # oshpd.PDD.16.0  <- read_sas(paste0(secure.location,"rawOSHPD/PDD/cdph_pdd_rln2016.sas7bdat") ) 
  # oshpd.PDD.16    <- oshpd.PDD.16.0 %>% select(ourVars,contains("ccs_odiag"),contains("odiag")) %>% mutate(year=2016)
  # 
  
  # oshpd.PDD.16  <- read_sas(paste0(secure.location,"rawOSHPD/PDD/pdd_2016.sas7bdat") ) %>% mutate(year=2016)
  # oshpd.PDD.17  <- read_sas(paste0(secure.location,"rawOSHPD/PDD/pdd_2017.sas7bdat") ) %>% mutate(year=2017)
  # oshpd.PDD.18  <- read_sas(paste0(secure.location,"rawOSHPD/PDD/pdd_2018.sas7bdat") ) %>% mutate(year=2018)
  # 
  # oshpd_subset  <- bind_rows(oshpd.PDD.16, oshpd.PDD.17,oshpd.PDD.18)
  #-------------------------------------------------------------------------------------------------------------------
  

  # Saving subset as RDS file
  oshpd.pdd.work  <- read_sas(paste0(secure.location,"rawOSHPD/PDD/pdd_2016.sas7bdat") )


  saveRDS(oshpd_subset, path=path(secure.location, "myData/oshpd_subset.RDS"))
  
  # FOR AGE/RACE Focus charts
  oshpd_subset0    <- oshpd_subset %>% 
                       select(year, patcnty, patzip, sex, Age=agyrdsch, race_grp, CCS=ccs_diagP, diag_p)
  
  write_rds(oshpd_subset0, path = path(secure.location, "myData/oshpd_subset0.RDS"))

  
# ED - Emergency Department Data ---------------------------------


# now in SAS
# ourVars <- c("dx_prin", "ccs_dx_prin", "patco", "sex", "race_grp",  "agyrserv", "dispn", "payer") # odx1 : odx24, 
# oshpd.ED.16  <- oshpd.ED.16.0  %>% mutate(year=2016) %>% select(ourVars)
# saveRDS(oshpd.ED.16, file=path(secure.location, "myData/oshpd.ED.16.rds"))


oshpd.ED.16  <- read_sas(paste0(secure.location,"rawOSHPD/ED/ed_2016.sas7bdat") ) %>% mutate(year=2016)
oshpd.ED.17  <- read_sas(paste0(secure.location,"rawOSHPD/ED/ed_2017.sas7bdat") ) %>% mutate(year=2017)
oshpd.ED.18  <- read_sas(paste0(secure.location,"rawOSHPD/ED/ed_2018.sas7bdat") ) %>% mutate(year=2018)

oshpd_ED_subset <- bind_rows(oshpd.ED.16, oshpd.ED.17, oshpd.ED.18)

## TODO could/should standardize all OSHPD names in this step,  eg  countyCode = patco

#write_rds(oshpd_ED_subset, path = path(secure.location, "myData/oshpd_ED_subset.rds"))

oshpd.ED.work  <- read_sas(paste0(secure.location,"rawOSHPD/ED/ed_work.sas7bdat") ) 

saveRDS(oshpd.ED.18, file= path(secure.location, "myData/oshpd_ED_subset2.rds"))
saveRDS(oshpd.ED.work, file= path(secure.location, "myData/oshpd_ED_subset.rds"))


# ------------------------------------------------------------------------------------------------
# -SAMPLE-----------------------------------------------------------------------------------------


#3% random sample, randomly permuted------------------------------------------------
set.seed(4)
#oshpd_sample <- sample_n(oshpd_subset, size = 0.01*nrow(oshpd_subset), replace = F)

sampN1 <- 0.01*nrow(oshpd_subset)  
sampN2 <- sampN1*2

half1  <- sample_n(oshpd_subset,sampN1)  # sample function from dplyr

p1           <- sample_n(oshpd_subset[,1:8],  sampN2)
p2           <- sample_n(oshpd_subset[,9:10], sampN2)
p3           <- sample_n(oshpd_subset[,11:16], sampN2); p3$race_grp  <- NA
p4           <- sample_n(oshpd_subset[,17:41], sampN2)
p5           <- sample_n(oshpd_subset[,42:65], sampN2)

half2        <- cbind(p1,p2,p3,p4,p5)

oshpd_sample <- rbind(half1,half2)

# Saving random sample as RDS file
saveRDS(oshpd_sample, file = path(upPlace, "upData/oshpd_subset_SAMPLE.rds")) 



