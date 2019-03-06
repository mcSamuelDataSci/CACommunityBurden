# ====================================================================================================
# "OSHPD_PDD_2016_CA_hospital_discharge.R" file                                                      |
#                                                                                                    |
#            Reads in OSHPD 2016 PDD sas file, saves as rds file (random sample)                     |
#            Used to assess hospitalizations for diabetes--primary diagnoses and all diagnoses.      |
#                                                                                                    |
#                                                                                                    |   
# ====================================================================================================

#-- Set Locations Etc-----------------------------------------------------------------------

# PROVIDE PATH FOR SECURE DATA HERE
secure.location  <- "G:/CCB/0.Secure.Data/"  # secure location of data
.sl              <- secure.location  # short name to shorten lines of code below

myDrive <- getwd()  #Root location of CBD project
myPlace <- paste0(myDrive,"/myCBD") 
upPlace <- paste0(myDrive,"/myUpstream")

#-- Load Packages -------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(haven)
library(fs)
library(readxl)

#Reading in oshpd 2016 PDD file
oshpd16  <- read_sas("S:\\CDCB\\Demonstration Folder\\Data\\OSHPD\\PDD\\2016\\cdph_pdd_ssn2016.sas7bdat") 


#Subset with only variables of interest
oshpd_subset  <- select(oshpd16,diag_p, odiag1, odiag2, odiag3, odiag4, odiag5, odiag6, odiag7, odiag8, odiag9, odiag10, odiag11, odiag12, odiag13, odiag14, odiag15, odiag16, odiag17, odiag18, odiag19, odiag20, odiag21, odiag22, odiag23, odiag24, mdc, charge, pay_cat, pay_type, admtyr, dschdate, patcnty, patzip, sex, agyrdsch, race_grp) %>%
  mutate(year = 2016)

#Saving subset as RDS file
saveRDS(oshpd_subset, file=path(.sl, "oshpd_subset.rds")) #I don't have access to G:/CCB/0.Secure/Data

#1% random sample
set.seed(4)
oshpd_sample <- sample_n(oshpd_subset, size = 0.01*nrow(oshpd_subset), replace = F)

#Now, create RDS file of whole SAS file and random sample of SAS file 

#saving rds file--only needs to be run once to initially create the file

#Saving 1% random sample as RDS file
saveRDS(oshpd16_sample, file = path(upPlace, "upData/oshpd16_sample.rds"))



#***************************************************************************************************************#
#Start code here if OSHPD 2016 subset has already been created:
#***************************************************************************************************************#


#loading oshpd rds file into R
oshpd16_subset <- readRDS(file=path(upPlace,"upData/oshpdHD2016_subset.rds")) 
oshpd16_sample <- readRDS(file=path(upPlace, "upData/oshpd16_sample.rds"))


#OSHPD Hospitalization primary and any diagnoses analysis

#reading in gbd.ICD.excel file}
icd_map <- read_excel(path(myPlace, "myInfo/gbd.ICD.Map.xlsx"))

diabetes <- icd_map[110, 14] #this pulls out the information from row 110, column 14 of icd_map dataframe, which is the regEx for ICD-10-CM for diabetes.
#We will use this condition to define diabetes 


#Defining diabetes_primary and diabetes_any variables (using sample dataset)
oshpd_test3 <- mutate(oshpd16_sample, diab_primary = ifelse(grepl(diabetes, diag_p), "1", "0"),
                      diab_any = ifelse(grepl(diabetes, diag_p) | grepl(diabetes, odiag1)|
                                          grepl(diabetes, odiag2)| grepl(diabetes, odiag3)|
                                          grepl(diabetes, odiag4)|
                                          grepl(diabetes, odiag5)|
                                          grepl(diabetes, odiag6)|
                                          grepl(diabetes, odiag7)|
                                          grepl(diabetes, odiag8)|
                                          grepl(diabetes, odiag9)|
                                          grepl(diabetes, odiag10)|
                                          grepl(diabetes, odiag11)|
                                          grepl(diabetes, odiag12)|
                                          grepl(diabetes, odiag13)|
                                          grepl(diabetes, odiag14)|
                                          grepl(diabetes, odiag15)|
                                          grepl(diabetes, odiag16)|
                                          grepl(diabetes, odiag17)|
                                          grepl(diabetes, odiag18)|
                                          grepl(diabetes, odiag19)|
                                          grepl(diabetes, odiag20)|
                                          grepl(diabetes, odiag21)|
                                          grepl(diabetes, odiag22)|
                                          grepl(diabetes, odiag23)|
                                          grepl(diabetes, odiag24), "1", "0"))

#Writing functions to make the above code simpler/more reproducible:

#apply(X, Margin, function, ...) X = an array, inclduing a matrix, Margin = vector giving the subscripts which the function will
#be applied over. E.g. 1 indicates rows, 2 indicates columns, c(1,2) indicates rows and columns. Since we want the function
#applied over rows (for multiple columns), we'll specify 1. 

oshpd16_sample$new <- apply(oshpd16_sample, 1, FUN = function(x) {
  pattern <- grepl(diabetes, x)
  if(any(pattern[(1)])) "1" else "0"
} )

oshpd16_sample$any <- apply(oshpd16_sample, 1, FUN = function(x) {
  pattern <- grepl(diabetes, x)
  if(any(pattern[(1:25)])) "1" else "0"
} )

filter(oshpd16_sample, new == 1) %>% nrow()

filter(oshpd16_sample, any == 1) %>% nrow()

#Number of diabetes primary diagnosis visits:
filter(oshpd_test3, diab_primary == "1") %>% nrow()

#Number of diabetes any diagnosis visits:
filter(oshpd_test3, diab_any == "1") %>% nrow()





























