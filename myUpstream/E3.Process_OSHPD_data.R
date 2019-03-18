# ====================================================================================================
# "E3.Process_OSHPD_data.R" file                                                      |
#                                                                                                    |
#            Reads in OSHPD 2016 PDD sas file, saves as rds file (random sample)                     |
#            Used to assess hospitalizations for diabetes--primary diagnoses and all diagnoses.      |
#                                                                                                    |
#                                                                                                    |   
# ====================================================================================================

# fingers crossed

#-- Set Locations Etc-----------------------------------------------------------------------

# PROVIDE PATH FOR SECURE DATA HERE
secure.location  <- "S:/CDCB/Demonstration Folder/Data/OSHPD/PDD/2016/"  # secure location of data
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
saveRDS(oshpd_subset, file=path(.sl, "oshpd_subset.rds"))

#1% random sample
set.seed(4)
oshpd_sample <- sample_n(oshpd_subset, size = 0.01*nrow(oshpd_subset), replace = F)

#Now, create RDS file of whole SAS file and random sample of SAS file 

#saving rds file--only needs to be run once to initially create the file

#Saving 1% random sample as RDS file
saveRDS(oshpd_sample, file = path(upPlace, "upData/oshpd16_sample.rds"))



#***************************************************************************************************************#
#Start code here if OSHPD 2016 subset has already been created:
#***************************************************************************************************************#


if (fullOSHPD) {
  oshpd16 <- readRDS(file=path(upPlace,"upData/oshpdHD2016_subset.rds")) #maybe change to secure location?
}

if (sampleOSHPD) {
  oshpd16 <- readRDS(file=path(upPlace, "upData/oshpd16_sample.rds"))
}

##------------------------------------Reading in gbd.ICD.excel file and defining variable conditions to be used in function--------------------#
#reading in gbd.ICD.excel file}
icd_map <- read_excel(path(myPlace, "myInfo/gbd.ICD.Map.xlsx"))

#Does it make sense to incorporate code similar to what was used for the death data that assesses variables based on CODE/LABEL levels?
#levels, rather than individual names? 



icd_map <- icd_map %>% select(name, CODE, LABEL, ICD10_CM, regExICD10_CM)

diabetes <- icd_map %>% filter(name == "C. Diabetes mellitus") %>% select(regExICD10_CM)

depression <- icd_map %>% filter(name == "a. Major depressive disorder" | name == "b. Dysthymia") %>% select(regEx10)
depression <- paste(depression[1,], depression[2,], sep = "|") %>% as.data.frame() #if we are including major depressive disorder and dysthmia
#together as one group, then we need to paste the regEx from the two conditions together

ischaemic_heart_disease <- icd_map %>% filter(name == "3. Ischaemic heart disease") %>% select(regEx10)

#--------------------------------------------------Writing function to create indicator variable for different conditions based on diagnosis codes-----------------------------

#dataset = dataset of interest (in this case, oshpd16_sample)
#colname = what we want to name column, based on disease and whether diagnosis is based only on primary or any of 25 diagnosis codes (e.g. diabetes_any)
#icd_regEx = regEx for disease of interest, as defined in gdb.ICD.Map.xlsx
#index = variable indicating index we've defined: either 1 for diag_p (only primary diagnosis) or 1:25 for diag_p-odiag25 (any diagnosis code)
#index variables will have to be defined prior to running function--although this makes the code not quite "self-annotated", R
#doesn't seem to allow calling an index based on a range of variable names within a data.frame

#apply(X, Margin, function, ...) X = an array, inclduing a matrix, Margin = vector giving the subscripts which the function will
#be applied over. E.g. 1 indicates rows, 2 indicates columns, c(1,2) indicates rows and columns. Since we want the function
#applied over rows (for multiple columns), we'll specify 1. 

diagnosis_definition <- function(dataset, col_name, icd_regEx, index) {
  dataset[[col_name]] <- apply(dataset, 1, FUN = function(x) {
    pattern <- grepl(icd_regEx, x)
    if(any(pattern[(index)])) "1" else "0"
  } )
  dataset
}
#index_p = only primary diagnosis
index_p <- 1
#index_any = any diagnosis
index_any <- 1:25

oshpd_sample2 <- diagnosis_definition(oshpd16, "diabetes_p", diabetes, index_p) %>% diagnosis_definition(., "diabetes_any", diabetes, index_any)








