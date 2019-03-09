# ====================================================================================================
# "E3.Process_OSHPD_data.R" file                                                      |
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

diabetes <- icd_map[110, "regEx_ICD10_CM"] #this pulls out the information from row 110, column 14 of icd_map dataframe, which is the regEx for ICD-10-CM for diabetes.
#We will use this condition to define diabetes 


#--Writing function to create indicator variable for different conditions based on diagnosis codes-----------------------------

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

oshpd_sample <- diagnosis_definition(oshpd16_sample, "diabetes_p", diabetes, index_p) %>% diagnosis_definition(., "diabetes_any", diabetes, index_any)








