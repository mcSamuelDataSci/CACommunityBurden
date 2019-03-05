# ====================================================================================================
# "OSHPD_PDD_2016_CA_hospital_discharge.R" file                                                      |
#                                                                                                    |
#            Reads in OSHPD 2016 PDD sas file, saves as rds file (random sample)                     |
#            Used to assess hospitalizations for diabetes--primary diagnoses and all diagnoses.      |
#                                                                                                    |
#                                                                                                    |   
# ====================================================================================================

#Loading packages necessary for this analysis
library(tidyverse)
library(ggplot2)
library(haven)
library(fs)
library(readxl)

#Creating 2016 OSHPD PDD dataset from OSHPD sas7bdat file, subsetting our variables of interest
#Note: This code should only be run once, to read in the original 2016 OSHPD PDD datafile and then create a subset with only the data of interest. This data will then be stored as an rds file in the subsequent code, which is what should be run in the future. 
myDrive <- getwd()  
myPlace <- paste0(myDrive,"/myCBD") 
upPlace <- paste0(myDrive,"/myUpstream")

#Reading in OSHPD sas file using haven, and selecting variables of interest. 
#Diagnosis Variables:
  # diag_p = primary diagnosis
# odiag1 - odiag24 = other diagnoses
# mdc = major diagnostic categories, formed by dividing all possible principal diagnoses into 25 mutually exclusive diagnosis groupings. Definitions of the 25 groups are located in the myInfo/App_I_MDC_PDD.xlsx file. 
#Cost-related Variables:
  # charge = 
  # pay_cat = type of entity or organization expected to pay the greatest share of the patient's bill
# 01 = Medicare
# 02 = Medi-Cal
# 03 = Private Coverage
# 04 = Workers' Compensation
# 05 = County Indigent Programs
# 06 = Other government
# 07 = Other indigent
# 08 = Self pay
# 09 = Other payer
# 00 = Invalid/blank
# pay_type = indicates type of coverage (not reported for other indigent, self pay, or other payer)
# 0 = Not Applicable
# 1 = Managed Care-Knox-Keene/Medi-Cal County Organized Health System
# 2 = Managed Care - Other
# 3 = Traditional Coverage
#Demographic Variables:
  # admtyr = year of admission
# patcnty = patient's county of residence (based on reported zip code)
# patzip = patient's reported zip code
# sex = patient's reported gender
# . = Invalid
# 1 = Male
# 2 = Female
# 3 = Other
# 4 = Unknown
# agyrdsch = age of the patient at discharge (based on reported discharge date and patient's date of birth. If DOB is unknown/invalid, age is set to 0; max age assigned is 120 years)
# race_grp = normalized race group for a patient based on a combination (merged) or their reported race and ethnicity. 
# 0 = Unknown/invalid/blank
# 1 = White
# 2 = Black
# 3 = Hispanic
# 4 = Asian/Pacific Islander
# 5 = Native American/Eskimo/Aleut
# 6 = Other

#reading in OSHPD 2016 PDD data, subsetting
oshpdHD0  <- read_sas("S:\\CDCB\\Demonstration Folder\\Data\\OSHPD\\PDD\\2016\\cdph_pdd_ssn2016.sas7bdat") 

#Subset with only variables of interest
oshpd16_subset  <- select(oshpdHD0,diag_p, odiag1, odiag2, odiag3, odiag4, odiag5, odiag6, odiag7, odiag8, odiag9, odiag10, odiag11, odiag12, odiag13, odiag14, odiag15, odiag16, odiag17, odiag18, odiag19, odiag20, odiag21, odiag22, odiag23, odiag24, mdc, charge, pay_cat, pay_type, admtyr, patcnty, patzip, sex, agyrdsch, race_grp)

#1% random sample
oshpd16_sample <- sample_n(oshpd16_subset, size = 0.01*nrow(oshpd16_subset), replace = F)

#Now, create RDS file of whole SAS file and random sample of SAS file 

#saving rds file--only needs to be run once to initially create the file

#Saving 1% random sample as RDS file
saveRDS(oshpd16_sample, file = path(upPlace, "upData/oshpd16_sample.rds"))

#Saving subset as RDS file
saveRDS(oshpd16_subset, file=path(upPlace,"upData/oshpdHD2016_subset.rds"))

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


cols <- c(oshpd16_sample$diag_p, oshpd16_sample$odiag1, oshpd16_sample$odiag2)

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




#case_when(
  diag_p | odiag1 | odiag2 == diabetes ~ "1"
)

































#MDC charge analysis
#Groups by mdc variable and counts the number of hospitalizations with that mdc as well as the total charge for each mdc
tWork   <- oshpd16_subset %>% mutate(mdc = as.numeric(mdc)) %>%
  group_by(mdc) %>%
  summarise(tCount = n(),
            tBucks = sum(charge)) %>%
  gather(tCount, tBucks, key = "count_charges", value = "number")

#This is the codebook for MDC numbers and the associated condiitons
oshpdMdcMap <- read_excel(path(myPlace,"myInfo/App_I_MDC_PDD.xlsx"), sheet="v32.0 ",skip=4)
#Renaming columns 1 and 2
names(oshpdMdcMap)[1] <- "mdc"
names(oshpdMdcMap)[2] <- "MDC"

#Joining tWork dataset above with the MDC codebook, removing the mdc column, saving as dataframe
tWork <- full_join(oshpdMdcMap,tWork,by="mdc")
tWork <- tWork %>% select(-mdc)
tWork <- as.data.frame((tWork))


#plotting the number of visits and the total charges for each MDC
ggplot(data = tWork, aes(x = MDC, y = number, fill = count_charges)) +  coord_flip() +
  geom_bar(stat = "identity" ) + 
  facet_grid(. ~ count_charges,scales="free_x")






