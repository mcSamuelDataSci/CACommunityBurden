#OSHPD Chart 1 = ggplot version

library(fs)
library(dplyr)

myCounty = "CALIFORNIA"
myOSHPDtype = "Number of Hospitalizations"
mySex = "Total"
myN = 10
myVar = "icd10_cm"
myVar = "mdc"
myVar = "drg"



myPlace           <- "f:/0.CBD/myCBD"
full_oshpd_summary <- readRDS(file = path(myPlace, "myData/real", "full_oshpd_summary.rds"))


nrow(full_oshpd_summary)
# 135083

myOSHPDtype_N_cause <- full_oshpd_summary %>%
  filter(diagnosis_var == "drg" | diagnosis_var == "mdc" | Level == "lev2")  %>%
  filter(!is.na(CAUSE), county == myCounty, sex == mySex, diagnosis_var == myVar)

myOSHPDtype_N_cause
# empty....




full_oshpd_summary <- readRDS(file = path(myPlace, "myData/fake", "full_oshpd_summary.rds"))
nrow(full_oshpd_summary)

# 364139


