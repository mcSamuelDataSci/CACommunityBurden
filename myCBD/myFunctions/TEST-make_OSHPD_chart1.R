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


good_Yes <- readRDS(file = "D:/0.CBD/myCBD/myData/fake/goodOSHPD/full_oshpd_summary.rds")
good_No <- readRDS(file = "D:/0.CBD/myCBD/myData/fake/badOSHPD/full_oshpd_summary.rds")

table(good_Yes$diagnosis_var)
table(good_No$diagnosis_var)


names(good_Yes)



names(good_No)



goodX_Yes <- readRDS(file = "D:/0.CBD/myCBD/myData/fake/goodOSHPD/countyOSHPD.rds")
goodX_No <- readRDS(file = "D:/0.CBD/myCBD/myData/fake/badOSHPD/countyOSHPD.rds")





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


