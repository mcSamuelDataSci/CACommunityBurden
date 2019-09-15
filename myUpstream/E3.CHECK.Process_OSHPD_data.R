#---SET LOCATIONS-----------------------------------------------------------------------

# PROVIDE PATH FOR SECURE DATA HERE
# secure.location  <- "S:/CDCB/Demonstration Folder/Data/OSHPD/PDD/2016/"  # secure location of data
secure.location  <- "g:/0.Secure.Data/"

myDrive <- getwd()  #Root location of CBD project
myPlace <- paste0(myDrive,"/myCBD") 
upPlace <- paste0(myDrive,"/myUpstream")

whichData <- "real"   # "real" or "fake"
newData  <- FALSE


#-------------------------------------------------LOAD PACKAGES -----------------------------------------------------------------------------------------------------------------------------------#

library(tidyverse)
library(haven)
library(fs)
library(readxl)
library(epitools)

#--------------------------------------------------------------------LOAD AND PROCESS OSHPD DATA-----------------------------------------------------------------------------------------#

if (whichData == "real") {
  oshpd16 <- readRDS(file=path(secure.location, "myData/oshpd_subset.rds")) 
}

if (whichData == "fake") {
  oshpd16 <- readRDS(file=path(upPlace, "upData/oshpd16_sample.rds"))
}


#  CHECK CHARGES AND DAYS ======================================================================================
# ==============================================================================================================


# -MCS exploring summary funtions
library(summarytools)
# dfSummary(oshpd16)
summarytools::descr(oshpd16)

# --------------------------------------------------------------------------------------------------------------

charges <- oshpd16$charge

tempCharges  <- table(charges)
round(100* tempCharges[1:10] / sum(tempCharges) , 6)  
tempCharges[1:20] 

chargesCUT <- charges[charges > 0 & charges <100000]
hist(chargesCUT,breaks=1000,main="Charges >0 & <100,000")

chargesCUT <- charges[charges >100000 & charges < 500000 ]
hist(chargesCUT,breaks=1000,main="Charges >0 & <100,000")

nMax <- length(tempCharges)
round(100* tempCharges[(nMax-10):nMax] / sum(tempCharges) , 6)  
tempCharges[(nMax-10):nMax] 


summarytools::descr(chargesCUT)


# OSHPD CHARGE AND CHARGE_PER_TDAY 0 and 1 to NA 
# ospitals that don't report charges (eg Kaiser) are assigned charges of 0, 
# pro bono cases as assigned charges of 1

#------------------LENGTH OF STAY-----------------------------------#

los <- oshpd16$los
tempLOS <- table(los)
round(100* tempLOS[1:15] / sum(tempLOS) , 2)


los_adj <- oshpd16$los_adj
tempLOS <- table(los_adj)
round(100* tempLOS[1:15] / sum(tempLOS) , 2)

losCUT <- los_adj[los_adj > 30 & los_adj < 365 ]
hist(losCUT,breaks=1000,main="los  > 30 & <365")




oshpd16 %>% filter(los_adj > 100) %>% nrow() #11132 greater than 100 days
oshpd16 %>% filter(los_adj > 200) %>% nrow() #4074 greater than 200 days
oshpd16 %>% filter(los_adj > 365) %>% nrow() #2046 greater than 1 year
oshpd16 %>% filter(los_adj > 1000) %>% nrow() #680 greater than 1000 days
oshpd16 %>% filter(los_adj > 3650) %>% nrow() #48 greater than 10 years 


#What should the exclusion cut-off be for los_adj? 365 days? Less than that? 

#----------------------------------------------------------------------------------------------------------------------------------------------#
#Some of these extreme los/charges may not even apply to the CAUSE/icdCodes that we're capturing though. Now only looking at values for our CAUSES of interest
