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
secure.location  <- "G:/CCB/0.Secure.Data/"
secure.location  <- "H:/0.Secure.Data/"

myDrive <- getwd()  #Root location of CBD project
myPlace <- paste0(myDrive,"/myCBD") 
upPlace <- paste0(myDrive,"/myUpstream")

whichData <- "real"   # "real" or "fake"
newData   <- FALSE
 
yF    <- 100000  # rate constant 
STATE <- "CALIFORNIA"

#-------------------------------------------------LOAD PACKAGES -------------------------

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(haven)
library(fs)
library(readxl)
library(epitools)

#-----------------------------LOADING/CREATING OSHPD DATASET FROM ORIGINAL DATA FILE-----
#  oshpd16 <- readRDS(file=path(secure.location, "myData/oshpd_subset.rds")) 
  oshpd.ED.16 <- readRDS(file=path(secure.location, "myData/oshpd_ED_subset.rds")) 
 



##------------------------------------Reading in data mapping/linkage files--------------------

#reading in county-codes-to-names linkage files --oshpd codes map to column "cdphcaCountyTxt"
geoMap     <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/County Codes to County Names Linkage.xlsx")))

# sex categories
# bind_cols keeps values as characters, wherese cbind converts to factor, which we don't want
sex_num   <- c(   "1",      "2",     "3",       "4",    "5")
sex_cat   <- c("Male", "Female", "Other", "Unknown","Total")
OSHPD_sex <- bind_cols(sex_num=sex_num, sex_cat=sex_cat) %>% as.data.frame() 

#race categories
race_grp       <- c("0",          "1",        "2",        "3",    "4",        "5",       "6")
race_cat       <- c("Unk/missing","White-NH", "Black-NH", "Hisp", "Asian-NH", "AIAN-NH", "Other-NH")
OSHPD_race_grp <- bind_cols(race_grp=race_grp, race_cat=race_cat) %>% as.data.frame() 

ageMap     <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/Age Group Standard and US Standard 2000 Population.xlsx"),sheet = "data"))

#----------------------------------------------ADD AGE-GROUp VARIABLE---------------------------------------------------------#

aL            <-      ageMap$lAge     # lower age ranges
aU            <- c(-1,ageMap$uAge)    # upper age ranges, plus inital value of "-1" for lower limit
aLabs         <- paste(aL,"-",aU[-1]) # make label for ranges






















# ====== QUICK AND DIRTY ED PROCESSING=================================================================================


# TODO check age variable name is differnt that PDD

aMark             <- findInterval(oshpd.ED.16$agyrserv,aU,left.open = TRUE)  
oshpd.ED.16$ageG  <- aLabs[aMark] 
oshpd.ED.16       <- oshpd.ED.16 %>% mutate( ccsCode = str_pad(ccs_dx_prin, 5,"left",pad="o"))


# functions WITHOUT CHARGES----------------------------------------------------------------------------------------------

sum_num_costsX <- function(data, groupvar, levLab) {
  
  dat <- data %>% group_by_at(.,vars(groupvar)) %>% 
    summarize(n_hosp = n(), ) #adding median charge per day
}

#function to calculate crude hospitalization rates and charge-rates 
calculate_crude_ratesX <- function(data, yearN) {
  data %>% mutate(cHospRate = yF*n_hosp/(yearN*pop), 
                  hosp_rateLCI    = yF*pois.approx(n_hosp,yearN*pop, conf.level = 0.95)$lower,
                  hosp_rateUCI    = yF*pois.approx(n_hosp,yearN*pop, conf.level = 0.95)$upper)
}

# processing -----------------------------------------------------------

## TODO  county names different than PDD

oshpd.ED.16.Primary <- oshpd.ED.16 %>% #            select(-(ccs_odiag1:msdrg)) %>% 
   left_join(., select(geoMap,cdphcaCountyTxt,county=countyName), by = c("patco"= "cdphcaCountyTxt")) %>% 
   left_join(., OSHPD_race_grp, by = "race_grp") %>%
   left_join(., OSHPD_sex, by = c("sex" = "sex_num"))  %>% select(-sex) %>% mutate(sex=sex_cat)  %>% select(-sex_cat)

#Adding Total in order to create total/statewide estimates (for grouping function later)
oshpd.ED.16.Primary <- mutate(oshpd.ED.16.Primary, sex = "Total") %>% 
            bind_rows(oshpd.ED.16.Primary) 

# ----------- by AGE GROUP ------------------------------------------------------------------------------------

#NO YEAR FOR NOW
t.ED.age.state   <- sum_num_costsX(oshpd.ED.16.Primary, c("sex", "ageG","ccsCode"), "ccs0") %>% mutate(county=STATE)
t.ED.age.county  <- sum_num_costsX(oshpd.ED.16.Primary, c("sex", "ageG","ccsCode", "county"), "ccs0") 
popCountySexAgeG <- filter(popCountySexAgeG,year==2016)

t.ED.age        <- bind_rows(t.ED.age.state ,t.ED.age.county) %>% 
                     left_join(., popCountySexAgeG, by = c("sex", "ageG","county")) %>% 
                     as.data.frame()

ED.age    <- calculate_crude_ratesX(t.ED.age, yearN = 1)

saveRDS(ED.age, file = path(myPlace, "myData/",whichData,"/ED.age.rds"))

# ====== END QUICK AND DIRTY ED PROCESSING=================================================================================


