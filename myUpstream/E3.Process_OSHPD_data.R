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
STATE <- "California"

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

# PDD - Patient Discharge Data ---------------------------------

if(newData) {
  
  #Reading in oshpd 2016 PDD file
  oshpd16  <- read_sas(paste0(secure.location,"rawOSHPD/PDD/cdph_pdd_rln2016.sas7bdat") )
  
  #Subset with only variables of interest
  oshpd_subset  <- select(oshpd16,
                          diag_p, 
                          # contains("odiag"),
                          ccs_diagP,
                          contains("ccs_odiag"),
                          mdc, msdrg, charge, 
                          pay_cat, pay_type, admtyr,patcnty, patzip, sex, agyrdsch, race_grp, oshpd_id, # dschdate,
                          los_adj, los) %>% mutate(year = 2016)
  
  
  #Saving subset as RDS file
  saveRDS(oshpd_subset, file=path(secure.location, "myData/oshpd_subset.rds"))
  
  #3% random sample, randomly permuted------------------------------------------------
  set.seed(4)
  #oshpd_sample <- sample_n(oshpd_subset, size = 0.01*nrow(oshpd_subset), replace = F)
  
  sampN1 <- 0.01*nrow(oshpd_subset)  
  sampN2 <- sampN1*2
  
  half1  <- sample_n(oshpd_subset,sampN1)  # sample function from dplyr
  
  p1           <- sample_n(oshpd_subset[,1:32],  sampN2)
  p2           <- sample_n(oshpd_subset[,33:34], sampN2)
  p3           <- sample_n(oshpd_subset[,35:41], sampN2)
  p3$race_grp  <- NA
  half2        <- cbind(p1,p2,p3)
  
  oshpd_sample <- rbind(half1,half2)
  
  # Saving random sample as RDS file
  saveRDS(oshpd_sample, file = path(upPlace, "upData/oshpd16_sample.rds")) #----------
  
} # END if(newData)


# ED - Emergency Department Data ---------------------------------


if(newData) {

oshpd.ED.16  <- read_sas(paste0(secure.location,"rawOSHPD/cdph_ed_rln2016.sas7bdat") )

oshpd_ED_subset  <- oshpd.ED.16 %>%
                    select(dx_prin, odx1 : odx24,  ccs_dx_prin, patco, race_grp,  agyrserv, dispn, payer) 

saveRDS(oshpd_ED_subset, file=path(secure.location, "myData/oshpd_ED_subset.rds"))
}

#-------------------------------------LOAD AND PROCESS OSHPD DATA-------------------------

if (whichData == "real") {
  oshpd16 <- readRDS(file=path(secure.location, "myData/oshpd_subset.rds")) 
}

if (whichData == "fake") {
  oshpd16 <- readRDS(file=path(upPlace, "upData/oshpd16_sample.rds"))
}


##------------------------------------Reading in data mapping/linkage files--------------------

# reading in gbd.ICD.excel file
# icd_map <- read_excel(path(myPlace, "myInfo/gbd.ICD.Map.xlsx")) 

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
aMark         <- findInterval(oshpd16$agyrdsch,aU,left.open = TRUE)  # vector indicating age RANGE value of each INDIVIDUAL age value
oshpd16$ageG  <- aLabs[aMark] 


#-------------------------------------LOAD AND PROCESS POPULATION DATA------------------------------

# ungrouping important for subsequent data set merging
popCounty        <- readRDS(path(upPlace,"/upData/popCounty.RDS")) %>% ungroup() 
popCountySex     <- filter(popCounty,ageG == "Total")
popCountySexAgeG <- filter(popCounty,ageG != "Total")

popStandard         <- ageMap %>% mutate(ageG = paste0(lAge," - ",uAge))


# CCS CODING ---------------------------------------------------------------------

# WARNING - need to change these column numbers if osdpd16 changes


# Adding "o" pads before numeric CCS codes for consistant matching/linking/coding

oshpd16[,2:26]  <-  apply(oshpd16[,2:26],2, function(x) str_pad(x, 5,"left",pad="o"))

ccsMap           <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/CCS Code and Names Linkage.xlsx"),sheet = "names")) %>%
                       mutate(ccsCodePaded = str_pad(ccsCode, 5,"left",pad="o"))

# Create one variable with all 25 CCS codes concatenated for efficient string searching
oshpd16Any <- oshpd16 %>% 
               mutate(all_CCS_One_String = 
                       paste(ccs_diagP,    ccs_odiag1,  ccs_odiag2, ccs_odiag3, ccs_odiag4,
                             ccs_odiag5,   ccs_odiag6,  ccs_odiag7, ccs_odiag8, ccs_odiag9,
                             ccs_odiag10, ccs_odiag11, ccs_odiag12, ccs_odiag13, ccs_odiag14,
                             ccs_odiag15, ccs_odiag16, ccs_odiag17, ccs_odiag18, ccs_odiag19, 
                             ccs_odiag20, ccs_odiag21, ccs_odiag22, ccs_odiag23, ccs_odiag24))  

# TODO
# learning about OSHPD data
# no ccs code "above" 670 appears in the data set 

start_time <- Sys.time()

# Create (283) new indicator varibles, one for each CCS code, indicting if that code occurrs in the
#  primary CCS code of any of the 24 secondary CCS codes

for (i in 1:nrow(ccsMap)) {
oshpd16Any <- oshpd16Any %>% mutate(
   !!ccsMap$ccsCodePaded[i] := ifelse(grepl(ccsMap$ccsCodePaded[i], all_CCS_One_String), 1, 0)) 
 # !!ccsMap$ccsCodePaded[i] := ifelse(apply(oshpd16_new[,2:26], 1, function(r) any(r == ccsMap$ccsCodePaded[i])), 1, 0)) #SLOWER 
}
end_time <- Sys.time()
end_time - start_time

# discard bunch of variable for efficient processing
oshpd16Any <- select(oshpd16Any,-(ccs_odiag1:msdrg),-all_CCS_One_String)

# save/read this file
saveRDS(oshpd16Any, file=path(secure.location, "myData/oshpd16Any.rds"))
oshpd16Any <- readRDS(file=path(secure.location, "myData/oshpd16Any.rds")) 


# Sum each CCS indicator variable, by county and sex, and "flip" it (i.e. wide to long) such that there is one
#  varible ("ccsANY") with the count for that CCS code, and one varible ("ccsMeasure") that indicates the CCS code

## NOT ENOUGH MEMORY(?) TO DO THIS
# temp <- pivot_longer(oshpd16Any,ooo1:o237,names_to = "ccsANY")

i <- 1
## get function!
ccs.t0 <- oshpd16Any %>% 
            group_by(patcnty, sex) %>% 
            summarize(n_hosp_any = sum(get(ccsMap$ccsCodePaded[i]))) %>% 
            mutate(ccsCode=ccsMap$ccsCodePaded[i])

for (i in 2:nrow(ccsMap)) {
ccs.t1 <- oshpd16Any %>% 
           group_by(patcnty, sex) %>% 
           summarize(n_hosp_any = sum(get(ccsMap$ccsCodePaded[i]))) %>% 
           mutate(ccsCode=ccsMap$ccsCodePaded[i])

ccs.t0 <- bind_rows(ccs.t0,ccs.t1)
}

# make "Total Sex" copy and join
ccs.t2      <- ccs.t0 %>% 
                    group_by(patcnty, ccsCode) %>% 
                    summarise(n_hosp_any=sum(n_hosp_any)) %>%   
                    mutate(sex = "5") %>%  
                    bind_rows(ccs.t0)

# make "CA State" copy and join
ccs.t3     <- ccs.t2 %>% 
                   group_by(ccsCode,sex) %>% 
                   summarise(n_hosp_any=sum(n_hosp_any)) %>%   
                   mutate(patcnty = "CA")  %>%   
                   bind_rows(ccs.t2)

## TODO -- why any missing sex data?

ccsAnyWork <- ccs.t3 %>%
                left_join(., OSHPD_sex, by = c("sex" = "sex_num"))  %>% 
                left_join(., select(geoMap,cdphcaCountyTxt,county=countyName), by = c("patcnty"= "cdphcaCountyTxt")) %>% 
                mutate(county  = ifelse(patcnty == "CA","CALIFORNIA",county)) %>%
                select(-sex) %>% 
                mutate(sex=sex_cat)  %>% select(-sex_cat) %>%
                filter(sex %in% c("Male","Female","Total")) %>%
                filter(n_hosp_any > 0)


saveRDS(ccsAnyWork, file = path(myPlace, "myData/",whichData,"/oshpd_PDD_any.rds"))
ccsAnyWork <- readRDS(file = path(myPlace, "myData/",whichData,"/oshpd_PDD_any.rds"))


# =====================================================================================================================
# ---------------------------------------------------------------------------------------------------------------------


oshpd16Primary <- oshpd16 %>% 
                     select(-(ccs_odiag1:msdrg)) %>% 
   left_join(., select(geoMap,cdphcaCountyTxt,county=countyName), by = c("patcnty"= "cdphcaCountyTxt")) %>% 
   left_join(., OSHPD_race_grp, by = "race_grp") %>%
   left_join(., OSHPD_sex, by = c("sex" = "sex_num"))  %>% select(-sex) %>% mutate(sex=sex_cat)  %>% select(-sex_cat)



# NOTE: warning here, that columns have "different attributes on LHS and RHS of join" if just becuase of the variable label that
#  comes along with reading the file from SAS -- not a problem

# -MCS  I removed mutate(race_grp = as.factor(race_grp)) %>%, and made the mapping dfs character above....
# -MCS  move sex and race recoding above....

#Adding Total in order to create total/statewide estimates (for grouping function later)
oshpd16Primary <- mutate(oshpd16Primary, sex = "Total") %>% 
            bind_rows(oshpd16Primary) 

#Calculating charge/day from los_adj and charges

oshpd16Primary <- oshpd16Primary %>% mutate(charge_per_day = charge/los_adj)

###-------------------------------------------EXPLORATORY ANALYSIS OF LENGTH OF STAY AND CHARGES (REAL OSHPD DATA, NOT SAMPLE)----------------------------------------------#

# TODO
# what do charges 2 and 3 mean? Are they also some sort of pro bono/not real charges info? 

#-----------------------------*_*_*_*_*_*_*_*_*_*_

oshpd16Primary$charge[oshpd16Primary$charge == 0] <- NA #changing 0 and 1 charges (kaiser or pro-bono cases) to NA
oshpd16Primary$charge[oshpd16Primary$charge == 1] <- NA
oshpd16Primary$charge_per_day[oshpd16Primary$charge_per_day == 0] <- NA #changing 0 and 1 charges (kaiser or pro-bono cases) to NA
oshpd16Primary$charge_per_day[oshpd16Primary$charge_per_day == 1] <- NA


#-------------OSHPD CALCULATIONS FOR TOTAL VISITS/CHARGES AND CRUDE RATES------------------------------------------------------------------#

# MCS
# https://dplyr.tidyverse.org/reference/scoped.html
# Group_by_at
# Function to sum number of hospitalizations and charges, calculate mean charges, mean length of stay, and mean charge per day


sum_num_costs <- function(data, groupvar, levLab) {
  
  dat <- data %>% group_by_at(.,vars(groupvar)) %>% 
    summarize(n_hosp = n(), 
              charges = sum(charge, na.rm = TRUE), #this still converts cases where there was only 1 with NA charges to 0 for charges
              avgcharge = mean(charge, na.rm = TRUE),
              medcharge = median(charge, na.rm = TRUE), #adding median charge to compare to average charge
              avg_los = mean(los_adj, na.rm = TRUE),
              avgcharge_per_day = mean(charge_per_day, na.rm = TRUE),
              medcharge_per_day = median(charge_per_day, na.rm = TRUE)) #adding median charge per day
  
  names(dat)[grep("lev", names(dat))] <- "CAUSE"
  dat$Level                           <- levLab
  dat$charges[dat$charges == 0] <- NA
  dat %>% data.frame()
  
}

#lev1 = Top level
#lev2 = public health level

#function to calculate crude hospitalization rates and charge-rates 
calculate_crude_rates <- function(data, yearN) {
  data %>% mutate(cHospRate = yF*n_hosp/(yearN*pop), 
                  hosp_rateLCI    = yF*pois.approx(n_hosp,yearN*pop, conf.level = 0.95)$lower,
                  hosp_rateUCI    = yF*pois.approx(n_hosp,yearN*pop, conf.level = 0.95)$upper,
                  cChargeRate     = yF*charges/(yearN*pop),
                  charge_rateLCI  = yF*pois.approx(charges,yearN*pop, conf.level = 0.95)$lower,
                  charge_rateUCI  = yF*pois.approx(charges,yearN*pop, conf.level = 0.95)$upper)
}

# function to calculate age-adjusted hospitalization rates
# https://github.com/cran/epitools/blob/master/R/ageadjust.direct.R

ageadjust.direct.SAM <- function (count, pop, rate = NULL, stdpop, conf.level = 0.95) 
{
  if (missing(count) == TRUE & !missing(pop) == TRUE & is.null(rate) == TRUE)   count <- rate * pop
  if (missing(pop) == TRUE & !missing(count) == TRUE & is.null(rate) == TRUE)     pop <- count/rate
  if (is.null(rate) == TRUE & !missing(count) == TRUE & !missing(pop) == TRUE)  rate <- count/pop
  
  rate[is.na(pop)]   <- 0
  rate[is.null(pop)] <- 0
  pop[is.na(pop)]    <- 0
  pop[is.null(pop)]  <- 0
  
  alpha <- 1 - conf.level
  cruderate <- sum(count,na.rm=TRUE)/sum(pop,na.rm=TRUE)
  stdwt <- stdpop/sum(stdpop,na.rm=TRUE)
  dsr <- sum(stdwt * rate,na.rm=TRUE)
  dsr.var <- sum((stdwt^2) * (count/pop^2))
  dsr.se  <- sqrt(dsr.var)
  wm<- max(stdwt/pop)
  gamma.lci <- qgamma(alpha/2, shape = (dsr^2)/dsr.var, scale = dsr.var/dsr)
  gamma.uci <- qgamma(1 - alpha/2, shape = ((dsr+wm)^2)/(dsr.var+wm^2), 
                      scale = (dsr.var+wm^2)/(dsr+wm))
  
  c(crude.rate = cruderate, adj.rate = dsr, lci = gamma.lci, 
    uci = gamma.uci, se = dsr.se)
}

#-----------------------COUNTY (AND STATE SUMMARY) LEVEL SUMMARY DATA FILES AND CRUDE RATES------------------------------------------------------#

#Statewide
s.lev0 <- sum_num_costs(oshpd16Primary, c("sex", "ccs_diagP", "year"), "ccs0")
# s.lev1 <- sum_num_costs(oshpd16Primary, c("sex", "lev1", "year"), "ccs0") #top level

state_sum <- bind_rows(s.lev0)
state_sum$county <- STATE #California as "county" variable

#ADD AGE!!!!!!!!!!!
cXXXXasge <- sum_num_costs(oshpd16Primary, c("sex", "ageG","ccs_diagP", "county", "year"), "ccs0")


#County
c.lev0 <- sum_num_costs(oshpd16Primary, c("sex", "ccs_diagP", "county", "year"), "ccs0")
#c.lev1 <- sum_num_costs(oshpd16Primary, c("sex", "lev1", "county", "year"), "lev1") #top level

county_sum <- bind_rows(c.lev0)

#merging county and state
total_sum <- bind_rows(state_sum, county_sum) %>% as.data.frame()

# removing unknown and other gender variables, NA county, and NA CAUSES
# total_sum_pop doesn't have any lev3 data because CAUSE = NA for all lev3 in this situation (and information is identical to lev0)
total_sum_pop <- total_sum %>% left_join(., popCountySex, by = c("year", "sex", "county")) %>% filter(sex != "Unknown" & sex != "Other", !is.na(county)) 


#checking NA charges
total_sum_pop_NA <- total_sum_pop %>% filter(is.na(charges))


total_sum_pop_AGE <- total_sum %>% left_join(., popCountySex, by = c("year", "sex", "county")) %>% filter(sex != "Unknown" & sex != "Other", !is.na(county)) 


# #----------------------------------------------------------------------------------------------------------------------------------------#

## MAIN RATE CALCULATION ---------------------------------------------------------------

total_crude_rates <- calculate_crude_rates(total_sum_pop, yearN = 1)

#-----------------------------------------------AGE ADJUSTED ("AA") RATES-----------------------------------------------------------------------------------------------------------------#

if (1==2) { #NOT USED FOR NOW; #MAY WANT TO STUDY/ASSESS THE IMPACT OF NOT USING THIS AT SOME POINT
  
  year     <- data.frame(year     = 2016) # these "vectors" need to be dataframes for the sq merge below to work
  yearG3   <- data.frame(yearG3   = sort(unique(cbdDat0$yearG3)))
  CAUSE1   <- data.frame(CAUSE    = allLabels) 
  sex      <- data.frame(sex      = c("Male","Female","Total"))
  ageG     <- data.frame(ageG     = sort(unique(cbdDat0$ageG)))
  county   <- data.frame(county   = c(geoMap$countyName,"California"))         
  raceCode <- data.frame(raceCode = sort(unique(cbdDat0$raceCode)))
  
  fullMatCounty <- sqldf(" select * from  county cross join year  cross join CAUSE1 cross join sex cross join ageG")
  
  fullMatCounty <- mutate(fullMatCounty, county = as.character(county),CAUSE = as.character(CAUSE), sex = as.character(sex), ageG = as.character(ageG), tester = 0)
  
}


#---------------------Age-adjusted hospitalizations (county and statewide)-----------------------------------------------------#
#Using summary function that was already created 

sA0 <- sum_num_costs(oshpd16Primary, c(          "year", "sex", "ageG", "ccs_diagP"), "ccs0") %>% mutate(county = STATE)
cA0 <- sum_num_costs(oshpd16Primary, c("county", "year", "sex", "ageG", "ccs_diagP"), "ccs0") 
total_sum_age <- bind_rows(sA0, cA0)

#data cleaning
total_sum_age <- filter(total_sum_age, !is.na(ageG)) #nothing removed
total_sum_age <- filter(total_sum_age, !is.na(county)) #removed records with missing county

#joining population data and standard population data with summary hosp/charges by age and sex
ageCounty <- full_join(total_sum_age, popCountySexAgeG, by = c("county", "year", "sex", "ageG")) %>% full_join(., popStandard[,c("ageG", "US2000POP")], by = "ageG") 

# Now we have a dataset with the number of hospitalizations for each CAUSE, by county, age group, gender, and "level", 
# as well as info about the population and standard population for each demographic group--this dataset will be use to calculate age-adjusted rates

#Calculating age-adjusted rates--why are we using ageadjust.direct.SAM instead of ageadjust.direct?

countyAA <- ageCounty %>% 
  group_by(county, year, sex, ccs_diagP, Level) %>% 
  summarize(ahospRate = ageadjust.direct.SAM(count = n_hosp, pop = pop, rate = NULL, stdpop = US2000POP, conf.level = 0.95)[2]*yF,
            aLCI = ageadjust.direct.SAM(count = n_hosp, pop = pop, rate = NULL, stdpop = US2000POP, conf.level = 0.95)[3]*yF,
            aUCI = ageadjust.direct.SAM(count = n_hosp, pop = pop, rate = NULL, stdpop = US2000POP, conf.level = 0.95)[4]*yF,
            aSE = ageadjust.direct.SAM(count = n_hosp, pop = pop, rate = NULL, stdpop = US2000POP, conf.level = 0.95)[5]*yF) %>% 
  filter(year == "2016")

countyAA_new <- countyAA


####------------------------------------------Creating new dataset to plot all metrics on same plot/facets---------------------------------------------------------#

# total_sum_pop     = contains n_hosp and charges
# total_crude_rates = contains crude hospitalization and charge rates
# countyAA_new = contains age-adjusted hospitalization rates
# Will have to do a series of spread/gather/join to create dataset 

calculated_sums        <- total_sum_pop     %>% gather(key = "type", value = "measure", n_hosp, avg_los, charges, avgcharge, avgcharge_per_day, medcharge, medcharge_per_day)
calculated_crude_rates <- total_crude_rates %>% gather(key = "type", value = "measure", cHospRate, cChargeRate)
calculated_aa_rates    <- countyAA_new %>%      gather(key = "type", value = "measure", ahospRate) 

calculated_metrics     <- bind_rows(calculated_sums, calculated_crude_rates, calculated_aa_rates) %>%
                            mutate(county  = ifelse(county== "California","CALIFORNIA",county),
                            diagnosis_var = "CCS-Beta") %>% 
                            select(sex, ccs_diagP, year, Level, county, ageG, pop, type, measure, diagnosis_var)

#Saving RDS file of this dataframe
saveRDS(calculated_metrics, file = path(myPlace, "myData/",whichData,"/countyOSHPD.rds"))

