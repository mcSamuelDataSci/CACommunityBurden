######  CELL SUPRESSION

# OSHPD sheet on coding of charges:
# https://oshpd.ca.gov/ml/v1/resources/document?rs:path=/Data-And-Reports/Documents/Submit/Patient-Level-Administrative/IP/IP-Total-Charges.pdf

# ====================================================================================================
# "E3.Process_OSHPD_data.R" file                                                                     |
#                                                                                                    |
# ====================================================================================================

#---SET LOCATIONS-----------------------------------------------------------------------

server    <- FALSE
readSAS   <- FALSE
whichData <- "real"   # "fake"

if (!server) source("g:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
if (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")


#---Get Standards -----------------------------------------------------------------------
raceLink   <-  raceLink %>%
  select("raceCode","OSHPD") %>%
  mutate(OSHPD=as.character(OSHPD))

countyLink <-  read_excel(paste0(standardsPlace,"countyLink.xlsx")) %>%
  select(cdphcaCountyTxt,countyName)

####################geoMap is now countyLink

#reading in county-codes-to-names linkage files --oshpd codes map to column "cdphcaCountyTxt"
geoMap     <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/County Codes to County Names Linkage.xlsx")))

source(paste0(standardsPlace,"ageChop.R"))
source(paste0(standardsPlace,"populationExtract.R"))


#---Constants
yF          <- 100000  # rate constant 
STATE       <- "CALIFORNIA"
myYearG.t   <- "2017-2019"
currentYear <- 2019





#---CONVERT SAS TO RDS HERE----------------------------------------------------------------



if (readSAS) {

  library(haven)
  
  pdd.work  <- read_sas(paste0(securePlace,"rawOSHPD/PDD/pdd_work.sas7bdat") )
  ###---------------------------------------------------------
  ### race_grp changed in 2019 to include NHPI and Multiracial
  ### prior to 2019 code 6 was "other"; in 2019 other is "8"
  ### line below standardizes that
  pdd.work$race_grp[pdd.work$year %in% 2017:2018 & pdd.work$race_grp == 6] <- 8
  
  pdd.work  <- pdd.work %>%
                  rename(pCounty = patcnty, age = agyrdsch, CCS=ccs_diagP)  %>%
                  mutate(causeCode = str_pad(CCS, 5,"left",pad="o"))
                    
  saveRDS(pdd.work, file = paste0(securePlace, "myData/oshpd_pdd.RDS"))
  
  pdd.currentYear <- pdd.work %>% filter(year==currentYear)
  saveRDS(pdd.currentYear, file = paste0(securePlace, "myData/oshpd_pdd_currentYear.RDS"))
  
  # FOR AGE/RACE Focus charts
  pdd.small <- pdd.work %>% select(-c(oshpd_id,los:msdrg,CCS:ccs_odiag24))
  saveRDS(pdd.small, file = paste0(securePlace, "myData/oshpd_pdd_small.RDS"))
  
  
  # oshpd.pdd.work2  <- read_sas(paste0(securePlace,"rawOSHPD/PDD/pdd_work2.sas7bdat") )
  # saveRDS(oshpd.pdd.work2, file = path(securePlace, "myData/oshpd_pdd_small.RDS"))
  
  
  oshpd.ed.work    <- read_sas(paste0(securePlace,"rawOSHPD/ED/ed_work.sas7bdat") )
  saveRDS(oshpd.ed.work, file = path(securePlace, "myData/oshpd_ed.RDS"))
  
}  


#------------------------------------------------------------------------------------------
#---PROCESS DATA ONLY FOR AGE/RACE FOCUS HERE FOR -----------------------------------------

#-- PDD DATA ---------------

pdd0   <- readRDS(paste0(securePlace,"/myData/oshpd_pdd_small.RDS")) 

pdd0 <-  pdd0 %>%
          left_join(raceLink, by=c("race_grp"="OSHPD")) %>% 
          left_join(countyLink, by=c("pCounty"="cdphcaCountyTxt")) 


# pdd_stroke_for_Catrina <- pdd0 %>%
#                           filter(age > 17) %>%
#                           mutate(stroke = ifelse(CCS=="oo109","Yes","No")) %>%
#                           group_by(year,stroke) %>%
#                           summarize(N=n()) %>%
#                           pivot_wider(names_from = stroke, values_from = N) %>%
#                           mutate(Stroke=Yes,
#                                  Total = Yes+No,
#                                  Percent_Stroke = 100*Yes/Total) %>%
#                           select(Year=year,Total,Stroke,Percent_Stroke)
# write_csv(pdd_stroke_for_Catrina, "pdd_stroke_for_Catrina.csv")

pdd1 <- bind_rows(pdd0,mutate(pdd0,countyName = "CALIFORNIA"))

hospYear <-  pdd1 %>%
  group_by(year, countyName, causeCode) %>%
  summarise(n_hosp=n()) %>%
  select(year,  county = countyName, causeCode, n_hosp) %>%
  ungroup()

hospAge  <- pdd1 %>%
  mutate(ageGroup = ageChop(age,"standard", ourServer = server)) %>%
  group_by(countyName, ageGroup, causeCode) %>%
  summarise(n_hosp=n()) %>%
  select(causeCode, county = countyName, ageGroup, n_hosp) %>%
  mutate(sex = "Total") %>% 
  mutate( yearG3 = myYearG.t) %>%   #HACK TO FIX Eventually
  ungroup()

saveRDS(hospAge, paste0(ccbData,"real/age_race_focus_data/hospAge.RDS"))

hospRace  <- pdd1 %>%
  group_by(countyName, raceCode, causeCode) %>%
  summarise(n_hosp=n()) %>%
  select(causeCode, county = countyName, raceCode, n_hosp) %>%
  mutate(sex = "Total") %>% 
  mutate( yearG3 = myYearG.t) %>%   #HACK TO FIX Eventually
  ungroup()
saveRDS(hospRace, paste0(ccbData,"real/age_race_focus_data/hospRace.RDS"))

#-- ED DATA ----------------

ed0  <- readRDS(paste0(securePlace,"/myData/oshpd_ed.RDS")) 
ed0  <- ed0 %>% rename(age = agyrserv, pCounty = patco, CCS = ccs_dx_prin)

ed0$race_grp[ed0$year %in% 2017:2018 & ed0$race_grp == 6] <- 8

ed0 <-  left_join(ed0,raceLink,by=c("race_grp"="OSHPD")) 
ed0 <-  left_join(ed0,countyLink,by=c("pCounty"="cdphcaCountyTxt"))  %>%
         mutate(causeCode = str_pad(CCS, 5,"left",pad="o"))   

ed1 <- bind_rows(ed0,mutate(ed0,countyName = "CALIFORNIA"))

edYear <- ed1 %>%
  group_by(year, countyName, causeCode) %>%
  summarise(n_ED=n()) %>%
  select(year,  county = countyName, causeCode, n_ED) %>%
  ungroup()

edAge  <- ed1 %>%
  mutate(ageGroup = ageChop(age,"standard", ourServer = server)) %>%
  group_by(countyName, ageGroup, CCS) %>%
  summarise(n_ED=n()) %>%
  select(causeCode = CCS, county = countyName, ageGroup, n_ED) %>%
  mutate(sex = "Total") %>%
  mutate( yearG3 = myYearG.t) %>%   #HACK TO FIX Eventually
  ungroup()
saveRDS(edAge, paste0(ccbData,"real/age_race_focus_data/edAge.RDS"))

edRace  <- ed1 %>%
  group_by(countyName, raceCode, CCS) %>%
  summarise(n_ED=n()) %>%
  select(causeCode = CCS, county = countyName, raceCode, n_ED) %>%
  mutate(sex = "Total") %>% 
  mutate( yearG3 = "2016-2018") %>%   #HACK TO FIX Eventually
  ungroup()
saveRDS(edRace, paste0(ccbData,"real/age_race_focus_data/edRace.RDS"))

hosp_ED_year <- full_join(hospYear, edYear, by =c("year", "county", "causeCode"))
saveRDS(hosp_ED_year, paste0(ccbData,"real/hosp_ED_year.RDS"))


#==========================================================================================
#==========================================================================================


# NOTE - input files names have changed.

#-------------------------------------LOAD AND PROCESS OSHPD DATA-------------------------
#-------------------------------------LOAD AND PROCESS OSHPD DATA-------------------------


# if (whichData == "real") {
#   oshpd.PDD.16 <- readRDS(file=path(securePlace, "myData/oshpd_subset.rds")) 
#   oshpd.ED.16 <- readRDS(file=path(securePlace, "myData/oshpd_ED_subset.rds")) 
#   
#   pdd_current_small  <-  readRDS(file=path(securePlace, "myData/oshpd_pdd_small.RDS")) 
#   ed_current         <-  readRDS(file=path(securePlace, "myData/oshpd_ed.RDS")) 
#   
# }
# 
# if (whichData == "fake") {
#   oshpd.PDD.16 <- readRDS(file=path(upPlace, "upData/oshpd16_sample.rds"))
# }



#NEW:

pdd1  <- readRDS(paste0(securePlace,"/myData/oshpd_pdd_currentYear.RDS")) 
#pdd1  <- pdd1 %>% select(-c(oshpd_id,odiag1:odiag24))

# Adding "o" pads before numeric CCS codes for consistent matching/linking/coding
ccsCols               <- grep("^ccs_",colnames(pdd1))
pdd1[,ccsCols]        <-  apply(pdd1[,ccsCols],2, function(x) str_pad(x, 5,"left",pad="o"))  ## tidyverse?  map?
pdd1[pdd1 == "ooooo"] <-  NA


# do this at the end  
##             rename(pCounty = patcnty, age = agyrdsch, CCS=ccs_diagP)  
##    pdd0 <-  left_join(pdd0,countyLink,by=c("pCounty"="cdphcaCountyTxt")) %>%


##------------------------------------Reading in data mapping/linkage files--------------------
# sex categories
# bind_cols keeps values as characters, wherese cbind converts to factor, which we don't want
sex_num   <- c(   "1",      "2",     "3",       "4",    "5")
sex_cat   <- c("Male", "Female", "Other", "Unknown","Total")
OSHPD_sex <- bind_cols(sex_num=sex_num, sex_cat=sex_cat) %>% as.data.frame() 



#----------------------------------------------ADD AGE-GROUp VARIABLE---------------------------------------------------------#

# ageMap     <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/Age Group Standard and US Standard 2000 Population.xlsx"),trim_ws = FALSE,sheet = "data"))
# 
# 
# aL            <-      ageMap$lAge     # lower age ranges
# aU            <- c(-1,ageMap$uAge)    # upper age ranges, plus inital value of "-1" for lower limit
# aLabs         <- paste(aL,"-",aU[-1]) # make label for ranges
# aLabs         <- ageMap$ageLabel
# aMark         <- findInterval(oshpd.PDD.16$agyrdsch,aU,left.open = TRUE)  # vector indicating age RANGE value of each INDIVIDUAL age value
# oshpd.PDD.16$ageG  <- aLabs[aMark] 
# 

#-------------------------------------LOAD AND PROCESS POPULATION DATA------------------------------





# ungrouping important for subsequent data set merging
# popCounty        <- readRDS(path(upPlace,"/upData/popCounty.RDS")) %>% ungroup() 
# popCountySex     <- filter(popCounty,ageG == "Total")
# popCountySexAgeG <- filter(popCounty,ageG != "Total")
# 
# popStandard         <- ageMap %>% mutate(ageG = paste0(lAge," - ",uAge))






# work on ANY HERE
# CCS CODING ---------------------------------------------------------------------

# WARNING - need to change these column numbers if osdpd16 changes



# Create one variable with all 25 CCS codes concatenated for efficient string searching
pddAny <- pdd1 %>% 
  mutate(all_CCS_One_String = 
           paste(causeCode,    ccs_odiag1,  ccs_odiag2, ccs_odiag3, ccs_odiag4,
                 ccs_odiag5,   ccs_odiag6,  ccs_odiag7, ccs_odiag8, ccs_odiag9,
                 ccs_odiag10, ccs_odiag11, ccs_odiag12, ccs_odiag13, ccs_odiag14,
                 ccs_odiag15, ccs_odiag16, ccs_odiag17, ccs_odiag18, ccs_odiag19, 
                 ccs_odiag20, ccs_odiag21, ccs_odiag22, ccs_odiag23, ccs_odiag24))  











# TODO
# learning about OSHPD data
# no ccs code "above" 670 appears in the data set 

start_time <- Sys.time()

# Create (283) new indicator varibles, one for each CCS code, indicting if that code occurrs in the
#  primary CCS code or any of the 24 secondary CCS codes

for (i in 1:nrow(hospCauseLink)) {
  pddAny <- pddAny %>% mutate(
    !!hospCauseLink$causeCode[i] := ifelse(grepl(hospCauseLink$causeCode[i], all_CCS_One_String), 1, 0)) 
  # !!hospCauseLink$ccsCodePaded[i] := ifelse(apply(oshpd16_new[,2:26], 1, function(r) any(r == hospCauseLink$causeCode[i])), 1, 0)) #SLOWER 
}
end_time <- Sys.time()
end_time - start_time
#Time difference of 45.19121 mins


# discard bunch of variable for efficient processing
pddAny <- select(pddAny,-c(CCS:ccs_odiag24, all_CCS_One_String))


##################################################


# save/read this file
saveRDS(pddAny, file=path(securePlace, "myData/pddAnyIntermediate.rds"))
pddAny <- readRDS(file=path(securePlace, "myData/pddAnyIntermediate.rds")) 

##################################################



# Sum each CCS indicator variable, by county and sex, and "flip" it (i.e. wide to long) such that there is one
#  variable ("ccsANY") with the count for that CCS code, and one variable ("ccsMeasure") that indicates the CCS code

## NOT ENOUGH MEMORY(?) TO DO THIS
# temp <- pivot_longer(pddAny,ooo1:o237,names_to = "ccsANY")

i <- 1
## get function!
ccs.t0 <- pddAny %>% 
  group_by(patcnty, sex) %>% 
  summarize(n_hosp_any = sum(get(hospCauseLink$causeCode[i]))) %>% 
  mutate(ccsCode=hospCauseLink$causeCode[i])

for (i in 2:nrow(hospCauseLink)) {
  ccs.t1 <- pddAny %>% 
    group_by(patcnty, sex) %>% 
    summarize(n_hosp_any = sum(get(hospCauseLink$causeCode[i]))) %>% 
    mutate(ccsCode=hospCauseLink$causeCode[i])
  
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
  mutate(county  = ifelse(patcnty == "CA",STATE,county)) %>%
  select(-sex) %>% 
  mutate(sex=sex_cat)  %>% select(-sex_cat) %>%
  filter(sex %in% c("Male","Female","Total")) %>%
  filter(n_hosp_any > 0)


saveRDS(ccsAnyWork, file = path(myPlace, "myData/",whichData,"/oshpd_PDD_any.rds"))




# =====================================================================================================================
# ---------------------------------------------------------------------------------------------------------------------
# start OSHPD PRIMARY work here



oshpd_PDD_Prim <- oshpd.PDD.16 %>% 
  mutate( ccsCode = str_pad(ccs_diagP, 5,"left",pad="o")) %>%
  select(-(ccs_odiag1:msdrg)) %>% 
  left_join(., select(geoMap,cdphcaCountyTxt,county=countyName), by = c("patcnty"= "cdphcaCountyTxt")) %>% 
  left_join(., OSHPD_race_grp, by = "race_grp") %>%
  left_join(., OSHPD_sex, by = c("sex" = "sex_num"))  %>% select(-sex) %>% mutate(sex=sex_cat)  %>% select(-sex_cat)

#Adding Total in order to create total/statewide estimates (for grouping function later)
oshpd_PDD_Prim <- mutate(oshpd_PDD_Prim, sex = "Total") %>% 
  bind_rows(oshpd_PDD_Prim) 

#Calculating charge/day from los_adj and charges
oshpd_PDD_Prim <- oshpd_PDD_Prim %>% mutate(charge_per_day = charge/los_adj)

###-------------------------------------------EXPLORATORY ANALYSIS OF LENGTH OF STAY AND CHARGES (REAL OSHPD DATA, NOT SAMPLE)----------------------------------------------#

# TODO
# what do charges 2 and 3 mean? Are they also some sort of pro bono/not real charges info? 

#-----------------------------*_*_*_*_*_*_*_*_*_*_

oshpd_PDD_Prim$charge[oshpd_PDD_Prim$charge == 0] <- NA #changing 0 and 1 charges (kaiser or pro-bono cases) to NA
oshpd_PDD_Prim$charge[oshpd_PDD_Prim$charge == 1] <- NA
oshpd_PDD_Prim$charge_per_day[oshpd_PDD_Prim$charge_per_day == 0] <- NA #changing 0 and 1 charges (kaiser or pro-bono cases) to NA
oshpd_PDD_Prim$charge_per_day[oshpd_PDD_Prim$charge_per_day == 1] <- NA


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
s.lev0 <- sum_num_costs(oshpd_PDD_Prim, c("sex", "ccsCode", "year"), "ccs0")
# s.lev1 <- sum_num_costs(oshpd_PDD_Prim, c("sex", "lev1", "year"), "ccs0") #top level

state_sum <- bind_rows(s.lev0)
state_sum$county <- STATE #CALIFORNIA as "county" variable

#County
c.lev0 <- sum_num_costs(oshpd_PDD_Prim, c("sex", "ccsCode", "county", "year"), "ccs0")
#c.lev1 <- sum_num_costs(oshpd_PDD_Prim, c("sex", "lev1", "county", "year"), "lev1") #top level

county_sum <- bind_rows(c.lev0)

#merging county and state
total_sum <- bind_rows(state_sum, county_sum) %>% as.data.frame()


# TODO review if any of these should be added back in?
# removing unknown and other gender variables, NA county, and NA CAUSES
# total_sum_pop doesn't have any lev3 data because CAUSE = NA for all lev3 in this situation (and information is identical to lev0)
# %>% filter(sex != "Unknown" & sex != "Other", !is.na(county)) 

total_sum_pop <- total_sum %>% left_join(., popCountySex, by = c("year", "sex", "county"))



#checking NA charges
total_sum_pop_NA <- total_sum_pop %>% filter(is.na(charges))

# ----------- by AGE GROUP ------------------------------------------------------------------------------------


# t.PD.age.state  <- sum_num_costs(oshpd_PDD_Prim, c("sex", "ageG","ccsCode",           "year"), "ccs0") %>% mutate(county=STATE)
# t.PD.age.county <- sum_num_costs(oshpd_PDD_Prim, c("sex", "ageG","ccsCode", "county", "year"), "ccs0") 
# 
# t.PD.age        <- bind_rows(t.PD.age.state ,t.PD.age.county) %>% 
#                      left_join(., popCountySexAgeG, by = c("year", "sex", "ageG","county")) %>% 
#                      as.data.frame()
# 
# 
# PDD.age    <- calculate_crude_rates(t.PD.age, yearN = 1)
# 
# saveRDS(PDD.age, file = path(myPlace, "myData/",whichData,"/oshpd_PDD_AGE.rds"))

# #----------------------------------------------------------------------------------------------------------------------------------------#

## MAIN RATE CALCULATION ---------------------------------------------------------------

total_crude_rates <- calculate_crude_rates(total_sum_pop, yearN = 1)

#-----------------------------------------------AGE ADJUSTED ("AA") RATES-----------------------------------------------------------------------------------------------------------------#
#---------------------Age-adjusted hospitalizations (county and statewide)-----------------------------------------------------#
#Using summary function that was already created 

sA0 <- sum_num_costs(oshpd_PDD_Prim, c(          "year", "sex", "ageG", "ccsCode"), "ccs0") %>% mutate(county = STATE)
cA0 <- sum_num_costs(oshpd_PDD_Prim, c("county", "year", "sex", "ageG", "ccsCode"), "ccs0") 
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
  group_by(county, year, sex, ccsCode, Level) %>% 
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
  mutate(diagnosis_var = "CCS-Beta") %>% 
  select(sex, ccsCode, year, Level, county, ageG, pop, type, measure, diagnosis_var)

#Saving RDS file of this dataframe

XXXXX rename this something with primary.  here, global, and plotting functions.


saveRDS(calculated_metrics, file = path(myPlace, "myData/",whichData,"/oshpd_PDD.rds"))



# ====== QUICK AND DIRTY ED PROCESSING=================================================================================

# 
# # TODO check age variable name is differnt that PDD
# 
# aMark             <- findInterval(oshpd.ED.16$agyrserv,aU,left.open = TRUE)  
# oshpd.ED.16$ageG  <- aLabs[aMark] 
# oshpd.ED.16       <- oshpd.ED.16 %>% mutate( ccsCode = str_pad(ccs_dx_prin, 5,"left",pad="o"))
# 
# 
# # functions WITHOUT CHARGES----------------------------------------------------------------------------------------------
# 
# sum_num_costsX <- function(data, groupvar, levLab) {
#   
#   dat <- data %>% group_by_at(.,vars(groupvar)) %>% 
#     summarize(n_hosp = n(), ) #adding median charge per day
# }
# 
# #function to calculate crude hospitalization rates and charge-rates 
# calculate_crude_ratesX <- function(data, yearN) {
#   data %>% mutate(cHospRate = yF*n_hosp/(yearN*pop), 
#                   hosp_rateLCI    = yF*pois.approx(n_hosp,yearN*pop, conf.level = 0.95)$lower,
#                   hosp_rateUCI    = yF*pois.approx(n_hosp,yearN*pop, conf.level = 0.95)$upper)
# }
# 
# # processing -----------------------------------------------------------
# 
# ## TODO  county names different than PDD
# 
# oshpd.ED.16.Primary <- oshpd.ED.16 %>% #            select(-(ccs_odiag1:msdrg)) %>% 
#   left_join(., select(geoMap,cdphcaCountyTxt,county=countyName), by = c("patco"= "cdphcaCountyTxt")) %>% 
#   left_join(., OSHPD_race_grp, by = "race_grp") %>%
#   left_join(., OSHPD_sex, by = c("sex" = "sex_num"))  %>% select(-sex) %>% mutate(sex=sex_cat)  %>% select(-sex_cat)
# 
# #Adding Total in order to create total/statewide estimates (for grouping function later)
# oshpd.ED.16.Primary <- mutate(oshpd.ED.16.Primary, sex = "Total") %>% 
#   bind_rows(oshpd.ED.16.Primary) 
# 
# # ----------- by AGE GROUP ------------------------------------------------------------------------------------
# 
# #NO YEAR FOR NOW
# t.ED.age.state   <- sum_num_costsX(oshpd.ED.16.Primary, c("sex", "ageG","ccsCode"), "ccs0") %>% mutate(county=STATE)
# t.ED.age.county  <- sum_num_costsX(oshpd.ED.16.Primary, c("sex", "ageG","ccsCode", "county"), "ccs0") 
# popCountySexAgeG <- filter(popCountySexAgeG,year==2016)
# 
# t.ED.age        <- bind_rows(t.ED.age.state ,t.ED.age.county) %>% 
#   left_join(., popCountySexAgeG, by = c("sex", "ageG","county")) %>% 
#   as.data.frame()
# 
# ED.age    <- calculate_crude_ratesX(t.ED.age, yearN = 1)
# 
# saveRDS(ED.age, file = path(myPlace, "myData/",whichData,"/oshpd_ED_AGE.rds"))
# 
# # ====== END QUICK AND DIRTY ED PROCESSING=================================================================================
# 
# 





# ------------------------------------------------------------------------------------------------
# -SAMPLE-----------------------------------------------------------------------------------------


#3% random sample, randomly permuted------------------------------------------------
set.seed(4)
#oshpd_sample <- sample_n(oshpd_subset, size = 0.01*nrow(oshpd_subset), replace = F)

sampN1 <- 0.01*nrow(oshpd.pdd.work1 )  
sampN2 <- sampN1*2

half1  <- sample_n(oshpd.pdd.work1 ,sampN1)  # sample function from dplyr

p1           <- sample_n(oshpd.pdd.work1 [,1:8],  sampN2)
p2           <- sample_n(oshpd.pdd.work1 [,9:10], sampN2)
p3           <- sample_n(oshpd.pdd.work1 [,11:16], sampN2); p3$race_grp  <- NA
p4           <- sample_n(oshpd.pdd.work1 [,17:41], sampN2)
p5           <- sample_n(oshpd.pdd.work1 [,42:65], sampN2)

half2        <- cbind(p1,p2,p3,p4,p5)

oshpd_sample <- rbind(half1,half2)

# Saving random sample as RDS file
saveRDS(oshpd_sample, file = path(upPlace, "upData/oshpd_pdd_SAMPLE.rds")) 

















