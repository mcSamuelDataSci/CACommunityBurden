######  CELL SUPRESSION

# OSHPD sheet on coding of charges:
# https://oshpd.ca.gov/ml/v1/resources/document?rs:path=/Data-And-Reports/Documents/Submit/Patient-Level-Administrative/IP/IP-Total-Charges.pdf

# ====================================================================================================
# "E3.Process_OSHPD_data.R" file                                                                     |
#                                                                                                    |
# ====================================================================================================

#--- GET STANDARDS AND SET LOCATIONS-----------------------------------------------------------------------

server    <- FALSE
readSAS   <- FALSE
whichData <- "real"   # "fake"

if (!server) source           ("g:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
if  (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")

# per Standards, currentYear is "currentYear  <- 2019"; edit here as needed
currentYear <- currentYear
currentYear <- 2019

raceLink   <-  raceLink %>%
                 select(raceCode, OSHPD, raceNameShort) %>%
                 mutate(OSHPD=as.character(OSHPD))

countyLink <-  read_excel(paste0(standardsPlace,"countyLink.xlsx")) %>%
                select(cdphcaCountyTxt,countyName)

source(paste0(standardsPlace,"ageChop.R"))
source(paste0(standardsPlace,"populationExtract.R"))


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
                  mutate(causeCode = str_pad(CCS, 5,"left",pad="o"))  %>%
                  mutate(sex = ifelse(sex=="M", "Male", ifelse(sex=="F", "Female", "unk")))
  
  # Full PDD data
  saveRDS(pdd.work, file = paste0(securePlace, "myData/oshpd_pdd.RDS"))
  
  # Current year PDD data
  pdd.currentYear <- pdd.work %>% filter(year==currentYear)
  saveRDS(pdd.currentYear, file = paste0(securePlace, "myData/oshpd_pdd_currentYear.RDS"))
  
  # Tiny sample of current year data
  pdd.currentYear_sample  <- sample_frac(pdd.currentYear, 0.001)
  saveRDS(pdd.currentYear_sample, file = paste0(securePlace, "myData/oshpd_pdd_currentYear_sample.RDS"))
  
  # Primary diagnosis only PDD data
  pdd.small <- pdd.work %>% select(-c(oshpd_id,los:msdrg,CCS:ccs_odiag24))
  saveRDS(pdd.small, file = paste0(securePlace, "myData/oshpd_pdd_small.RDS"))
  
  # Full ED data
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
  mutate( yearG3 = yearG3) %>%   #HACK TO FIX Eventually
  ungroup()

saveRDS(hospAge, paste0(ccbData,"real/age_race_focus_data/hospAge.RDS"))

hospRace  <- pdd1 %>%
  group_by(countyName, raceCode, causeCode) %>%
  summarise(n_hosp=n()) %>%
  select(causeCode, county = countyName, raceCode, n_hosp) %>%
  mutate(sex = "Total") %>% 
  mutate( yearG3 = yearGrp3) %>%   #HACK TO FIX Eventually
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
  mutate( yearG3 = yearGrp3) %>%   #HACK TO FIX Eventually
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
# PROCESSING FILE FOR CCB "HOSPITALIZATION" TAB

# pdd1  <- readRDS(paste0(securePlace,"/myData/oshpd_pdd_currentYear_sample.RDS")) 
pdd1  <- readRDS(paste0(securePlace,"/myData/oshpd_pdd_currentYear.RDS")) 

# Adding "o" pads before numeric "other" CCS codes for consistent matching/linking/coding
#  already done above for primary cause
ccsCols               <- grep("^ccs_",colnames(pdd1))
pdd1[,ccsCols]        <-  apply(pdd1[,ccsCols],2, function(x) str_pad(x, 5,"left",pad="o"))  ## tidyverse?  map?
pdd1[pdd1 == "ooooo"] <-  NA

#-------------------------------------LOAD AND PROCESS POPULATION DATA------------------------------

popCounty        <- readRDS(paste0(ccbUpstream,"/upData/popCounty.RDS")) %>% ungroup() 
popCountySex     <- filter(popCounty,	raceCode == "Total", ageGroup == "Total")
popCountySexAgeG <- filter(popCounty,	raceCode == "Total", ageGroup != "Total")

ageStandard     <- as.data.frame(read_excel(paste0(ccbInfo,"Age Group Standard and US Standard 2000 Population.xlsx"),sheet = "data"))



# -------------------------------------------------------------------------------------------------
# PDD "ANY" cause work here


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

# Create (283) new indicator variable, one for each CCS code, indicting if that code occurs in the
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

# save/read this file for efficiency
saveRDS(pddAny, file=path(securePlace,   "myData/pddAnyIntermediate.rds"))
pddAny <- readRDS(file=path(securePlace, "myData/pddAnyIntermediate.rds")) 

##################################################


# Sum each CCS indicator variable, by county and sex, and "flip" it (i.e. wide to long) such that there is one
#  variable ("ccsANY") with the count for that CCS code, and one variable ("ccsMeasure") that indicates the CCS code
## NOT ENOUGH MEMORY(?) TO DO THIS
# temp <- pivot_longer(pddAny,ooo1:o237,names_to = "ccsANY")

i <- 1
ccs.t0 <- pddAny %>% 
  group_by(year, pCounty, sex) %>% 
  summarize(n_hosp_any = sum(get(hospCauseLink$causeCode[i]),na.rm = TRUE)) %>% 
  mutate(ccsCode=hospCauseLink$causeCode[i])

options(dplyr.summarise.inform = FALSE)

for (i in 2:nrow(hospCauseLink)) {
  ccs.t1 <- pddAny %>% 
    group_by(year, pCounty, sex) %>% 
    summarize(n_hosp_any = sum(get(hospCauseLink$causeCode[i]))) %>% 
    mutate(ccsCode=hospCauseLink$causeCode[i])
  
  ccs.t0 <- bind_rows(ccs.t0,ccs.t1)
}



# make "Total Sex" copy and join
ccs.t2      <- ccs.t0 %>% 
  group_by(year, pCounty, ccsCode) %>% 
  summarise(n_hosp_any=sum(n_hosp_any)) %>%   
  mutate(sex = "Total") %>%  
  bind_rows(ccs.t0)

# make "CA State" copy and join
ccs.t3     <- ccs.t2 %>% 
  filter(! pCounty %in% c("","-","00")) %>%          #### NEED TO DO DO THIS HIGHER UP  - NOT CA????
  group_by(year, ccsCode,sex) %>% 
  summarise(n_hosp_any=sum(n_hosp_any)) %>%   
  mutate(pCounty = "CA")  %>%   
  bind_rows(ccs.t2)

ccsAnyWork <- ccs.t3 %>%
   left_join(select(countyLink, cdphcaCountyTxt,county=countyName), by = c("pCounty"= "cdphcaCountyTxt")) %>% 
   mutate(county  = ifelse(pCounty == "CA", STATE, county)) %>% select(-pCounty) %>%
   filter(sex %in% c("Male","Female","Total")) %>%
   filter(n_hosp_any > 0) 

saveRDS(ccsAnyWork, file = path(myPlace, "myData/",whichData,"/oshpd_PDD_any.rds"))


# =====================================================================================================================
# ---------------------------------------------------------------------------------------------------------------------
# start OSHPD PRIMARY work here

# pdd1   <- readRDS(paste0(securePlace,"/myData/oshpd_pdd_currentYear_sample.RDS")) 
  pdd1   <- readRDS(paste0(securePlace,"/myData/oshpd_pdd_currentYear.RDS")) 

# ageGroup
oshpd_PDD_Prim <- pdd1 %>% 
                   mutate(ageGroup = ageChop(age,"standard", ourServer = server)) %>%
                   select(year, pCounty, sex, ageGroup, race_grp, causeCode, charge, los_adj)

oshpd_PDD_Prim <- oshpd_PDD_Prim %>% 
  left_join(select(countyLink, cdphcaCountyTxt, county=countyName), by = c("pCounty"= "cdphcaCountyTxt")) %>%
  left_join(raceLink,by=c("race_grp"="OSHPD")) 

# Adding Total sex 
oshpd_PDD_Prim <- mutate(oshpd_PDD_Prim, sex = "Total") %>% 
                     bind_rows(oshpd_PDD_Prim) 

###-------------------------------------------EXPLORATORY ANALYSIS OF LENGTH OF STAY AND CHARGES (REAL OSHPD DATA, NOT SAMPLE)----------------------------------------------#
#Calculating charge/day from los_adj and charges
# TODO
# what do charges 2 and 3 mean? Are they also some sort of pro bono/not real charges info? 

oshpd_PDD_Prim <- oshpd_PDD_Prim %>% mutate(charge_per_day = charge/los_adj)

oshpd_PDD_Prim$charge[oshpd_PDD_Prim$charge == 0] <- NA #changing 0 and 1 charges (kaiser or pro-bono cases) to NA
oshpd_PDD_Prim$charge[oshpd_PDD_Prim$charge == 1] <- NA
oshpd_PDD_Prim$charge_per_day[oshpd_PDD_Prim$charge_per_day == 0] <- NA #changing 0 and 1 charges (kaiser or pro-bono cases) to NA
oshpd_PDD_Prim$charge_per_day[oshpd_PDD_Prim$charge_per_day == 1] <- NA


#-------------OSHPD CALCULATIONS FOR TOTAL VISITS/CHARGES AND CRUDE RATES------------------------------------------------------------------#

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

# Function to calculate crude hospitalization rates and charge-rates 
calculate_crude_rates <- function(data, yearN) {
  data %>% mutate(cHospRate = yF*n_hosp/(yearN*population), 
                  hosp_rateLCI    = yF*pois.approx(n_hosp,yearN*population, conf.level = 0.95)$lower,
                  hosp_rateUCI    = yF*pois.approx(n_hosp,yearN*population, conf.level = 0.95)$upper,
                  cChargeRate     = yF*charges/(yearN*population),
                  charge_rateLCI  = yF*pois.approx(charges,yearN*population, conf.level = 0.95)$lower,
                  charge_rateUCI  = yF*pois.approx(charges,yearN*population, conf.level = 0.95)$upper)
}

# Function to calculate age-adjusted hospitalization rates
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

# Statewide
s.lev0           <- sum_num_costs(oshpd_PDD_Prim, c("sex", "causeCode", "year"), "ccs0")
state_sum        <- bind_rows(s.lev0)
state_sum$county <- STATE #CALIFORNIA as "county" variable -- not done above?

# County
c.lev0     <- sum_num_costs(oshpd_PDD_Prim, c("sex", "causeCode", "county", "year"), "ccs0")
county_sum <- bind_rows(c.lev0)

# Merging county and state
total_sum <- bind_rows(state_sum, county_sum) %>% as.data.frame()


# TODO review if any of these should be added back in?
# removing unknown and other gender variables, NA county, and NA CAUSES
# %>% filter(sex != "Unknown" & sex != "Other", !is.na(county)) 


total_sum_pop <- total_sum %>% left_join(popCountySex, by = c("year", "sex", "county"))

#checking NA charges
total_sum_pop_NA <- total_sum_pop %>% filter(is.na(charges))


## MAIN RATE CALCULATION ---------------------------------------------------------------

library(epitools)
total_crude_rates <- calculate_crude_rates(total_sum_pop, yearN = 1)

# AGE ADJUSTED ("AA") RATES-----------------------------------------------------------------------------------------------------------------#
# Using summary function that was already created 

sA0 <- sum_num_costs(oshpd_PDD_Prim, c(          "year", "sex", "ageGroup", "causeCode"), "ccs0") %>% mutate(county = STATE)
cA0 <- sum_num_costs(oshpd_PDD_Prim, c("county", "year", "sex", "ageGroup", "causeCode"), "ccs0") 
total_sum_age <- bind_rows(sA0, cA0)

#data cleaning
total_sum_age <- filter(total_sum_age, !is.na(ageGroup)) #nothing removed
total_sum_age <- filter(total_sum_age, !is.na(county)) #removed records with missing county

#joining population data and standard population data with summary hosp/charges by age and sex

popCountySexAgeG <- popCountySexAgeG %>% filter(year == currentYear)

ageCounty <- full_join(total_sum_age, popCountySexAgeG, by = c("county", "year", "sex", "ageGroup")) %>% 
              full_join(ageStandard[,c("ageLabel", "US2000POP")], by = c("ageGroup"="ageLabel")) 

countyAA <- ageCounty %>% 
  group_by(county, year, sex, causeCode, Level) %>% 
  summarize(ahospRate = ageadjust.direct.SAM(count = n_hosp, pop = population, rate = NULL, stdpop = US2000POP, conf.level = 0.95)[2]*yF,
            aLCI = ageadjust.direct.SAM(count = n_hosp, pop = population, rate = NULL, stdpop = US2000POP, conf.level = 0.95)[3]*yF,
            aUCI = ageadjust.direct.SAM(count = n_hosp, pop = population, rate = NULL, stdpop = US2000POP, conf.level = 0.95)[4]*yF
            #aSE = ageadjust.direct.SAM(count = n_hosp, population = pop, rate = NULL, stdpop = US2000POP, conf.level = 0.95)[5]*yF) 
  )



####-----------Creating new dataset to plot all metrics on same plot/facets-----------------------------------------

# total_sum_pop     = contains n_hosp and charges
# total_crude_rates = contains crude hospitalization and charge rates
# countyAA_new = contains age-adjusted hospitalization rates
# Will have to do a series of spread/gather/join to create dataset 

calculated_sums        <- total_sum_pop     %>% gather(key = "type", value = "measure", n_hosp, avg_los, charges, avgcharge, avgcharge_per_day, medcharge, medcharge_per_day)
calculated_crude_rates <- total_crude_rates %>% gather(key = "type", value = "measure", cHospRate, cChargeRate)
calculated_aa_rates    <- countyAA          %>% gather(key = "type", value = "measure", ahospRate) 

calculated_metrics <- bind_rows(calculated_sums, calculated_crude_rates, calculated_aa_rates) %>%
                        mutate(diagnosis_var = "CCS-Beta") %>% 
                        select(sex, causeCode, year, Level, county, ageGroup, population, type, measure, diagnosis_var)

saveRDS(calculated_metrics, file = path(myPlace, "myData/",whichData,"/oshpd_PDD_primary.rds"))


# ====================================================================================================================
