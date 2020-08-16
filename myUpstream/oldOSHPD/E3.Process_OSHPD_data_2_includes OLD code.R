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


#----------------Map ICD-10-CM codes to GBD conditions-----------------------------------------------------------------------------------------#

# allLabels <- sort(icd_map$LABEL[!is.na(icd_map$LABEL)]) #Sorts all non-missing LABEL variables 
# 
# 
# # This creates a new object, mapICD, of all non-missing CODE variables, and the corresponding regEx10
# # associated with them. This object will be used to assign CODE/LABELS to diagnoses later
# mapICD    <- icd_map[!is.na(icd_map$CODE),c("CODE","regExICD10_CM")] 
# 
# # data.frame() as final step avoids problems later when using match() on tbl.df()
# # ######## REVIEW THIS ISSUE AT SOME POINT; in whole ccb use join() instead of match() -- may well be others issues -MCS
# fullCauseList     <- icd_map[!is.na(icd_map$causeList),c("LABEL","causeList","nameOnly")] %>% arrange(LABEL) %>% as.data.frame()
# fullList          <- fullCauseList[,"LABEL"]
# names(fullList)   <- fullCauseList[,"causeList" ]
# 
# # Function for mapping icd code to disease group
# # What this says is: for the length of the input vector, match the ICD10 regEx codes to the corresponding CODE in mapICD
# icdToGroup <- function(inputVectorICD10) {
#   Cause   <- rep(NA,length(inputVectorICD10))
#   for (i in 1:nrow(mapICD)) {Cause[grepl(mapICD[i,"regExICD10_CM"],inputVectorICD10)] <- mapICD[i,"CODE"] } 
#   Cause}

#-----------------------------ADDING VARIABLES TO OSHPD DATASET AND CLEANING/RECODING----------------------------------#



# #Using icdToGroup Function to map icd codes from diag_p variable to disease group
# oshpd16$icdCODE  <- icdToGroup(inputVectorICD10=oshpd16$diag_p) %>% as.character()
# 
# ##This converts the NAs from characters to NA so subsequent code won't treat them as characters, which is necessary when using str_sub to create the lev1, lev2, lev3 columns
# oshpd16$icdCODE[oshpd16$icdCODE == "NA"] <- NA
# 
# #This next section adds variables to the input vector, breaking down the CODE into up to 4 levels
# codeLast4 <- str_sub(oshpd16$icdCODE,2,5) #puts characters 2-5 from the CODE string
# nLast4    <- nchar(codeLast4) #counts number of characters 


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

# moved frequencies and histograms etc to "OSHPDexplore.Rmd



# OSHPD CHARGE AND CHARGE_PER_TDAY 0 and 1 to NA --hospitals that don't report charges (eg Kaiser) are assigned charges of 0, 
# pro bono cases as assigned charges of 1
#oshpd16_charge0 <- oshpd16 %>% filter(charge == 0)

#What should the exclusion cut-off be for los_adj? 365 days? Less than that? 

#----------------------------------------------------------------------------------------------------------------------------------------------#
#Some of these extreme los/charges may not even apply to the CAUSE/icdCodes that we're capturing though. Now only looking at values for our CAUSES of interest

#table of charges---Note that if charges are greater than the max seven digit input field size, they are listed as $9,999,999. Also, when a patient's length of stay is more than 365 days, only
#the last 365 days of charges are reported. HOWEVER, there are some charges with 8 digits listed, even though OSHPD info said standard format before December 2018 only have 7 digits? 
#https://oshpd.ca.gov/ml/v1/resources/document?rs:path=/Data-And-Reports/Documents/Submit/Patient-Level-Administrative/IP/IP-Total-Charges.pdf

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



##All of the NA cases that are present in the dataset at this point are cases in which there was only one hospitalization for a given CAUSE-gender-county, and the charge happened to be 0/NA. All the others (ie where there were multiple
#cases for a given CAUSE-gender-county combination) in which some of the charges happened to be 0/NA are grouped where total charges is calculated by removing NA values. 



# #----------------------------------------------------------------------------------------------------------------------------------------#
# #The problem that now arises in total_sum_pop is that CAUSES may appear among females in a given county that don't appear among males, and vice versa, which will cause issues when trying to make visualizations and summarizations later, since the data isn't the same length. 
# #To address this problem, we need to add in observations for the non-congruent CAUSES, and give them values of 0 for n_hosp and charges:
# 
# 
# # -MCS add commnet on rowid thing....
# spread_female_sum_pop <- total_sum_pop %>%                               filter(sex == "Female") %>% spread(., sex, CAUSE) %>% select(year, Level, Female, county)  #summarises all the CAUSES for females, by county and level
# spread_male_sum_pop   <- total_sum_pop %>% tibble::rowid_to_column() %>% filter(sex == "Male")   %>% spread(., sex, CAUSE) %>% select(year, Level, Male,   county) #summarises all the CAUSES for male, by county and level
# 
# female_only <- anti_join(spread_female_sum_pop, spread_male_sum_pop,   by = c("Female" = "Male", "year", "county", "Level")) #These are the CAUSE-county-level pairs that are in the female dataset but not the male dataset
# male_only   <- anti_join(spread_male_sum_pop,   spread_female_sum_pop, by = c("Male" = "Female", "year", "county", "Level")) #These are the CAUSE-county-level pairs that are in the male dataset but not the female dataset
# 
# # Now that we have these datasets, we need to add them back to their respective county_level spread dataset, convert NA to zero for n_hosp and charges. Gather, switch sex to opposite then join back with original total_sum_pop
# 
# add_males   <- gather(female_only, key = "sex", value = "CAUSE", Female) %>% mutate(sex = "Male")
# add_females <- gather(male_only,   key = "sex", value = "CAUSE", Male)   %>% mutate(sex = "Female")
# 
# # Now joining original total_sum_pop with these datasets, replacing NA with zeros
# total_sum_pop_new <- full_join(total_sum_pop, add_females, by = c("year", "Level", "county", "sex", "CAUSE")) %>% 
#   full_join(.,             add_males,   by = c("year", "Level", "county", "sex", "CAUSE"))
# 
# ## The issue now is distinguishing between NA that really should be changed to 0s--i.e 0 n_hosp, so 0 charges, and the 0 charges that we defined as NA earlier--need to keep these as NA
# # Example of replacing NA data in selected column
# # dat$four[is.na(dat$four)] <- 0  https://stackoverflow.com/questions/13172711/replace-na-values-from-a-column-with-0-in-data-frame-r
# 
# #replacing year with 2016
# # mcs NOTE: this will not generize when other years are added -- deal with that then
# total_sum_pop_new$year[is.na(total_sum_pop_new$year)] <- 2016
# 
# #replacing NA for n_hosp, charges, avgcharge with 0
# total_sum_pop_new$charges[          is.na(total_sum_pop_new$n_hosp)] <- 0 #only codes NA -> 0 for cases where n_hosp was NA, not a number of hospitalizations
# total_sum_pop_new$avgcharge[        is.na(total_sum_pop_new$n_hosp)] <- 0 #only codes 0 for cases where n_hosp was NA, not a real number of hospitalizations
# total_sum_pop_new$avgcharge_per_day[is.na(total_sum_pop_new$n_hosp)] <- 0 #only codes 0 for cases where n_hosp was NA
# total_sum_pop_new$medcharge[        is.na(total_sum_pop_new$n_hosp)] <- 0 #only codes 0 for cases where n_hosp was NA
# total_sum_pop_new$medcharge_per_day[is.na(total_sum_pop_new$n_hosp)] <- 0 #only codes 0 for cases where n_hosp was NA
# total_sum_pop_new$avg_los[          is.na(total_sum_pop_new$n_hosp)] <- 0 #only codes 0 for cases where n_hosp was NA
# total_sum_pop_new$n_hosp[           is.na(total_sum_pop_new$n_hosp)] <- 0 #changing NA n_hosp to 0
# 
# #replacing NA in ageG with "Total"
# total_sum_pop_new$ageG[is.na(total_sum_pop_new$ageG)] <- "Total"
# 
# 
# #Now we will  re-join the population dataset to make sure that the new 0-value female/male variables have associated populations
# 
# total_sum_pop_new <- total_sum_pop_new %>% left_join(., popCountySex, by = c("year", "county", "sex", "ageG")) %>% select(-pop.x) %>% rename(pop = pop.y)



## MAIN RATE CALCULATION ---------------------------------------------------------------
#total_crude_rates <- calculate_crude_rates(total_sum_pop_new, yearN = 1)

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
# sA1 <- sum_num_costs(oshpd16Primary, c(          "year", "sex", "ageG", "lev1"), "lev1") %>% mutate(county = STATE)
# sA2 <- sum_num_costs(oshpd16Primary, c(          "year", "sex", "ageG", "lev2"), "lev2") %>% mutate(county = STATE)
# sA3 <- sum_num_costs(oshpd16Primary, c(          "year", "sex", "ageG", "lev3"), "lev3") %>% mutate(county = STATE)
cA0 <- sum_num_costs(oshpd16Primary, c("county", "year", "sex", "ageG", "ccs_diagP"), "ccs0") 
# cA1 <- sum_num_costs(oshpd16Primary, c("county", "year", "sex", "ageG", "lev1"), "lev1")
# cA2 <- sum_num_costs(oshpd16Primary, c("county", "year", "sex", "ageG", "lev2"), "lev2") 
# cA3 <- sum_num_costs(oshpd16Primary, c("county", "year", "sex", "ageG", "lev3"), "lev3") 

total_sum_age <- bind_rows(sA0, cA0)

#data cleaning
total_sum_age <- filter(total_sum_age, !is.na(ageG)) #nothing removed
total_sum_age <- filter(total_sum_age, !is.na(county)) #removed records with missing county
#total_sum_age <- filter(total_sum_age, !is.na(CAUSE)) #remove missing cause?
#total_sum_age <- filter(total_sum_age, !is.na(sex)) #remove missing sex?

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

# countyAA contains age-adjusted hospitalization rates by CAUSE/level
# Now, need to address the fact that CAUSES may appear among females in a given county that don't appear among males, and vice versa as was dealt with above.
# Since the dataset total_sum_pop_new contains 0-value nhosp/charges placeholders, we can join the countyAA dataset with this dataset to "add in" this values, then convert the NA ahospRate into 0. 

# countyAA_new <- countyAA %>% full_join(total_sum_pop_new, by = c("year", "county", "sex", "CAUSE", "Level")) %>% filter(!is.na(CAUSE), !is.na(county))
# 
# countyAA_new <- countyAA_new %>% mutate(ahospRate = case_when(n_hosp != 0 ~ ahospRate, n_hosp == 0 ~ 0))  %>% select(-n_hosp, -charges, -ageG, -pop) 


countyAA_new <- countyAA


####------------------------------------------Creating new dataset to plot all metrics on same plot/facets---------------------------------------------------------#

# total_sum_pop_new = contains n_hosp and charges
# total_crude_rates = contains crude hospitalization and charge rates
# countyAA_new = contains age-adjusted hospitalization rates
# Will have to do a series of spread/gather/join to create dataset 

#calculated_sums       <- total_sum_pop_new %>% gather(key = "type", value = "measure", n_hosp, avg_los, charges, avgcharge, avgcharge_per_day, medcharge, medcharge_per_day)
calculated_sums        <- total_sum_pop     %>% gather(key = "type", value = "measure", n_hosp, avg_los, charges, avgcharge, avgcharge_per_day, medcharge, medcharge_per_day)

calculated_crude_rates <- total_crude_rates %>% gather(key = "type", value = "measure", cHospRate, cChargeRate)

calculated_aa_rates    <- countyAA_new %>%      gather(key = "type", value = "measure", ahospRate) 


calculated_metrics     <- bind_rows(calculated_sums, calculated_crude_rates, calculated_aa_rates) %>%
                            mutate(county  = ifelse(county== "California","CALIFORNIA",county),
                            diagnosis_var = "CCS-Beta") %>% 
                            select(sex, ccs_diagP, year, Level, county, ageG, pop, type, measure, diagnosis_var)

test <- calculated_metrics %>% filter(type == "charges") %>% filter(is.na(measure)) #for real data, all of the NA charges visits are male A08 values


#Saving RDS file of this dataframe
saveRDS(calculated_metrics, file = path(myPlace, "myData/",whichData,"/countyOSHPD.rds"))

#-------------------------------------------------Filtering only age-adjusted hospitalization rates from countyOSHPD, saving as separate rds file for map---------------------------#

# age_adjusted_hosp_rates <- calculated_metrics %>% filter(type == "ahospRate") %>% rename(ahospRate = measure) %>% select(-type)
# 
# saveRDS(age_adjusted_hosp_rates, file = path(myPlace, "myData/", whichData, "/ageadj_hospratesOSHPD.rds"))

#---------------------------------------------------------Exploring MDC and DRG Frequencies ---------------------------------------------------------------------------------------------------------














# library(DT)
# 
# # rows 1:26 are MDC codes/names and rows 27:781 are DRG codes/names
# 
# hdCodes   <- read.delim(paste0(upPlace,"/OSHPD/MDC_DRG.txt"), header = FALSE, sep = "=") 
# mdcNames  <- hdCodes[ 1:26,]   %>%  select(mdc=V1,  mdcNames=V2)
# drgNames  <- hdCodes[27:781,]  %>%  select(msdrg=V1,drgNames=V2)
# 
# hdCodes <- hdCodes %>% rename(mdc_drg_codes = V1, names = V2) %>% mutate(mdc_drg_codes = as.character(mdc_drg_codes), names = as.character(names))





##------------------------------------------------------------------------MDC dataset---------------------------------------------------------------------##
# mdc_state  <- sum_num_costs(oshpd16, c("year",           "mdc", "sex"), "") %>% select(-Level) %>% mutate(county = STATE)
# mdc_county <- sum_num_costs(oshpd16, c("year", "county", "mdc", "sex"), "") %>% select(-Level)
# 
# total_mdc  <- bind_rows(mdc_state, mdc_county) %>% 
#   filter(sex == "Male" | sex == "Female" | sex == "Total", !is.na(county)) %>% 
#   mutate(diagnosis_var = "mdc")






##------------------------------------------------------DRG dataset-------------------------------------------------------------------------------------------##

# drg_state  <- sum_num_costs(oshpd16, c("year",           "msdrg", "sex"), "") %>% select(-Level) %>% mutate(county = STATE)
# drg_county <- sum_num_costs(oshpd16, c("year", "county", "msdrg", "sex"), "") %>% select(-Level)
# 
# total_drg  <- bind_rows(drg_state, drg_county) %>% 
#   filter(sex == "Male" | sex == "Female" | sex == "Total", !is.na(county)) %>% 
#   mutate(diagnosis_var = "drg")





# Joining both together ----------------------------------------------------------------------------------------------------------------


## PLANS????
## ---have "Total" sex.... for whenever total is needed; remember to exclude when needed


# total_mdc_drg <- full_join(total_mdc, total_drg, by = c("year", "sex", "n_hosp", "charges", "avgcharge", "county", "diagnosis_var", "medcharge", "avg_los", "avgcharge_per_day", "medcharge_per_day", "mdc" = "msdrg")) %>% rename(mdc_drg_codes = mdc)









#-------------------------------------------------Creating 0 level values for discordant gender pairs---------------------------------------------------------------#

# mdc_drg_female_sum <-total_mdc_drg %>% filter(sex == "Female") %>% tibble::rowid_to_column() %>% spread(., sex, mdc_drg_codes) %>% select(year, Female, county, diagnosis_var)  #summarises all the CAUSES for females, by county and level
# 
# mdc_drg_male_sum <- total_mdc_drg %>% filter(sex == "Male") %>% tibble::rowid_to_column() %>% spread(., sex, mdc_drg_codes) %>% select(year, Male, county, diagnosis_var) #summarises all the CAUSES for male, by county and level
# 
# 
# mdc_drg_female_only <- anti_join(mdc_drg_female_sum, mdc_drg_male_sum, by = c("Female" = "Male", "year", "county", "diagnosis_var")) #These are the mdc-county pairs that are in the female dataset but not the male dataset
# 
# mdc_drg_male_only <- anti_join(mdc_drg_male_sum, mdc_drg_female_sum, by = c("Male" = "Female", "year", "county", "diagnosis_var")) #These are the mdc-county pairs that are in the male dataset but not the female dataset
# 
# #Now that we have these datasets, we need to add them back to their respective county_level spread dataset, convert NA to zero for n_hosp and charges. Gather, switch sex to opposite then join back with original total_sum_pop
# 
# mdc_drg_add_males <- gather(mdc_drg_female_only, key = "sex", value = "mdc_drg_codes", Female) %>% mutate(sex = "Male")
# 
# mdc_drg_add_females <- gather(mdc_drg_male_only, key = "sex", value = "mdc_drg_codes", Male) %>% mutate(sex = "Female")
# 
# 
# #Now joining original total_sum_pop with these datasets, replacing NA with zeros
# total_mdc_drg_new <- full_join(total_mdc_drg, mdc_drg_add_females, by = c("year", "county", "sex", "mdc_drg_codes", "diagnosis_var")) %>% full_join(., mdc_drg_add_males, by = c("year", "county", "sex", "mdc_drg_codes", "diagnosis_var"))
# 
# 
# #replacing year with 2016
# total_mdc_drg_new$year[is.na(total_mdc_drg_new$year)] <- 2016
# 
# #replacing NA for n_hosp, charges, avgcharge, medcharge, avg_los, avgcharge_per_day, medcharge_per_day  with 0
# total_mdc_drg_new$n_hosp[is.na(total_mdc_drg_new$n_hosp)] <- 0
# total_mdc_drg_new$charges[is.na(total_mdc_drg_new$charges)] <- 0
# total_mdc_drg_new$avgcharge[is.na(total_mdc_drg_new$avgcharge)] <- 0
# total_mdc_drg_new$medcharge[is.na(total_mdc_drg_new$medcharge)] <- 0
# total_mdc_drg_new$avgcharge_per_day[is.na(total_mdc_drg_new$avgcharge_per_day)] <- 0
# total_mdc_drg_new$medcharge_per_day[is.na(total_mdc_drg_new$medcharge_per_day)] <- 0
# total_mdc_drg_new$avg_los[is.na(total_mdc_drg_new$avg_los)] <- 0
# 
# #
# mdc_drg_sums <- total_mdc_drg_new %>% gather(key = "type", value = "measure", n_hosp, charges, avgcharge, medcharge, avgcharge_per_day, medcharge_per_day, avg_los) %>% select(year, mdc_drg_codes, sex, county, diagnosis_var, type, measure)
# 
# mdc_drg_sums$county[mdc_drg_sums$county == "California"] <- "CALIFORNIA"
# 
# 
# #Saving RDS file of this dataframe
# saveRDS(mdc_drg_sums, file = path(myPlace, "myData/",whichData,"/mdc_drg.rds"))


#-----------------------------------------------JOINING MDC/DRG SUMMARY DATA WITH ICD-10-CM SUMMARY DATA---------------------------------------------------------#

# mcs - delete all thes comments except for issue summary


#-->CD 8/24/19:This is where the error arises:
# total_mdc_drg_new formatting doesn't match calculated_metrics formatting--gather hasn't been implemented (12 columns vs 10 columns in calculated_metrics). 
# The number of observations is: calculated_metrics 22526 + total_mdc_drg_new 48769 = 71295 obs
# however, since there is only data in the "measure" and "type" columns (which are used in the function to generate the app viz) for icd10_cm data (calculated_metrics)
# (since those cols weren't created in the total_mdc_drg_new), the facet error appears when mdc/drg are selected
# full_oshpd_summary <- bind_rows(calculated_metrics, total_mdc_drg_new) %>% mutate(CAUSE = case_when(diagnosis_var == "icd10_cm" ~ CAUSE,
#diagnosis_var == "mdc" ~ mdc_drg_codes,
#diagnosis_var == "drg" ~ mdc_drg_codes))  #puts cause/mdc_drg codes in the same column 




# full_oshpd_summary <- bind_rows(calculated_metrics, mdc_drg_sums) %>% mutate(CAUSE = case_when(diagnosis_var == "icd10_cm" ~ CAUSE,
#                                                                                                diagnosis_var == "mdc" ~ mdc_drg_codes,
#                                                                                                diagnosis_var == "drg" ~ mdc_drg_codes))  #puts cause/mdc_drg codes in the same column 








#calculated_metrics = 22526
#mdc_drg_sums = 341383
#total = 363909

#full_oshpd_summary = 363909 -->this means that all observations are in the dataset

# mdc_drg
# total_mdc_drg_new

#Good OSHPD file: 364,139 observations
#Error OSHPD file: 71,295 observations

#Calculated_sums = 15,750
#Calculated_metrics = 22,526

#County_OSHPD = 22,526 -this is what is currently saved

#Mdc_drg_sums = 341,383 observations

#Resolved OSHPD full_oshpd_summary = 363,909







# saveRDS(full_oshpd_summary, file = path(myPlace, "myData/", whichData, "/full_oshpd_summary.rds"))






# --------------------------------------------------------------------------------------------------------------
# Joining fullCauseList and hdCodes into one reference dataset--maybe this can eventually be moved to global file/outside of function?:

# fullCauseList_ed       <- select(fullCauseList, LABEL, nameOnly) %>% rename(names = nameOnly)
# hdCodes_ed             <- hdCodes %>% rename(LABEL = mdc_drg_codes)
# full_CAUSE_mdcdrg_list <- bind_rows(fullCauseList_ed, hdCodes_ed)
# 
# write_csv(full_CAUSE_mdcdrg_list, path = paste0(myPlace, "/myInfo/fullCAUSE_mdcdrgicd.csv"))






#---------------------------------------------------------CALCULATING ANY VS PRIMARY DIAGNOSES------------------------------------------------------------------#

#--------------------------------------------------Writing function to create indicator variable for different conditions based on diagnosis codes-----------------------------#

# dataset = dataset of interest (in this case, oshpd16_sample)
# colname = what we want to name column, based on disease and whether diagnosis is based only on primary or any of 25 diagnosis codes (e.g. diabetes_any)
# icd_regEx = regEx for disease of interest, as defined in gdb.ICD.Map.xlsx
# index = variable indicating index we've defined: either 1 for diag_p (only primary diagnosis) or 1:25 for diag_p-odiag25 (any diagnosis code)
# index variables will have to be defined prior to running function--although this makes the code not quite "self-annotated", R
# doesn't seem to allow calling an index based on a range of variable names within a data.frame

# apply(X, Margin, function, ...) X = an array, including a matrix, Margin = vector giving the subscripts which the function will
#  be applied over. E.g. 1 indicates rows, 2 indicates columns, c(1,2) indicates rows and columns. Since we want the function
#  applied over rows (for multiple columns), we'll specify 1. 

##--------------CREATING DATASET WITH ICD CODES AND CORRESPONDING LABEL TO BE INPUT INTO FUNCTION FOR CREATING ANY DIAGNOSIS INDICATOR COLUMN----------#

# # defining paste function to include sep = "|"
# pastex <- function(...) { paste(..., sep = "|") }
# 
# # function for pasting all icd codes for a given label together in one variable
# p <- function(v) { Reduce(x = v, f=pastex) }
# 
# # Note that Reduce is a base R function, but there is also a purrr::reduce() function that apparently performs the same general
# #   purpose, but the input variables are set up somewhat differently. I haven't been able to successfully use purrr::reduce() instead of Reduce()
# 
# #creating input data
# test_map <- icd_map %>% mutate(LABEL = paste0(BG, PH)) %>% 
#   filter(!is.na(regExICD10_CM)) %>% 
#   group_by(LABEL) %>% 
#   mutate(newICDcode = p(regExICD10_CM)) %>% 
#   select(LABEL, newICDcode) %>% unique()



#--------------------------------------FUNCTION FOR CALCULATING ANY VS PRIMARY-------------------------------------------------------#

# oshpd16new <- oshpd16 %>% mutate(all_diag = paste(diag_p, odiag1, odiag2, odiag3, odiag4,
#                                                   odiag5, odiag6, odiag7, odiag8, odiag9,
#                                                   odiag10, odiag11, odiag12, odiag13, odiag14,
#                                                   odiag15, odiag16, odiag17, odiag18, odiag19, odiag20, odiag21,
#                                                   odiag22, odiag23, odiag24,sep = "|")) 

# This pastes all the ||| separators together at the end of the variable once the odiag columns contain NA--this doesn't seem to create a problem with the grepl statement below though. 


# This function loops through the LABEL codes in test_map, pulls out the ICD regex expression that matches that LABEL code, 
#  and then runs that regex expression against the all_diag variable (which contains all of the regex ICD codes for a given visit)
#  --using the mutate statement, a new indicator variable is created, named LABEL (eg if LABEL is A07, new column is named A07), 
# with 1 if any of LABEL ICD codes are present, 0 otherwise. 


# mcs why using dt here (i.e. := )

# any_diag_code <- function(df){
#   for (i in 1: nrow(test_map)){
#     label = test_map$LABEL[i]
#     code_def <- filter(test_map, LABEL == label) %>% pull(newICDcode)
#     df <- df %>% mutate(!!label := ifelse(grepl(code_def, all_diag), 1, 0))
#   }
#   return(df) #need to include return df in order to have the df return with multiple  new columns appended to it (otherwise it returns a null value)
#   
# }
# 
# #Need to define oshpd16an_primary as oshpd16new, and then use it as input variable so that the new columns all append onto the dataframe
# oshpd16any_primary <- oshpd16new
# oshpd16any_primary <- any_diag_code(oshpd16any_primary)


#-----Summarizing oshpd any vs primary data-------#

# MCS
# summarize_at below....NICEEEEEEE! and with gather!!!!!!!!!!!!

# AND NEED TO ADD ANY NEW CONDITIONS HERE!!!!!!!!
# A07:D10 how to genralize this?

# summary_any dataframe contains the number of hospitalizations for which each CAUSE was one of any diagnosis codes 
#    (diag_p through odiag24), by sex (statewide numbers) 



summary_any_CA <-  oshpd16any_primary %>% 
  group_by(sex) %>% 
  summarise_at(vars(A07:D10), sum) %>% 
  gather(key = LABEL, value = n_hosp, A07: D10) %>% 
  mutate(county = STATE) %>% 
  filter(sex == "Female" | sex == "Male" | sex == "Total") %>% 
  left_join(., select(icd_map, nameOnly, LABEL), by = c("LABEL"))

#summary_any_county dataframe contains the number of hospitalizations for which each CAUSE was one of any diagnosis codes, by county and sex

# summary_any_county <- oshpd16any_primary %>% 
#   group_by(sex, county) %>% 
#   summarise_at(vars(A07:D10), sum) %>% 
#   gather(key = LABEL, value = n_hosp, A07:D10) %>% 
#   filter(sex == "Female" | sex == "Male" | sex == "Total") %>% 
#   left_join(., select(icd_map, nameOnly, LABEL), by = c("LABEL"))
# 
# 
# # MCS combine two things above with
# # oshpd16any_primary_both <- rbind(oshpd16any_primary, mutate(oshpd16any_primary,county=STATE ) )
# # then lines 715-719 ??
# # if so, this could (should?) be used elsewhere in CCB
# 
# 
# summary_any <- bind_rows(summary_any_CA, summary_any_county) %>% 
#   mutate(diag_type = "any", year = 2016) %>% 
#   rename(CAUSE = LABEL)
# 
# summary_any$county[summary_any$county == "California"] <- "CALIFORNIA"
# 
# #------------To plot comparisons of any vs primary diagnoses, need to join primary diagnoses n_hosp summary data with summary_any:
# 
# total_primary <-   total_sum_pop_new %>% 
#   left_join(., fullCauseList, by = c("CAUSE" = "LABEL")) %>%
#   select(sex, CAUSE, year, n_hosp, Level, county, nameOnly) %>% 
#   filter(Level == "lev2") %>% 
#   mutate(diag_type = "primary") %>% select(-Level)
# 
# total_primary$county[total_primary$county == "California"] <- "CALIFORNIA"
# 
# any_primary <- bind_rows(summary_any, total_primary)
# 
# #calculating difference between any n_hosp and primary n_hosp in order to plot as a stacked bar plot (since primary is a subset of any in this dataset)
# any_primary_diff <-  any_primary %>% 
#   spread(., diag_type, n_hosp) %>% 
#   group_by(sex, CAUSE, county, year) %>% 
#   mutate(any_diff = any - primary) %>% #calculates difference between any and primary n_hosp 
#   gather(key = "diag_type", value = "n_hosp", primary, any_diff) %>% #gathers so we have n_hosp and diag_type columns again 
#   select(-any) #removes primary column
# 
# #Saving RDS file of primary-any dataframe for stacked bar plot
# saveRDS(any_primary_diff, file = path(myPlace, "myData/",whichData,"/any_primary_stackedbar.rds"))


#----------------------------------Any-primary comparisons---------------------------------------------#

# mcs -- pretty sure this is not needed... can just do line 773?

# codeLast4 <- str_sub(oshpd16any_primary$icdCODE,2,5) #puts characters 2-5 from the CODE string
# nLast4    <- nchar(codeLast4) #counts number of characters 
# 
# oshpd_test   <- oshpd16any_primary  %>% 
#   mutate(lev0X  = "0",
#          lev1X  = str_sub(icdCODE,2,2), #pulls out 2nd character in string--this is the capital letter (ie BG in full xlsx dataset)
#          lev2X  = str_sub(icdCODE,2,4), #pulls out 2nd, 3rd, 4th characters--this is the BG + PH in full xlsx dataset (equivalent to label if there is a label)
#          lev3X  = ifelse(nLast4 == 4,codeLast4,NA)   ) %>% 
#   select(-lev0, -lev1, -lev3)
# 
# 
# oshpd_test   <- oshpd16any_primary  %>%  select(-lev0, -lev1, -lev3)
# 
# 
# # and, per thinking above....
# 
# 
# oshpd_test <- bind_rows(       oshpd_test,
#                                mutate(oshpd_test,county=STATE))


#what any are associated with primaries?
#state level

# group_any_primary_state <-  oshpd_test %>% 
#                               group_by(lev2, sex) %>% 
#                               summarise_at(vars(A07:C05), sum) %>% 
#                               filter(!is.na(lev2)) %>% 
#                               left_join(., select(icd_map, nameOnly, LABEL), by = c("lev2" = "LABEL")) %>% 
#                               rename(primary_name = nameOnly, primary = lev2) %>% 
#                               gather(key = any, value = n_hosp_any, A07: C05) %>%
#                               left_join(., select(icd_map, nameOnly, LABEL), by = c("any" = "LABEL")) %>% 
#                               rename(any_name = nameOnly) %>% 
#                               mutate(county = STATE)
# 
# group_any_primary_state$county <- "CALIFORNIA"

#by county
# group_any_primary_county <- oshpd_test %>% 
#   group_by(lev2, county, sex) %>% 
#   summarise_at(vars(A07:C05), sum) %>% 
#   filter(!is.na(lev2)) %>% 
#   left_join(., select(icd_map, nameOnly, LABEL), by = c("lev2" = "LABEL")) %>% 
#   rename(primary_name = nameOnly, primary = lev2) %>% 
#   gather(key = any, value = n_hosp_any, A07: C05) %>% 
#   left_join(., select(icd_map, nameOnly, LABEL), by = c("any" = "LABEL")) %>% 
#   rename(any_name = nameOnly)

# # group_any_primary <- bind_rows(group_any_primary_state, group_any_primary_county) %>% filter(!is.na(county), !is.na(sex))
# group_any_primary <-  group_any_primary_county %>% filter(!is.na(county), !is.na(sex))
# 
# #Saving RDS file of this dataframe--shows the number of "any" diagnoses associated with each primary diagnosis
# saveRDS(group_any_primary, file = path(myPlace, "myData/",whichData,"/group_any_primary.rds"))



# mcs Delete below?

# #------------------------------------------------------------------------------------------------OLD OPTION--TAKES TOO LONG TO PROCESS----------------------------------------------------------#
# #-----------------------------FUNCTION FOR mapping icd code to diagnoses code variables--------------------------------#
# any_diagnosis_definition <- function(df, label) {
#   index <- grep("diag", colnames(df)) #gives the index of all cols with names that include diag in them, which is what we want to run the function over
#   df[[label]] <- apply(df, 1, FUN = function(x) {
#     icd_regEx <- filter(test_map, LABEL == label) %>% pull(newICDcode)
#     pattern <- grepl(icd_regEx, x)
#     if(any(pattern[(index)])) 1 else 0
#   } )
#   return(df)
# }
# 
# 
# #-----------------------------------Creating new dataset with the "any" columns------------------------------------------#
# #The problem with this is that it takes hours to process the real oshpd dataset (3 million + records) and append the new column to it. May need to 
# #re-write function so that new columns are created as separate dataframes, and then append all the columns to the oshpd dataset. 
# oshpd_test <- oshpd16 %>% select(-lev0, -lev1, -lev2, -lev3) #removing levels for primary diagnosis
# for (i in 1: nrow(test_map)) {
#   oshpd_test <- any_diagnosis_definition(oshpd_test,test_map$LABEL[i]) #need to make input df the name of the "final" output df in order to make sure each column is iteratively added, doesn't write over itself
#   
# } 
# 
# 


