# removed discharge date for now?
# https://oshpd.ca.gov/ml/v1/resources/document?rs:path=/Data-And-Reports/Documents/Submit/Patient-Level-Administrative/IP/IP-Total-Charges.pdf

# ====================================================================================================
# "E3.Process_OSHPD_data.R" file                                                                     |
#                                                                                                    |
#            Reads in OSHPD 2016 PDD sas file, saves as rds file (random sample)                     |
#            Used to assess hospitalizations for diabetes--primary diagnoses and all diagnoses.      |
#                                                                                                    |
#                                                                                                    |   
# ====================================================================================================


#---SET LOCATIONS-----------------------------------------------------------------------

# PROVIDE PATH FOR SECURE DATA HERE
# secure.location  <- "S:/CDCB/Demonstration Folder/Data/OSHPD/PDD/2016/"  # secure location of data
secure.location  <- "E:/0.Secure.Data/"

myDrive <- getwd()  #Root location of CBD project
myPlace <- paste0(myDrive,"/myCBD") 
upPlace <- paste0(myDrive,"/myUpstream")

whichData <- "fake"   # "real" or "fake"
newData  <- TRUE

# fullOSHPD <- FALSE
# sampleOSHPD <- TRUE

#-------------------------------------------------LOAD PACKAGES -----------------------------------------------------------------------------------------------------------------------------------#

library(tidyverse)
library(haven)
library(fs)
library(readxl)
library(epitools)


#-----------------------------LOADING/CREATING OSHPD DATASET FROM ORIGINAL DATA FILE----------------------------------------------------------------------------------------#



if(newData) {

#Reading in oshpd 2016 PDD file
# oshpd16  <- read_sas("S:\\CDCB\\Demonstration Folder\\Data\\OSHPD\\PDD\\2016\\cdph_pdd_ssn2016.sas7bdat") 
oshpd16  <- read_sas(paste0(secure.location,"rawOSHPD/cdph_pdd_rln2016.sas7bdat") )


#Subset with only variables of interest
oshpd_subset  <- select(oshpd16,diag_p, odiag1, odiag2, odiag3, odiag4, odiag5, odiag6, odiag7, odiag8, odiag9, odiag10, odiag11, odiag12, odiag13, odiag14, odiag15, odiag16, odiag17, odiag18, odiag19, odiag20, odiag21, odiag22, odiag23, odiag24, mdc, msdrg, charge, pay_cat, pay_type, admtyr,  patcnty, patzip, sex, agyrdsch, race_grp, oshpd_id, los_adj, los) %>% mutate(year = 2016)
# dschdate,


#Saving subset as RDS file
saveRDS(oshpd_subset, file=path(secure.location, "myData/oshpd_subset.rds"))



#3% random sample, randomly permuted
set.seed(4)
#oshpd_sample <- sample_n(oshpd_subset, size = 0.01*nrow(oshpd_subset), replace = F)

sampN1 <- 0.01*nrow(oshpd_subset)  
sampN2 <- sampN1*2

half1  <- sample_n(oshpd_subset,sampN1)  # sample function from dplyr

p1           <- sample_n(oshpd_subset[,1:29],  sampN2)
p2           <- sample_n(oshpd_subset[,30:31], sampN2)
p3           <- sample_n(oshpd_subset[,32:40], sampN2)
p3$race_grp  <- NA
half2        <- cbind(p1,p2,p3)

oshpd_sample <- rbind(half1,half2)


#Now, create RDS file of whole SAS file and random sample of SAS file 

#saving rds file--only needs to be run once to initially create the file

# Saving random sample as RDS file
saveRDS(oshpd_sample, file = path(upPlace, "upData/oshpd16_sample.rds"))

} # END if(newData)


#--------------------------------------------------------------------LOAD AND PROCESS OSHPD DATA-----------------------------------------------------------------------------------------#


if (whichData == "real") {
  oshpd16 <- readRDS(file=path(secure.location, "myData/oshpd_subset.rds")) 
}

if (whichData == "fake") {
  oshpd16 <- readRDS(file=path(upPlace, "upData/oshpd16_sample.rds"))
}


##------------------------------------Reading in data mapping/linkage files--------------------#
#reading in gbd.ICD.excel file
icd_map <- read_excel(path(myPlace, "myInfo/gbd.ICD.Map.xlsx")) 

#reading in county-codes-to-names linkage files --oshpd codes map to column "cdphcaCountyTxt"
geoMap     <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/County Codes to County Names Linkage.xlsx")))

#sex categories
sex_num <- c("1", "2", "3", "4")
sex_cat <- c("Male", "Female", "Other", "Unknown")
OSHPD_sex <- cbind(sex_num, sex_cat) %>% as.data.frame() #Should I create an excel/csv file with this information? 


#DISCUSS***
#race categories
race_grp <- c("0", "1", "2", "3", "4", "5", "6")
race_cat <- c("Unknown/Invalid/blank","White-NH", "Black-NH", "Hisp", "Asian-PI", "Native American/Alaskan Native", "Other")
OSHPD_race_grp <- cbind(race_grp, race_cat) %>% as.data.frame() 


ageMap     <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/Age Group Standard and US Standard 2000 Population.xlsx"),sheet = "data"))

STATE <- "California" #Defining California to be included later in county population labelling/estimates (California represents total)

yF   <- 100000  # rate constant 
pop5 <- 5       # 5 years
pop1 <- 1       # 1 year

yearGrp <- "2013-2017"

criticalNum <- 11



#-----------------------------------------------------------------------------------LOAD AND PROCESS POPULATION DATA-----------------------------------------------------------------------#

# ungrouping important for subsequent data set merging
popTract         <- readRDS(path(upPlace,"/upData/popTract2013.RDS")) %>% ungroup() 
popTractSexAgeG  <- filter(popTract,ageG != "Total")

popTractSex      <- filter(popTract,ageG == "Total")
popCommSex       <- popTractSex     %>% group_by(yearG,county,comID,sex)      %>% summarise(pop=sum(pop))  %>% ungroup()  
popCommSexAgeG   <- popTractSexAgeG %>% group_by(yearG,county,comID,sex,ageG) %>% summarise(pop=sum(pop))  %>% ungroup() 

popCounty        <- readRDS(path(upPlace,"/upData/popCounty.RDS")) %>% ungroup() 
popCountySex     <- filter(popCounty,ageG == "Total")
popCountySexAgeG <- filter(popCounty,ageG != "Total")

popCounty.RACE        <- readRDS(path(upPlace,"/upData/popCounty_RE.RDS")) %>% ungroup() 
popCountySex.RACE     <- filter(popCounty.RACE,ageG == "Total")
popCountySexAgeG.RACE <- filter(popCounty.RACE,ageG != "Total")

popStandard         <- ageMap %>% mutate(ageG = paste0(lAge," - ",uAge))




#----------------------------------------------ADD AGE-GROUp VARIABLE---------------------------------------------------------#

aL            <-      ageMap$lAge     # lower age ranges
aU            <- c(-1,ageMap$uAge)    # upper age ranges, plus inital value of "-1" for lower limit
aLabs         <- paste(aL,"-",aU[-1]) # make label for ranges
aMark         <- findInterval(oshpd16$agyrdsch,aU,left.open = TRUE)  # vector indicating age RANGE value of each INDIVIDUAL age value



#----------------Map ICD-10-CM codes to GBD conditions-----------------------------------------------------------------------------------------#

allLabels <- sort(icd_map$LABEL[!is.na(icd_map$LABEL)]) #This sorts all of the LABEL variables that aren't missing (i.e. coded as NA)

mapICD    <- icd_map[!is.na(icd_map$CODE),c("CODE","regExICD10_CM")] #This creates a new object, mapICD, of all non-missing CODE variables, and the corresponding regEx10
#associated with them. This object will be used to assign CODE/LABELS to diagnoses later

#If as.data.frame() isn't included as the final step when reading in fullCauseList, fullCauseList is stored as a tbl.df(), which causes problems if match() is used to create a new variable in a dataset later on. 
fullCauseList     <- icd_map[!is.na(icd_map$causeList),c("LABEL","causeList","nameOnly")] %>% arrange(LABEL) %>% as.data.frame()
fullList          <- fullCauseList[,"LABEL"]
names(fullList)   <- fullCauseList[,"causeList" ]

#Function for mapping icd code to disease group

icdToGroup <- function(inputVectorICD10) {
  Cause   <- rep(NA,length(inputVectorICD10))
  for (i in 1:nrow(mapICD)) {Cause[grepl(mapICD[i,"regExICD10_CM"],inputVectorICD10)] <- mapICD[i,"CODE"] } 
  Cause}
#What this says is: for the length of the input vector, match the ICD10 regEx codes to the corresponding CODE in mapICD

#-----------------------------ADDING VARIABLES TO OSHPD DATASET---------------------------------------------#

#Adding age groups
oshpd16$ageG  <- aLabs[aMark] 

#Using icdToGroup Function to map icd codes from diag_p variable to disease group
oshpd16$icdCODE  <- icdToGroup(inputVectorICD10=oshpd16$diag_p) %>% as.character()

##This converts the NAs from characters to NA so subsequent code won't treat them as characters, which is necessary when using str_sub to create the lev1, lev2, lev3 columns
oshpd16$icdCODE[oshpd16$icdCODE == "NA"] <- NA

#This next section adds variables to the input vector, breaking down the CODE into up to 4 levels
codeLast4 <- str_sub(oshpd16$icdCODE,2,5) #puts characters 2-5 from the CODE string
nLast4    <- nchar(codeLast4) #counts number of characters 

oshpd16   <- oshpd16  %>% 
  mutate(lev0  = "0",
         lev1  = str_sub(icdCODE,2,2), #pulls out 2nd character in string--this is the capital letter (ie BG in full xlsx dataset)
         lev2  = str_sub(icdCODE,2,4), #pulls out 2nd, 3rd, 4th characters--this is the BG + PH in full xlsx dataset (equivalent to label if there is a label)
         lev3  = ifelse(nLast4 == 4,codeLast4,NA) 
         ) %>% 
  left_join(., select(geoMap,cdphcaCountyTxt,county=countyName), by = c("patcnty"= "cdphcaCountyTxt")) %>% mutate(race_grp = as.factor(race_grp)) %>%
  left_join(., OSHPD_race_grp, by = "race_grp") %>%
  left_join(., OSHPD_sex, by = c("sex" = "sex_num"))


#Adding Total in order to create total/statewide estimates (for grouping function later)
oshpd16sex <- mutate(oshpd16, sex_cat = "Total") #Adding 'Total' in order to work calculate values statewide (in grouping function later)
oshpd16 <- bind_rows(oshpd16, oshpd16sex) %>% select(-sex) %>% rename(., sex = sex_cat) #removing numerical coding of sex, renaming sex_cat as sex so it will map with population standards datasets

#Calculating charge/day from los_adj and charges

oshpd16 <- oshpd16 %>% mutate(charge_per_day = charge/los_adj)
###-------------------------------------------EXPLORATORY ANALYSIS OF LENGTH OF STAY AND CHARGES (REAL OSHPD DATA, NOT SAMPLE)----------------------------------------------#


#------------------------CHARGES------------------------------#
#histogram of charges
min(oshpd16$charge, na.rm = TRUE) #0
max(oshpd16$charge, na.rm = TRUE) #73,798,776

#total histogram
oshpd16 %>% ggplot(aes(x = charge)) + geom_histogram(bins = 100) + scale_x_continuous(labels = scales::comma)

#histogram, excluding max
oshpd16 %>% ggplot(aes(x = charge)) + geom_histogram(bins = 100, breaks = seq(0, 500000, by = 1000)) + scale_x_continuous(labels = scales::comma)

#histogram, removing 0 and 1 charges
oshpd16 %>% filter(charge != 0 & charge != 1) %>% ggplot(aes(x = charge)) + geom_histogram(bins = 100, breaks = seq(0, 500000, by = 1000)) + scale_x_continuous(labels = scales::comma)


# OSHPD CHARGE AND CHARGE_PER_TDAY 0 and 1 to NA --hospitals that don't report charges (eg Kaiser) are assigned charges of 0, pro bono cases as assigned charges of 1
#oshpd16_charge0 <- oshpd16 %>% filter(charge == 0)

#------------------LENGTH OF STAY-----------------------------------#

#histogram of charges
min(oshpd16$los_adj, na.rm = TRUE)
max(oshpd16$los_adj, na.rm = TRUE) #9373

#total histogram
oshpd16 %>% ggplot(aes(x = los_adj)) + geom_histogram(bins = 100) + scale_x_continuous(labels = scales::comma)

#histogram, excluding max
oshpd16 %>% ggplot(aes(x = los_adj)) + geom_histogram(bins = 100, breaks = seq(0, 5000, by = 100)) + scale_x_continuous(labels = scales::comma)

#histogram, removing 0 and 1 los_adj
oshpd16 %>% filter(los_adj != 1) %>% ggplot(aes(x = los_adj)) + geom_histogram(bins = 100, breaks = seq(0, 500, by = 100)) + scale_x_continuous(labels = scales::comma)

oshpd16 %>% filter(los_adj > 100) %>% nrow() #11132 greater than 100 days

oshpd16 %>% filter(los_adj > 200) %>% nrow() #4074 greater than 200 days

oshpd16 %>% filter(los_adj > 365) %>% nrow() #2046 greater than 1 year
 

#What should the exclusion cut-off be for los_adj? 365 days? Less than that? 

#----------------------------------------------------------------------------------------------------------------------------------------------#
#Some of these extreme los/charges may not even apply to the CAUSE/icdCodes that we're capturing though. Now only looking at values for our CAUSES of interest

oshpd16test <- oshpd16 %>% filter(!is.na(icdCODE)) 

#min charge
min(oshpd16test$charge) #0
#max charge
max(oshpd16test$charge) #62,982,371

#max los
max(oshpd16test$los_adj) #6995

#histogram of los 
oshpd16test %>%  ggplot(aes(x = los_adj)) + geom_histogram(bins = 100, breaks = seq(0, 600, by = 10)) + scale_x_continuous(labels = scales::comma)

#table of los_adj

los_table <- table(oshpd16test$los_adj) %>% as.data.frame() %>% rename(los_adj = Var1)

#total charges
charges_table <- table(oshpd16test$charge) %>% as.data.frame() %>% rename(charge = Var1)
#table of charges---Note that if charges are greater than the max seven digit input field size, they are listed as $9,999,999. Also, when a patient's length of stay is more than 365 days, only
#the last 365 days of charges are reported. HOWEVER, there are some charges with 8 digits listed, even though OSHPD info said standard format before December 2018 only have 7 digits? 
#https://oshpd.ca.gov/ml/v1/resources/document?rs:path=/Data-And-Reports/Documents/Submit/Patient-Level-Administrative/IP/IP-Total-Charges.pdf


#histogram of charges
oshpd16test %>% filter(charge != 1 & charge != 0) %>% ggplot(aes(x = charge)) + geom_histogram(bins = 100, breaks = seq(0, 7000000, by = 1000)) + scale_x_continuous(labels = scales::comma)

#what do charges 2 and 3 mean? Are they also some sort of pro bono/not real charges info? 





#-----------------------------*_*_*_*_*_*_*_*_*_*_

oshpd16$charge[oshpd16$charge == 0] <- NA #changing 0 and 1 charges (kaiser or pro-bono cases) to NA

oshpd16$charge[oshpd16$charge == 1] <- NA

oshpd16$charge_per_day[oshpd16$charge_per_day == 0] <- NA #changing 0 and 1 charges (kaiser or pro-bono cases) to NA

oshpd16$charge_per_day[oshpd16$charge_per_day == 1] <- NA


#-------------OSHPD CALCULATIONS FOR TOTAL VISITS/CHARGES AND CRUDE RATES------------------------------------------------------------------#


#Group_by_at
#Function to sum number of hospitalizations and charges, calculate mean charges, mean length of stay, and mean charge per day
sum_num_costs <- function(data, groupvar, levLab) {
  
  dat <- data %>% group_by_at(.,vars(groupvar)) %>% 
    summarize(n_hosp = n(), 
              charges = sum(charge, na.rm = TRUE), #this still converts cases where there was only 1 with NA charges to 0 for charges
              avgcharge = mean(charge, na.rm = TRUE),
              avg_los = mean(los_adj, na.rm = TRUE),
              avgcharge_per_day = mean(charge_per_day, na.rm = TRUE)) 
  
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
            hosp_rateLCI     = yF*pois.approx(n_hosp,yearN*pop, conf.level = 0.95)$lower,
            hosp_rateUCI     = yF*pois.approx(n_hosp,yearN*pop, conf.level = 0.95)$upper,
            cChargeRate = yF*charges/(yearN*pop),
            charge_rateLCI     = yF*pois.approx(charges,yearN*pop, conf.level = 0.95)$lower,
            charge_rateUCI     = yF*pois.approx(charges,yearN*pop, conf.level = 0.95)$upper)
}



#function to calculate age-adjusted hospitalization rates

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
s.lev0 <- sum_num_costs(oshpd16, c("sex", "lev0", "year"), "lev0")
s.lev1 <- sum_num_costs(oshpd16, c("sex", "lev1", "year"), "lev1") #top level
s.lev2 <- sum_num_costs(oshpd16, c("sex", "lev2", "year"), "lev2") #public health level
s.lev3 <- sum_num_costs(oshpd16, c("sex", "lev3", "year"), "lev3")
state_sum <- bind_rows(s.lev0, s.lev1, s.lev2, s.lev3)
state_sum$county <- STATE #California as "county" variable

#County
c.lev0 <- sum_num_costs(oshpd16, c("sex", "lev0", "county", "year"), "lev0")
c.lev1 <- sum_num_costs(oshpd16, c("sex", "lev1", "county", "year"), "lev1") #top level
c.lev2 <- sum_num_costs(oshpd16, c("sex", "lev2", "county", "year"), "lev2") #public health level
c.lev3 <- sum_num_costs(oshpd16, c("sex", "lev3", "county", "year"), "lev3") 
county_sum <- bind_rows(c.lev0, c.lev1, c.lev2, c.lev3)

#merging county and state
total_sum <- bind_rows(state_sum, county_sum) %>% as.data.frame()

total_sum_pop <- total_sum %>% filter(!is.na(CAUSE)) %>% left_join(., popCountySex, by = c("year", "sex", "county")) %>% filter(sex != "Unknown" & sex != "Other", !is.na(county)) ##removing unknown and other gender variables, NA county, and NA CAUSES?
#total_sum_pop doesn't have any lev3 data because CAUSE = NA for all lev3 in this situation (and information is identical to lev0)


#checking NA charges
total_sum_pop_NA <- total_sum_pop %>% filter(is.na(charges))

##All of the NA cases that are present in the dataset at this point are cases in which there was only one hospitalization for a given CAUSE-gender-county, and the charge happened to be 0/NA. All the others (ie where there were multiple
#cases for a given CAUSE-gender-county combination) in which some of the charges happened to be 0/NA are grouped where total charges is calculated by removing NA values. 

#----------------------------------------------------------------------------------------------------------------------------------------#
#The problem that now arises in total_sum_pop is that CAUSES may appear among females in a given county that don't appear among males, and vice versa, which will cause issues when trying to make visualizations and summarizations later, since the data isn't the same length. 
#To address this problem, we need to add in observations for the non-congruent CAUSES, and give them values of 0 for n_hosp and charges:


spread_female_sum_pop <-total_sum_pop %>% filter(sex == "Female") %>% spread(., sex, CAUSE) %>% select(year, Level, Female, county)  #summarises all the CAUSES for females, by county and level

spread_male_sum_pop <- total_sum_pop %>% filter(sex == "Male") %>% spread(., sex, CAUSE) %>% select(year, Level, Male, county) #summarises all the CAUSES for male, by county and level


female_only <- anti_join(spread_female_sum_pop, spread_male_sum_pop, by = c("Female" = "Male", "year", "county", "Level")) #These are the CAUSE-county-level pairs that are in the female dataset but not the male dataset

male_only <- anti_join(spread_male_sum_pop, spread_female_sum_pop, by = c("Male" = "Female", "year", "county", "Level")) #These are the CAUSE-county-level pairs that are in the male dataset but not the female dataset

#Now that we have these datasets, we need to add them back to their respective county_level spread dataset, convert NA to zero for n_hosp and charges. Gather, switch sex to opposite then join back with original total_sum_pop

add_males <- gather(female_only, key = "sex", value = "CAUSE", Female) %>% mutate(sex = "Male")

add_females <- gather(male_only, key = "sex", value = "CAUSE", Male) %>% mutate(sex = "Female")


#Now joining original total_sum_pop with these datasets, replacing NA with zeros
total_sum_pop_new <- full_join(total_sum_pop, add_females, by = c("year", "Level", "county", "sex", "CAUSE")) %>% full_join(., add_males, by = c("year", "Level", "county", "sex", "CAUSE"))

##The issue now is distinguishing between NA that really should be changed to 0s--i.e 0 n_hosp, so 0 charges, and the 0 charges that we defined as NA earlier--need to keep these as NA
#Example of replacing NA data in selected column
#dat$four[is.na(dat$four)] <- 0  https://stackoverflow.com/questions/13172711/replace-na-values-from-a-column-with-0-in-data-frame-r

#replacing year with 2016
total_sum_pop_new$year[is.na(total_sum_pop_new$year)] <- 2016

#replacing NA for n_hosp, charges, avgcharge with 0
total_sum_pop_new$charges[is.na(total_sum_pop_new$n_hosp)] <- 0 #only codes NA -> 0 for cases where n_hosp was NA, not a number of hospitalizations

total_sum_pop_new$avgcharge[is.na(total_sum_pop_new$n_hosp)] <- 0 #only codes 0 for cases where n_hosp was NA, not a real number of hospitalizations

total_sum_pop_new$avgcharge_per_day[is.na(total_sum_pop_new$n_hosp)] <- 0 #only codes 0 for cases where n_hosp was NA

total_sum_pop_new$n_hosp[is.na(total_sum_pop_new$n_hosp)] <- 0 #changing NA n_hosp to 0

#replacing NA in ageG with "Total"
total_sum_pop_new$ageG[is.na(total_sum_pop_new$ageG)] <- "Total"


#Now we will  re-join the population dataset to make sure that the new 0-value female/male variables have associated populations

total_sum_pop_new <- total_sum_pop_new %>% left_join(., popCountySex, by = c("year", "county", "sex", "ageG")) %>% select(-pop.x) %>% rename(pop = pop.y)


total_crude_rates <- calculate_crude_rates(total_sum_pop_new, yearN = 1)



#-----------------------------------------------AGE ADJUSTED ("AA") RATES-----------------------------------------------------------------------------------------------------------------#

if (1==2) {
# makes dataframe of all possible combinations of county, year, CAUSE, and ageG 

year     <- data.frame(year     = 2000:2017) # these "vectors" need to be dataframes for the sq merge below to work
yearG    <- data.frame(yearG    = yearGrp)
yearG3   <- data.frame(yearG3   = sort(unique(cbdDat0$yearG3)))
CAUSE1   <- data.frame(CAUSE    = allLabels) 
CAUSE2   <- data.frame(CAUSE    = CAUSE1[nchar(as.character(CAUSE1$CAUSE)) < 4,])
CAUSE3   <- data.frame(CAUSE    = CAUSE1[nchar(as.character(CAUSE1$CAUSE)) < 2,])
sex      <- data.frame(sex      = c("Male","Female","Total"))
ageG     <- data.frame(ageG     = sort(unique(cbdDat0$ageG)))
county   <- data.frame(county   = c(geoMap$countyName,"California"))         
comID    <- data.frame(comID    = unique(cbdLinkCA[,"comID"]))
GEOID    <- data.frame(GEOID    = cbdLinkCA[,"GEOID"])
raceCode <- data.frame(raceCode = sort(unique(cbdDat0$raceCode)))

# other cool approach from Adam:
# fullMatCounty <- Reduce(function(...) merge(..., all = TRUE), list(county, year, CAUSE, sex, ageG))
fullMatCounty <- sqldf(" select * from  county cross join year  cross join CAUSE1 cross join sex cross join ageG")
# fullMatComm   <- sqldf(" select * from  comID  cross join yearG cross join CAUSE2 cross join sex cross join ageG")
# fullMatTract  <- sqldf(" select * from  GEOID  cross join yearG cross join CAUSE3 cross join sex cross join ageG")


#######CAUSE CHARACTER##################

fullMatCounty <- mutate(fullMatCounty, county = as.character(county),                             CAUSE = as.character(CAUSE), sex = as.character(sex), ageG   = as.character(ageG), tester = 0)
#fullMatComm   <- mutate(fullMatComm,   comID  = as.character(comID), yearG = as.character(yearG), CAUSE = as.character(CAUSE), sex = as.character(sex), ageG   = as.character(ageG), tester = 0)
#fullMatTract  <- mutate(fullMatTract,  GEOID  = as.character(GEOID), yearG = as.character(yearG), CAUSE = as.character(CAUSE), sex = as.character(sex), ageG   = as.character(ageG), tester = 0)

#What's the purpose of all of the above code? --> creates a matrix of all possible combinations--needed at an intermediate stage

}


#---------------------Age-adjusted hospitalizations (county and statewide)-----------------------------------------------------#
#Using summary function that was already created 

sA0 <- sum_num_costs(oshpd16, c("year", "sex", "ageG", "lev0"), "lev0") %>% mutate(county = STATE)
sA1 <- sum_num_costs(oshpd16, c("year", "sex", "ageG", "lev1"), "lev1") %>% mutate(county = STATE)
sA2 <- sum_num_costs(oshpd16, c("year", "sex", "ageG", "lev2"), "lev2") %>% mutate(county = STATE)
sA3 <- sum_num_costs(oshpd16, c("year", "sex", "ageG", "lev3"), "lev3") %>% mutate(county = STATE)
cA0 <- sum_num_costs(oshpd16, c("county", "year", "sex", "ageG", "lev0"), "lev0") 
cA1 <- sum_num_costs(oshpd16, c("county", "year", "sex", "ageG", "lev1"), "lev1")
cA2 <- sum_num_costs(oshpd16, c("county", "year", "sex", "ageG", "lev2"), "lev2") 
cA3 <- sum_num_costs(oshpd16, c("county", "year", "sex", "ageG", "lev3"), "lev3") 

total_sum_age <- bind_rows(sA0, sA1, sA2, sA3, cA0, cA1, cA2, cA3)

#data cleaning
total_sum_age <- filter(total_sum_age, !is.na(ageG)) #nothing removed
total_sum_age <- filter(total_sum_age, !is.na(county)) #removed records with missing county
#total_sum_age <- filter(total_sum_age, !is.na(CAUSE)) #remove missing cause?
#total_sum_age <- filter(total_sum_age, !is.na(sex)) #remove missing sex?


ageCounty <- full_join(total_sum_age, popCountySexAgeG, by = c("county", "year", "sex", "ageG")) %>% full_join(., popStandard[,c("ageG", "US2000POP")], by = "ageG") #joining population data and standard population data with summary hosp/charges by age and sex

#Now we have a dataset with the number of hospitalizations for each CAUSE, by county, age group, gender, and "level", as well as info about the population and standard population for each demographic group--this dataset will be use to calculate age-adjusted rates

#Calculating age-adjusted rates--why are we using ageadjust.direct.SAM instead of ageadjust.direct?

countyAA <- ageCounty %>% filter(!is.na(CAUSE)) %>% group_by(county, year, sex, CAUSE, Level) %>% summarize(ahospRate = ageadjust.direct.SAM(count = n_hosp, pop = pop, rate = NULL, stdpop = US2000POP, conf.level = 0.95)[2]*yF,
                                                                           aLCI = ageadjust.direct.SAM(count = n_hosp, pop = pop, rate = NULL, stdpop = US2000POP, conf.level = 0.95)[3]*yF,
                                                                           aUCI = ageadjust.direct.SAM(count = n_hosp, pop = pop, rate = NULL, stdpop = US2000POP, conf.level = 0.95)[4]*yF,
                                                                           aSE = ageadjust.direct.SAM(count = n_hosp, pop = pop, rate = NULL, stdpop = US2000POP, conf.level = 0.95)[5]*yF) %>% filter(year == "2016")

#countyAA contains age-adjusted hospitalization rates by CAUSE/level

#Now, need to address the fact that CAUSES may appear among females in a given county that don't appear among males, and vice versa as was dealt with above.

#Since the dataset total_sum_pop_new contains 0-value nhosp/charges placeholders, we can join the countyAA dataset with this dataset to "add in" this values, then convert the NA ahospRate into 0. 

countyAA_new <- countyAA %>% full_join(total_sum_pop_new, by = c("year", "county", "sex", "CAUSE", "Level")) %>% filter(!is.na(CAUSE), !is.na(county))


countyAA_new <- countyAA_new %>% mutate(ahospRate = case_when(n_hosp != 0 ~ ahospRate, n_hosp == 0 ~ 0))  %>% select(-n_hosp, -charges, -ageG, -pop) 


####------------------------------------------Creating new dataset to plot all metrics on same plot/facets---------------------------------------------------------#

#total_sum_pop_new = contains n_hosp and charges

#total_crude_rates = contains crude hospitalization and charge rates

#countyAA_new = contains age-adjusted hospitalization rates

#Will have to do a series of spread/gather/join to create dataset 


calculated_sums <- total_sum_pop_new %>% gather(key = "type", value = "measure", n_hosp, avg_los, charges, avgcharge, avgcharge_per_day)

calculated_crude_rates <- total_crude_rates %>% gather(key = "type", value = "measure", cHospRate, cChargeRate)

calculated_aa_rates <- countyAA_new %>% gather(key = "type", value = "measure", ahospRate) 


calculated_metrics <- bind_rows(calculated_sums, calculated_crude_rates, calculated_aa_rates) 

calculated_metrics$county[calculated_metrics$county == "California"] <- "CALIFORNIA"


test <- calculated_metrics %>% filter(type == "charges") #for real data, all of the NA charges visits are male A08 values


#Saving RDS file of this dataframe
saveRDS(calculated_metrics, file = path(myPlace, "myData/",whichData,"/countyOSHPD.rds"))


#---------------------------------------------------------Exploring MDC and DRG Frequencies ---------------------------------------------------------------------------------------------------------

library(DT)

# rows 1:26 are MDC codes/names and rows 27:781 are DRG codes/names

hdCodes   <- read.delim(paste0(upPlace,"/OSHPD/MDC_DRG.txt"), header = FALSE, sep = "=") 
mdcNames  <- hdCodes[ 1:26,]   %>%  select(mdc=V1,  mdcNames=V2)
drgNames  <- hdCodes[27:781,]  %>%  select(msdrg=V1,drgNames=V2)

hdCodes <- hdCodes %>% rename(mdc_drg_codes = V1, names = V2) %>% mutate(mdc_drg_codes = as.character(mdc_drg_codes), names = as.character(names))

##------------------------------------------------------------------------MDC dataset---------------------------------------------------------------------##
mdc_state <- sum_num_costs(oshpd16, c("year", "mdc", "sex"), "") %>% select(-Level) %>% mutate(county = STATE)

mdc_county <- sum_num_costs(oshpd16, c("year", "county", "mdc", "sex"), "") %>% select(-Level)

total_mdc <- bind_rows(mdc_state, mdc_county) %>% filter(sex == "Male" | sex == "Female" | sex == "Total", !is.na(county)) %>% mutate(diagnosis_var = "mdc")


##------------------------------------------------------DRG dataset-------------------------------------------------------------------------------------------##

drg_state <- sum_num_costs(oshpd16, c("year", "msdrg", "sex"), "") %>% select(-Level) %>% mutate(county = STATE)

drg_county <- sum_num_costs(oshpd16, c("year", "county", "msdrg", "sex"), "") %>% select(-Level)

total_drg <- bind_rows(drg_state, drg_county) %>% filter(sex == "Male" | sex == "Female" | sex == "Total", !is.na(county)) %>% mutate(diagnosis_var = "drg")

#Joining both together
total_mdc_drg <- full_join(total_mdc, total_drg, by = c("year", "sex", "n_hosp", "charges", "avgcharge", "county", "diagnosis_var", "mdc" = "msdrg"))


#-------------------------------------------------Creating 0 level values for discordant gender pairs---------------------------------------------------------------#

mdc_drg_female_sum <-total_mdc_drg %>% filter(sex == "Female") %>% tibble::rowid_to_column() %>% spread(., sex, mdc) %>% select(year, Female, county, diagnosis_var)  #summarises all the CAUSES for females, by county and level

mdc_drg_male_sum <- total_mdc_drg %>% filter(sex == "Male") %>% tibble::rowid_to_column() %>% spread(., sex, mdc) %>% select(year, Male, county, diagnosis_var) #summarises all the CAUSES for male, by county and level


mdc_drg_female_only <- anti_join(mdc_drg_female_sum, mdc_drg_male_sum, by = c("Female" = "Male", "year", "county", "diagnosis_var")) #These are the mdc-county pairs that are in the female dataset but not the male dataset

mdc_drg_male_only <- anti_join(mdc_drg_male_sum, mdc_drg_female_sum, by = c("Male" = "Female", "year", "county", "diagnosis_var")) #These are the mdc-county pairs that are in the male dataset but not the female dataset

#Now that we have these datasets, we need to add them back to their respective county_level spread dataset, convert NA to zero for n_hosp and charges. Gather, switch sex to opposite then join back with original total_sum_pop

mdc_drg_add_males <- gather(mdc_drg_female_only, key = "sex", value = "mdc", Female) %>% mutate(sex = "Male")

mdc_drg_add_females <- gather(mdc_drg_male_only, key = "sex", value = "mdc", Male) %>% mutate(sex = "Female")


#Now joining original total_sum_pop with these datasets, replacing NA with zeros
total_mdc_drg_new <- full_join(total_mdc_drg, mdc_drg_add_females, by = c("year", "county", "sex", "mdc", "diagnosis_var")) %>% full_join(., mdc_drg_add_males, by = c("year", "county", "sex", "mdc", "diagnosis_var"))


#Example of replacing NA data in selected column
#dat$four[is.na(dat$four)] <- 0  https://stackoverflow.com/questions/13172711/replace-na-values-from-a-column-with-0-in-data-frame-r

#replacing year with 2016
total_mdc_drg_new$year[is.na(total_mdc_drg_new$year)] <- 2016

#replacing NA for n_hosp, charges, avgcharge with 0
total_mdc_drg_new$n_hosp[is.na(total_mdc_drg_new$n_hosp)] <- 0

total_mdc_drg_new$charges[is.na(total_mdc_drg_new$charges)] <- 0

total_mdc_drg_new$avgcharge[is.na(total_mdc_drg_new$avgcharge)] <- 0

mdc_drg_sums <- total_mdc_drg_new %>% gather(key = "type", value = "measure", n_hosp, charges, avgcharge)

mdc_drg_sums$county[mdc_drg_sums$county == "California"] <- "CALIFORNIA"


#Saving RDS file of this dataframe
saveRDS(mdc_drg_sums, file = path(myPlace, "myData/",whichData,"/MDC_DRG.rds"))


#---------------------------------------------------------CALCULATING ANY VS PRIMARY DIAGNOSES------------------------------------------------------------------#

#--------------------------------------------------Writing function to create indicator variable for different conditions based on diagnosis codes-----------------------------#

#dataset = dataset of interest (in this case, oshpd16_sample)
#colname = what we want to name column, based on disease and whether diagnosis is based only on primary or any of 25 diagnosis codes (e.g. diabetes_any)
#icd_regEx = regEx for disease of interest, as defined in gdb.ICD.Map.xlsx
#index = variable indicating index we've defined: either 1 for diag_p (only primary diagnosis) or 1:25 for diag_p-odiag25 (any diagnosis code)
#index variables will have to be defined prior to running function--although this makes the code not quite "self-annotated", R
#doesn't seem to allow calling an index based on a range of variable names within a data.frame

#apply(X, Margin, function, ...) X = an array, including a matrix, Margin = vector giving the subscripts which the function will
#be applied over. E.g. 1 indicates rows, 2 indicates columns, c(1,2) indicates rows and columns. Since we want the function
#applied over rows (for multiple columns), we'll specify 1. 

##--------------CREATING DATASET WITH ICD CODES AND CORRESPONDING LABEL TO BE INPUT INTO FUNCTION FOR CREATING ANY DIAGNOSIS INDICATOR COLUMN----------#

#defining paste function to include sep = "|"
pastex <- function(...) {
  paste(..., sep = "|")
}

#function for pasting all icd codes for a given label together in one variable
p <- function(v) {
  Reduce(x = v, f=pastex)
}
#Note that Reduce is a base R function, but there is also a purrr::reduce() function that apparently performs the same general
#purpose, but the input variables are set up somewhat differently. I haven't been able to successfully use purrr::reduce() instead of Reduce()


#creating input data
test_map <- icd_map %>% mutate(LABEL = paste0(BG, PH)) %>% filter(!is.na(regExICD10_CM)) %>% group_by(LABEL) %>% mutate(newICDcode = p(regExICD10_CM)) %>% select(LABEL, newICDcode) %>% unique()



#--------------------------------------FUNCTION FOR CALCULATING ANY VS PRIMARY-------------------------------------------------------#


#creating a new variable where all codes are pasted together 

paste_stop <- function(df,...){
for(i in 1:25) {
  paste(..., sep = "|") 
    if (is.na(df$...)) {break}
  } 
}
#this ^ doesn't work

oshpd16new <- oshpd16 %>% mutate(all_diag = paste(diag_p, odiag1, odiag2, odiag3, odiag4,
                                                  odiag5, odiag6, odiag7, odiag8, odiag9,
                                                  odiag10, odiag11, odiag12, odiag13, odiag14,
                                                  odiag15, odiag16, odiag17, odiag18, odiag19, odiag20, odiag21,
                                                  odiag22, odiag23, odiag24,sep = "|")) 

#This pastes all the ||| separators together at the end of the variable once the odiag columns contain NA--this doesn't seem to create a problem with the grepl statement below though. 


#This function loops through the LABEL codes in test_map, pulls out the ICD regex expression that matches that LABEL code, and then runs that regex expression against the all_diag variable (which contains
#all of the regex ICD codes for a given visit)--using the mutate statement, a new indicator variable is created, named LABEL (eg if LABEL is A07, new column is named A07), with 1 if any of LABEL ICD codes
#are present, 0 otherwise. 

any_diag_code <- function(df){
  for (i in 1: nrow(test_map)){
    label = test_map$LABEL[i]
    code_def <- filter(test_map, LABEL == label) %>% pull(newICDcode)
    df <- df %>% mutate(!!label := ifelse(grepl(code_def, all_diag), 1, 0))
    }
  return(df) #need to include return df in order to have the df return with multiple  new columns appended to it (otherwise it returns a null value)
  
  }

#Need to define oshpd16test2 as oshpd16new, and then use it as input variable so that the new columns all append onto the dataframe
oshpd16test2 <- oshpd16new
oshpd16test2 <- any_diag_code(oshpd16test2)


#-----Summarizing oshpd any vs primary data-------#

#summary_any dataframe contains the number of hospitalizations for which each CAUSE was one of any diagnosis codes (diag_p through odiag24), by county and sex. 
summary_any_CA <- oshpd16test2 %>% group_by(sex) %>% summarise_at(vars(A07:C05), sum) %>% gather(key = LABEL, value = n_hosp, A07: C05) %>% mutate(county = STATE) %>% 
  filter(sex == "Female" | sex == "Male" | sex == "Total") %>% left_join(., select(icd_map, nameOnly, LABEL), by = c("LABEL"))


summary_any_county <- oshpd16test2 %>% group_by(sex, county) %>% summarise_at(vars(A07:C05), sum) %>% gather(key = LABEL, value = n_hosp, A07:C05) %>% 
  filter(sex == "Female" | sex == "Male" | sex == "Total") %>% left_join(., select(icd_map, nameOnly, LABEL), by = c("LABEL"))


summary_any <- bind_rows(summary_any_CA, summary_any_county) %>% mutate(diag_type = "any", year = 2016) %>% rename(CAUSE = LABEL)

summary_any$county[summary_any$county == "California"] <- "CALIFORNIA"

#------------To plot comparisons of any vs primary diagnoses, need to join primary diagnoses n_hosp summary data with summary_any:

total_primary <- total_sum_pop_new %>% left_join(., fullCauseList, by = c("CAUSE" = "LABEL")) %>%
  select(sex, CAUSE, year, n_hosp, Level, county, nameOnly) %>% filter(Level == "lev2") %>% mutate(diag_type = "primary") %>% select(-Level)

total_primary$county[total_primary$county == "California"] <- "CALIFORNIA"

any_primary <- bind_rows(summary_any, total_primary)

#calculating difference between any n_hosp and primary n_hosp in order to plot as a stacked bar plot (since primary is a subset of any in this dataset)
any_primary_diff <- any_primary %>% spread(., diag_type, n_hosp) %>% group_by(sex, CAUSE, county, year) %>% mutate(any_diff = any - primary) %>% #calculates difference between any and primary n_hosp 
  gather(key = "diag_type", value = "n_hosp", primary, any_diff) %>% #gathers so we have n_hosp and diag_type columns again 
  select(-any) #removes primary column

#Saving RDS file of primary-any dataframe for stacked bar plot
saveRDS(any_primary_diff, file = path(myPlace, "myData/",whichData,"/any_primary_stackedbar.rds"))




#----------------------------------Any-primary comparisons---------------------------------------------#
codeLast4 <- str_sub(oshpd16test2$icdCODE,2,5) #puts characters 2-5 from the CODE string
nLast4    <- nchar(codeLast4) #counts number of characters 

oshpd_test   <- oshpd16test2  %>% 
  mutate(lev0  = "0",
         lev1  = str_sub(icdCODE,2,2), #pulls out 2nd character in string--this is the capital letter (ie BG in full xlsx dataset)
         lev2  = str_sub(icdCODE,2,4), #pulls out 2nd, 3rd, 4th characters--this is the BG + PH in full xlsx dataset (equivalent to label if there is a label)
         lev3  = ifelse(nLast4 == 4,codeLast4,NA) 
  ) %>% select(-lev0, -lev1, -lev3)

#what any are associated with primaries?
#state level
group_any_primary_state <- oshpd_test %>% group_by(lev2, sex) %>% summarise_at(vars(A07:C05), sum) %>% filter(!is.na(lev2)) %>% 
  left_join(., select(icd_map, nameOnly, LABEL), by = c("lev2" = "LABEL")) %>% rename(primary_name = nameOnly, primary = lev2) %>% gather(key = any, value = n_hosp_any, A07: C05) %>%
  left_join(., select(icd_map, nameOnly, LABEL), by = c("any" = "LABEL")) %>% rename(any_name = nameOnly) %>% mutate(county = STATE)

group_any_primary_state$county <- "CALIFORNIA"

#by county
group_any_primary_county <- oshpd_test %>% group_by(lev2, county, sex) %>% summarise_at(vars(A07:C05), sum) %>% filter(!is.na(lev2)) %>% 
  left_join(., select(icd_map, nameOnly, LABEL), by = c("lev2" = "LABEL")) %>% rename(primary_name = nameOnly, primary = lev2) %>% gather(key = any, value = n_hosp_any, A07: C05) %>% 
  left_join(., select(icd_map, nameOnly, LABEL), by = c("any" = "LABEL")) %>% rename(any_name = nameOnly)


group_any_primary <- bind_rows(group_any_primary_state, group_any_primary_county) %>% filter(!is.na(county), !is.na(sex))

#Saving RDS file of this dataframe--shows the number of "any" diagnoses associated with each primary diagnosis
saveRDS(group_any_primary, file = path(myPlace, "myData/",whichData,"/group_any_primary.rds"))



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

