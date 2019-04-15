# removed discharge date for now?


# ====================================================================================================
# "E3.Process_OSHPD_data.R" file                                                                     |
#                                                                                                    |
#            Reads in OSHPD 2016 PDD sas file, saves as rds file (random sample)                     |
#            Used to assess hospitalizations for diabetes--primary diagnoses and all diagnoses.      |
#                                                                                                    |
#                                                                                                    |   
# ====================================================================================================

# fingers crossed

#-- Set Locations Etc-----------------------------------------------------------------------

# PROVIDE PATH FOR SECURE DATA HERE
secure.location  <- "S:/CDCB/Demonstration Folder/Data/OSHPD/PDD/2016/"  # secure location of data
#secure.location  <- "E:/0.Secure.Data/"

myDrive <- getwd()  #Root location of CBD project
myPlace <- paste0(myDrive,"/myCBD") 
upPlace <- paste0(myDrive,"/myUpstream")

whichDat <- "fake"   # "real" or "fake"
newData  <- FALSE

# fullOSHPD <- FALSE
# sampleOSHPD <- TRUE

#-- Load Packages -------------------------------------------------------------

library(tidyverse)
library(haven)
library(fs)
library(readxl)
library(epitools)


#------------------------------------------------------------------------------


if(newData) {

#Reading in oshpd 2016 PDD file
oshpd16  <- read_sas("S:\\CDCB\\Demonstration Folder\\Data\\OSHPD\\PDD\\2016\\cdph_pdd_ssn2016.sas7bdat") 
oshpd16  <- read_sas(paste0(secure.location,"rawOSHPD/cdph_pdd_rln2016.sas7bdat") )


#Subset with only variables of interest
oshpd_subset  <- select(oshpd16,diag_p, odiag1, odiag2, odiag3, odiag4, odiag5, odiag6, odiag7, odiag8, odiag9, odiag10, odiag11, odiag12, odiag13, odiag14, odiag15, odiag16, odiag17, odiag18, odiag19, odiag20, odiag21, odiag22, odiag23, odiag24, mdc, charge, pay_cat, pay_type, admtyr,  patcnty, patzip, sex, agyrdsch, race_grp) %>% mutate(year = 2016)
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
p3           <- sample_n(oshpd_subset[,32:37], sampN2)
p3$race_grp  <- NA
half2        <- cbind(p1,p2,p3)

oshpd_sample <- rbind(half1,half2)


#Now, create RDS file of whole SAS file and random sample of SAS file 

#saving rds file--only needs to be run once to initially create the file

# Saving random sample as RDS file
saveRDS(oshpd_sample, file = path(upPlace, "upData/oshpd16_sample.rds"))

} # END if(newData)

#***************************************************************************************************************#
#Start code here if OSHPD 2016 subset has already been created:
#***************************************************************************************************************#

##------------------------------------Reading in data mapping/linkage files--------------------#
#reading in gbd.ICD.excel file}
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
popTractSex      <- filter(popTract,ageG == "Total")
popTractSexAgeG  <- filter(popTract,ageG != "Total")

popCommSex       <- popTractSex     %>% group_by(yearG,county,comID,sex)      %>% summarise(pop=sum(pop))  %>% ungroup()  
popCommSexAgeG   <- popTractSexAgeG %>% group_by(yearG,county,comID,sex,ageG) %>% summarise(pop=sum(pop))  %>% ungroup() 

popCounty        <- readRDS(path(upPlace,"/upData/popCounty.RDS")) %>% ungroup() 
popCountySex     <- filter(popCounty,ageG == "Total")
popCountySexAgeG <- filter(popCounty,ageG != "Total")

popCounty.RACE        <- readRDS(path(upPlace,"/upData/popCounty_RE.RDS")) %>% ungroup() 
popCountySex.RACE     <- filter(popCounty.RACE,ageG == "Total")
popCountySexAgeG.RACE <- filter(popCounty.RACE,ageG != "Total")

popStandard         <- ageMap %>% mutate(ageG = paste0(lAge," - ",uAge))


#--------------------------------------------------------------------LOAD AND PROCESS OSHPD DATA-----------------------------------------------------------------------------------------#


if (whichDat == "real") {
  oshpd16 <- readRDS(file=path(secure.location, "myData/oshpd_subset.rds")) #maybe change to secure location?  YES
}

if (whichDat == "fake") {
  oshpd16 <- readRDS(file=path(upPlace, "upData/oshpd16_sample.rds"))
}


#-----------------------------------------------Add Age-Group variable ---------------------------------------------------------#

aL            <-      ageMap$lAge     # lower age ranges
aU            <- c(-1,ageMap$uAge)    # upper age ranges, plus inital value of "-1" for lower limit
aLabs         <- paste(aL,"-",aU[-1]) # make label for ranges
aMark         <- findInterval(oshpd16$agyrdsch,aU,left.open = TRUE)  # vector indicating age RANGE value of each INDIVIDUAL age value
oshpd16$ageG  <- aLabs[aMark]                                   # make new "ageG" variable based on two objects above 


#----------------Map ICD-10-CM codes to GBD conditions-----------------------------------------------------------------------------------------#

allLabels <- sort(icd_map$LABEL[!is.na(icd_map$LABEL)]) #This sorts all of the LABEL variables that aren't missing (i.e. coded as NA)

mapICD    <- icd_map[!is.na(icd_map$CODE),c("CODE","regExICD10_CM")] #This creates a new object, mapICD, of all non-missing CODE variables, and the corresponding regEx10
#associated with them. This object will be used to assign CODE/LABELS to diagnoses later

#If as.data.frame() isn't included as the final step when reading in fullCauseList, fullCauseList is stored as a tbl.df(), which causes problems if match() is used to create a new variable in a dataset later on. 
fullCauseList     <- icd_map[!is.na(icd_map$causeList),c("LABEL","causeList","nameOnly")] %>% arrange(LABEL) %>% as.data.frame()
fullList          <- fullCauseList[,"LABEL"]
names(fullList)   <- fullCauseList[,"causeList" ]



#Function from death code R script by MS

icdToGroup <- function(inputVectorICD10) {
  Cause   <- rep(NA,length(inputVectorICD10))
  for (i in 1:nrow(mapICD)) {Cause[grepl(mapICD[i,"regExICD10_CM"],inputVectorICD10)] <- mapICD[i,"CODE"] } 
  Cause}
#What this says is: for the length of the input vector, match the ICD10 regEx codes to the corresponding CODE in mapICD



#Testing function on my test dataset
oshpd16$icdCODE  <- icdToGroup(inputVectorICD10=oshpd16$diag_p) %>% as.character()

oshpd16$icdCODE[oshpd16$icdCODE == "NA"] <- NA

##This converts the NAs from characters to NA so subsequent code won't treat them as characters, which is necessary when using str_sub to create the lev1, lev2, lev3 columns


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



#-------------Group by statement testing------------------------------------------------------------------#


#Group_by_at
#Function to sum number of hospitalizations and charges 
sum_num_costs <- function(data, groupvar, levLab) {
  
  dat <- data %>% group_by_at(.,vars(groupvar)) %>% 
    summarize(n_hosp = n(), charges = sum(charge, na.rm = TRUE)) 
  
  names(dat)[grep("lev", names(dat))] <- "CAUSE"
  dat$Level                           <- levLab
  dat %>%  data.frame
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

total_sum_pop <- left_join(total_sum, popCountySex, by = c("year", "sex", "county"))












####Other info add here



















#calculating crude rates

total_crude_rates <- calculate_crude_rates(total_sum_pop, yearN = 1)



#--------------------------------------------RACE-ETHNICITY COUNTY (AND STATE SUMMARY) LEVEL SUMMARY DATA AND CRUDE RATES----------------------------------------------------#

#popCountySex.RE dataset has populations by year groups (eg 2000-2002) instead of single years, and the race/ethnicity groups are different from the race_grp codes in OSHPD--how should we go about race-ethnicity summary data sets?


#?


















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


#---------------------Age deaths (county and statewide)-------------------------------------
#Using summary function that was already created instead of doing group-by statements as was done in E1 R script? 

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

#********************************************************************************************************#
#-------Quick plot of charges------------------------------------------------------------------------------------------------------#
#************************************************************************************************************#

#-------------Writing function for visualizations. Variables that change: dataset (df), level (lev), county (cnty), and y variable (var) (e.g. charge, nhosp, rates), stratification variable (mygender)

#For join: LABEL in fullCauseList is our key, we want to match to CAUSE in total_sum 

oshpd_visualize <- function(df, lev, var, cnty, mygender){
  var <- enquo(var)
  df %>% left_join(., fullCauseList, by = c("CAUSE" = "LABEL")) %>% filter(!is.na(CAUSE), Level == lev, county == cnty) %>% group_by(sex) %>% 
    mutate(nameOnly = forcats::fct_reorder(nameOnly, filter(., sex == mygender) %>% pull(!!var))) %>% 
    ggplot(., aes(x = nameOnly, y = !!var)) + coord_flip() + geom_bar(stat = "identity") + facet_grid(sex ~., scales = "free_x")
  
}

#Example:
oshpd_visualize(total_sum, "lev1", charges, "California", "Total") #lev1, California, Total are in quotes and don't have to be enquo() within the function because they are the specific value of the variable Level that we're interested in. The var that represents the variable of interest
#does have to be put through enquo() at the beginning of the function because it is a variable name. 
#Notes on programming with dplyr and explanation of enquo(): https://dplyr.tidyverse.org/articles/programming.html


##Changing the county name will only work when the number of female and male cases are the same--if not, even with the group_by(sex) statement, we get the error message:  Evaluation error: length(f) == length(.x) is not TRUE.

plottest<- total_crude_rates %>% left_join(., fullCauseList, by = c("CAUSE" = "LABEL")) %>% filter(!is.na(CAUSE), Level == "lev2", county == "Fresno") %>% group_by(sex) %>%
  mutate(nameOnly = forcats::fct_reorder(nameOnly, filter(., sex == "Female") %>% pull(cChargeRate))) %>% 
  ggplot(., aes(x = nameOnly, y = cChargeRate)) + coord_flip() + geom_bar(stat = "identity") + facet_grid(sex ~., scales = "free_x") ##

#----------How to set up axis order based on charges for Total facet--------------------------------#

#Testing reorder--this works, although it is a base R way 
#reordered_factor <- reorder(s.lev2$CAUSE[s.lev2$sex == "Total"], s.lev2$charges[s.lev2$sex == "Total"])

#s.lev2$CAUSE <- factor(s.lev2$CAUSE, levels = levels(reordered_factor)) 


#s.lev2 %>% filter(., CAUSE != is.na(CAUSE)) %>% ggplot(., aes(x = CAUSE, y = charges)) + coord_flip() + geom_bar(stat = "identity") + facet_grid(. ~ sex,scales="free_x")



#Using forcats::fct_reorder--this allows you to reorder, but doesn't control the facet variable by which you want it ordered:
#s.lev2 %>% filter(CAUSE != is.na(CAUSE)) %>% mutate(CAUSE = forcats::fct_reorder(CAUSE, charges)) %>% ggplot(., aes(x = CAUSE, y = charges)) + coord_flip() + geom_bar(stat = "identity") + facet_grid(. ~ sex,scales="free_x")


#This uses fct_reorder, based on an example here: https://stackoverflow.com/questions/54458018/passing-string-variable-to-forcatsfct-reorder. 
s.lev2 %>% filter(CAUSE != is.na(CAUSE)) %>% group_by(sex) %>% mutate(CAUSE = forcats::fct_reorder(CAUSE, filter(., sex == "Total") %>% pull(charges))) %>% ggplot(., aes(x = CAUSE, y = charges)) + coord_flip() + geom_bar(stat = "identity") + facet_grid(. ~ sex,scales="free_x")

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------#



























#####CHARGES RANKING############
#SUMS-CHARGES

#total CA, s.lev1
total_sum %>% left_join(., fullCauseList, by = c("CAUSE" = "LABEL")) %>% filter(!is.na(CAUSE), Level == "lev1", county == "California") %>% group_by(sex) %>% mutate(nameOnly = forcats::fct_reorder(nameOnly, filter(., sex == "Total") %>% pull(charges))) %>% 
  ggplot(., aes(x = nameOnly, y = charges)) + coord_flip() + geom_bar(stat = "identity") + facet_grid(sex ~., scales = "free_x")


#total CA, s.lev2
total_sum %>% left_join(., fullCauseList, by = c("CAUSE" = "LABEL"))%>% filter(!is.na(CAUSE), Level == "lev2", county == "California") %>% group_by(sex) %>% mutate(nameOnly = forcats::fct_reorder(nameOnly, filter(., sex == "Total") %>% pull(charges))) %>% 
  ggplot(., aes(x = nameOnly, y = charges)) + coord_flip() + geom_bar(stat = "identity") + facet_grid(sex ~ ., scales = "free_x")


#county rankings, lev1--this isn't really a useful visual
total_sum %>% left_join(., fullCauseList, by = c("CAUSE" = "LABEL")) %>% filter(!is.na(CAUSE), Level == "lev1") %>% 
  ggplot(., aes(x = nameOnly, y = charges, fill = county)) + coord_flip() + geom_bar(stat = "identity", position = position_dodge()) 



#-------------Writing function for the above pipeline to simply. Variables that change: dataset, level and y variable (charge, nhosp, rates)--can also make county variable change?

oshpd_visualize <- function(df, lev, var){
  var <- enquo(var)
  df %>% left_join(., fullCauseList, by = c("CAUSE" = "LABEL")) %>% filter(!is.na(CAUSE), Level == lev, county == "California") %>% group_by(sex) %>% 
    mutate(nameOnly = forcats::fct_reorder(nameOnly, filter(., sex == "Total") %>% pull(!!var))) %>% 
  ggplot(., aes(x = nameOnly, y = !!var)) + coord_flip() + geom_bar(stat = "identity") + facet_grid(sex ~., scales = "free_x")

}

oshpd_visualize(total_sum, "lev1", "Fresno", charges) #lev1 is in quotes and doesn't have to be enquo() within the function because it is the specific value of the variable Level that we're interested in. The var that represents the variable of inter
#does have to be put through enquo() at the beginning of the function because it is a variable name. 


##Changing the county name will only work when the number of female and male cases are the same--if not, even with the group_by(sex) statement, we get the error message:  Evaluation error: length(f) == length(.x) is not TRUE.

total_crude_rates %>% left_join(., fullCauseList, by = c("CAUSE" = "LABEL")) %>% filter(!is.na(CAUSE), Level == "lev2", county == "Fresno") %>% group_by(sex) %>%
  mutate(nameOnly = forcats::fct_reorder(nameOnly, filter(., sex == "Female") %>% pull(cChargeRate))) %>% 
  ggplot(., aes(x = nameOnly, y = cChargeRate)) + coord_flip() + geom_bar(stat = "identity") + facet_grid(sex ~., scales = "free_x")

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------#


#CHARGE-RATES
#total CA, s.lev1
total_crude_rates %>% left_join(., fullCauseList, by = c("CAUSE" = "LABEL")) %>% filter(!is.na(CAUSE), Level == "lev1", county == "California") %>% group_by(sex) %>% 
  mutate(CAUSE = forcats::fct_reorder(nameOnly, filter(., sex == "Total") %>% pull(cChargeRate))) %>% 
  ggplot(., aes(x = nameOnly, y = cChargeRate)) + coord_flip() + geom_bar(stat = "identity") + facet_grid(. ~ sex, scales = "free_x")

























#---------------------------------------------------------Other------------------------------------------------------------------#
diabetes <- icd_map %>% filter(name == "C. Diabetes mellitus") %>% select(regExICD10_CM)

depression <- icd_map %>% filter(name == "a. Major depressive disorder" | name == "b. Dysthymia") %>% select(regExICD10_CM)
depression <- paste(depression[1,], depression[2,], sep = "|") %>% as.data.frame() #if we are including major depressive disorder and dysthmia
#together as one group, then we need to paste the regEx from the two conditions together

ischaemic_heart_disease <- icd_map %>% filter(name == "3. Ischaemic heart disease") %>% select(regExICD10_CM)

#--------------------------------------------------Writing function to create indicator variable for different conditions based on diagnosis codes-----------------------------

#dataset = dataset of interest (in this case, oshpd16_sample)
#colname = what we want to name column, based on disease and whether diagnosis is based only on primary or any of 25 diagnosis codes (e.g. diabetes_any)
#icd_regEx = regEx for disease of interest, as defined in gdb.ICD.Map.xlsx
#index = variable indicating index we've defined: either 1 for diag_p (only primary diagnosis) or 1:25 for diag_p-odiag25 (any diagnosis code)
#index variables will have to be defined prior to running function--although this makes the code not quite "self-annotated", R
#doesn't seem to allow calling an index based on a range of variable names within a data.frame

#apply(X, Margin, function, ...) X = an array, including a matrix, Margin = vector giving the subscripts which the function will
#be applied over. E.g. 1 indicates rows, 2 indicates columns, c(1,2) indicates rows and columns. Since we want the function
#applied over rows (for multiple columns), we'll specify 1. 

diagnosis_definition <- function(dataset, col_name, icd_regEx, index) {
  dataset[[col_name]] <- apply(dataset, 1, FUN = function(x) {
    pattern <- grepl(icd_regEx, x)
    if(any(pattern[(index)])) "1" else "0"
  } )
  dataset
}
#index_p = only primary diagnosis
index_p <- 1
#index_any = any diagnosis
index_any <- 1:25

oshpd_sample2 <- diagnosis_definition(oshpd16, "diabetes_p", diabetes, index_p) %>% diagnosis_definition(., "diabetes_any", diabetes, index_any)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------#


