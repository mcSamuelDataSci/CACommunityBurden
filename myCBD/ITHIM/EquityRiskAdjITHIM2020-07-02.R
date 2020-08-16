# EquityRiskAdjITHIM.R - Neil Maizlish -July 2, 2020
#
# Using small area estimates of social determinants of health at the census 
# this program calculates the age-sex-region-SDOH cause-specific mortality rates
# aggregated to California MPO regions for equity analyses in California ITHIM.

# Program reads 2011-2015 ACS data from Census API and calculates census
# population counts for income, poverty, education and other social 
# determinants of health in age-sex and California census strata, which are
# aggregated into ITHIM age groups (0-4, 5-14, 15-29, 30-44, 45-59,
# 60-69, 70-79, 80+) and regions that correspond to major MPOs in
# SF Bay Area, Sacramento Area, San Joaquin Valley, Southern California, and
# San Diego. The program calculated a disease-specific adjustment factor 
# given by disease-specific mortality rate ratio of the age-sex-region-SDOH
# mortality rate (i,j,k,j) and the age-sex-region mortality rate (i,j,k).
# The adjustment factor is multiplied by the cause-specific burden of disease
# to give the burden for a specific population subgroup (e.g. lowest income 
# quartile)

# Other inputs: 
# 1) Look-up table for counties to MPO regions (County2MPOlookup.csv)
# 2) community burden of disease file created by Michael Samuel located at
# "g:/CCB/0.Secure.Data/myData/cbdDat0FULL.rds at line 290
# User must set working directory in line 37

# 4 Output files: Burden of disease risk adjuster for region-age-sex-cause 
# Q1Inc_RiskAdjusterCCYY-MM-DD.csv (lowest income quartile), 
# Q2Inc_RiskAdjusterCCYY-MM-DD.csv, Q3Inc_RiskAdjusterCCYY-MM-DD.csv, 
# Q4Inc_RiskAdjusterCCYY-MM-DD.csv (highest income quartile)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialize Program 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load Census API package
library(censusapi)

# SET WORKING DIRECTORY AS NEEDED
#setwd("E:/RetiredAnnuitant/RCode")

# Add key to .Renviron
Sys.setenv(CENSUS_KEY="5d7c514fd7d4550eb569c573e10a13792eccad9b")

# Reload .Renviron
readRenviron("~/.Renviron")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Download age-sex population for California census tracts, ACS 2011-2015
# Age groups are in 22 bands <5, 5-9, 10-14, 15-17, 18-19, 20, 21, 22-24, 
# 25-29, 30-34, 35-39, 40-44, 45-49, 50-54, 55-59, 60-61, 62-64, 65-66, 67-69
# 70-74, 75-79, 80-84, 85+
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

agelist <- c("00","05","10","15","18","20","21","22","25","30","35","40",
              "45","50","55","60","62","65","67","70","75","80","85")

magelist <- paste0("M", agelist)
 
 # Males age variables, padded zeros
mlist <- paste0("B01001_0",sprintf("%02d",3:25),"E")

# Female age variables
flist <- paste0("B01001_0",27:49,"E")

# Age-Sex-tract count 2011-2015 to Match ITHIM baseline ~2010-2012
tract <- getCensus(name = "acs/acs5",
                    vintage  = 2015, 
                    vars     = c("NAME", mlist, flist), 
                    region   = "tract:*",
                    regionin = "state:06")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Download California census tracts for median household income, B19013_001E
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

income <- getCensus(name = "acs/acs5",
                    vintage = 2015, 
                    vars = c("NAME", "B19013_001E", "B19013_001M"), 
                    region = "tract:*",
                    regionin = "state:06")

# Recode ACS  -2222, -3333, -66666, -88888 etc. as missing
income$B19013_001E[income$B19013_001E < 0] <- NA

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create income quartiles (income4tile)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
income <- within(income, income4tile <- cut(B19013_001E, 
                  quantile(B19013_001E, probs=seq(0,1, by=0.25), na.rm=TRUE), 
                  include.lowest=TRUE, labels=FALSE))

# Check on cutpoints and missing data
summary(income$B19013_001E)
summary(income$income4tile)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merge age-sex pop counts with income quartile 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

income <- merge(tract, income, by = c("state", "county", "tract"), all.x = TRUE)

# create county fips code so counties can ge aggregated into MPO regions
income$County_FIPS <- as.numeric(paste0(income$state,income$county))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read County to MPO look-up file
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

county2mpo <- read.csv(file= "County2MPOlookup.csv",  head = TRUE, sep = "," , 
                     stringsAsFactors = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merge with county to MPO lookup table
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

income <- merge(income, county2mpo, by = "County_FIPS", all.x = TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save census tractsfile for assignment of deaths by census tract and 
# income quartile
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
income_stub <- income[,c("state","county","tract","income4tile","Region")]
income_stub$CT <- paste0(income_stub$state,income_stub$county,
                 income_stub$tract)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Aggregate age and sex population counts by MPO (Region) & income quartile
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
income <- income[,c(6:51,55,57)]
region4tile <- aggregate(. ~ Region + income4tile, data=income, sum, 
               na.rm=TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rename age sex columns 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

agelist <- c("00","05","10","15","18","20","21","22","25","30","35","40",
             "45","50","55","60","62","65","67","70","75","80","85")
magelist <- paste0("M", agelist)
fagelist <- paste0("F", agelist)
colnames(region4tile)[3:48] <- c(magelist,fagelist)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Reshape file for age aggregation
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

region4tile_long <- reshape(region4tile,
             varying = c(magelist,fagelist),
             v.names = "pop",
             timevar = "agegroup",
             times = c(magelist,fagelist),
             new.row.names = 1:1288,
             direction = "long")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create separate age and sex fields
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

region4tile_long$Sex <- substr(region4tile_long$agegroup,1,1)
region4tile_long$agecat <- substr(region4tile_long$agegroup,2,3)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ITHIM Age groups
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Age Labels
ithim_age_groups <- function(x) { 
  if    (is.na(x)) y <- "NA"
  else if (x=="00")             y <- 0
  else if (x>="05" & x < "15")  y <- 5
  else if (x>="15" & x < "30")  y <- 15
  else if (x>="30" & x < "45")  y <- 30
  else if (x>="45" & x < "60")  y <- 45
  else if (x>="60" & x < "70")  y <- 60
  else if (x>="70" & x < "80")  y <- 70
  else if (x>="80")             y <- 80
  else y <- NA
  return(y)
}

# Apply ITHIM Age Group labels
region4tile_long$Age  <- sapply(region4tile_long$agecat,ithim_age_groups)

# Apply ITHIM Sex categories M=1, F=2
region4tile_long$Sex <- ifelse(region4tile_long$Sex =="M", 1, 2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Aggregate age and sex variables by MPO and income quartile
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

age_sex_region_income <- aggregate(pop ~ Region + income4tile + Sex + Age, 
                         data = region4tile_long, sum, na.rm=TRUE)

age_sex_ca_income <- aggregate(pop ~ income4tile + Sex + Age, 
                         data = region4tile_long, sum, na.rm=TRUE)

# Add California as a Region
age_sex_ca_income$Region <- "California"

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add California to Regions
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
age_sex_region_income <- rbind(age_sex_region_income, age_sex_ca_income)

# Re-order file
age_sex_region_income <- age_sex_region_income[order(
  age_sex_region_income$Region,
  age_sex_region_income$income4tile,
  age_sex_region_income$Sex,
  age_sex_region_income$Age),]

# Check on population total
# sum(age_sex_region_income[age_sex_region_income$Region=="SF Bay Area","pop"])

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Age-sex-region population for reference (denominator of risk adjuster)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

age_sex_region <- aggregate(pop ~ Region + Sex + Age, 
                            data = region4tile_long, sum, na.rm=TRUE)
age_sex_ca <- aggregate(pop ~ Sex + Age, 
                            data = region4tile_long, sum, na.rm=TRUE)
age_sex_ca$Region <- "California"
age_sex_region <- rbind(age_sex_region, age_sex_ca)

# Re-order file
age_sex_region <- age_sex_region[order(
  age_sex_region$Region,
  rev(age_sex_region$Sex),
  age_sex_region$Age),]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merge files income and reference files  (denominators of risk adjuster)
# pop.x = income quartile, pop.y is reference
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

denominator <- merge(age_sex_region_income, age_sex_region, 
                      by = c("Region", "Sex", "Age"), all.x=T)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Simulated Death Data for 13 ITHIM causes 
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# list of California census tracts
# ctlist <- paste0(income_stub$state,income_stub$county,income_stub$tract)
# icdlist <- c("I20", "I10", "I60", "E10", "F01", "F32", "C18", "C50",
#               "I30", "C33", "J00", "J10", "V01")
# causetitles <- c("Ischemic Heart Disease", "Hypertensive Heart Disease",  
#                 "Stroke", "Diabetes", "Dementia", "Depression", "Colon Cancer",
#                 "Breast Cancer", "Inflammatory Heart Disease", "Lung Cancer",
#                 "Respiratory diseases", "Acute resp infections",
#                 "Road Traffic Injuries")
# set.seed(1)
# Number of deaths 
# n <- 250000

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create random sample of age-sex counts deaths in 2011 to 2015 based
# on California Burden of Disease cbdDat0FULL format
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# deaths <-
#  data.frame(
#    "ID" = c(1:n),
#    "year" = c(2011, 2012, 2013, 2014, 2015),
#    "age" = 85 - as.integer(rexp(n, 1/60)), # somewhat exponential age increase
#    "sex" = sample(c("M","F"),  n, replace = TRUE),
#    "CT"  = sample(ctlist, n, replace = TRUE),
#    "ICD10" = sample(icdlist, n, replace = TRUE),
#    stringsAsFactors = F
#  )
# Top code age at death to 85 (highest age catgory 85+)
# deaths$Age <- ifelse(deaths$age < 0, 85, deaths$age)
# deaths$stateFIPS <- substr(deaths$CT,1,2)
# deaths$countyFIPS <- substr(deaths$CT,3,5)
# deaths$GEOID <- substr(deaths$CT,6,11)

# cbdDat0FULL <- deaths[, c("year", "GEOID", "countyFIPS", "stateFIPS",
#                         "age", "sex", "ICD10")]
# saveRDS(cbdDat0FULL, "cbdDat0FULL.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read cbdDat0FULL and format for ITHIM 
# EDIT the securePath and SecureDataFile as needed
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# securePath     <- "H:/CCB/0.Secure.Data/"
# securePath     <- "g:/CCB/0.Secure.Data/"
securePath     <- "g:/0.Secure.Data/myData/"
secureDataFile <- paste0(securePath,"cbdDat0FULL.R") 
# cbdDat0FULL <- readRDS(secureDataFile)

load(secureDataFile)



# Subset Years
deaths <- cbdDat0FULL[cbdDat0FULL$year %in% c(2011,2012,2013, 2014, 2015),]

# create census tract 
#deaths$CT <- with(deaths, paste0(stateFIPS,countyFIPS, GEOID))
deaths$CT <- with(deaths, paste0(stateCode,countyCode, tractCode))

# create 3-digit ICD Major Cause group 
deaths$icd10chr3 <- substr(deaths$ICD10,1,3)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Assign ITHIM Age categories
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Age Labels
age_groups <- function(x) { 
  if    (is.na(x)) y <- "NA"
  else if (x< 5 )           y <- 0
  else if (x>=5  & x < 15)  y <- 5
  else if (x>=15 & x < 30)  y <- 15
  else if (x>=30 & x < 45)  y <- 30
  else if (x>=45 & x < 60)  y <- 45
  else if (x>=60 & x < 70)  y <- 60
  else if (x>=70 & x < 80)  y <- 70
  else if (x>=80)           y <- 80
  else y <- "Total"
  return(y)
}

# Label Age for ITHIM age categories
deaths$Age <- sapply(deaths$age,age_groups)

# Apply ITHIM Sex categories M=1, F=2
# Rename Sex to be consistent with ITHIM naming convention 
names(deaths)[names(deaths)=="sex"] <- "Sex"
deaths$Sex <- ifelse(deaths$Sex =="M", 1, 2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Assign ITHIM Cause categories
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cause labels
cause_groups <- function(x) { 
  if    (is.na(x)) y <- "NA"
  else if (x == "C18")      y <- "Colon Cancer"
  else if (x == "C50")      y <- "Breast Cancer"
  else if (x >= "I10" & x <= "I13")  y <- "Hypertensive Heart Disease"
  else if (x >= "I20" & x <= "I25")  y <- "Ischemic Heart Disease"
  else if (x >= "I60" & x <= "I69")  y <- "Stroke"
  else if (x %in% c("F01","F03") | x %in% c("G30","G31"))  y <- "Dementia"
  else if (x >= "E10" & x <= "E14")  y <- "Diabetes"
  else if (x >= "F32" & x <= "F33")  y <- "Depression"
  else if (x >= "V01" & x <= "V89" | x == "Y85")  y <- "Road Traffic Injuries"
  else if (x >= "I30" & x <= "I33" | x %in% c("I38","I40","I42"))  
           y <- "Inflammatory Heart Disease"
  else if (x >= "C33" & x <= "C34")  y <- "Lung Cancer"
  else if  ((x >= "J10" & x <= "J18") |  (x >= "J20" & x <= "J22") |  
            (x >= "J00" & x <= "J06") |  (x >= "J40" & x < "J44") | 
            (x >= "J45" & x <  "J46") |  (x >= "J30" & x <= "J39") |  
            (x >= "J47" & x <= "J98"))  y <- "Respiratory diseases"
    else y <- "Other"
  return(y)
}

# Label Age for ITHIM age categories
deaths$Cause <- sapply(deaths$icd10chr3,cause_groups)

# Acute respiratory illnesses in children
deaths$Cause <- with(deaths, ifelse(age < 5 & 
         ((icd10chr3 >= "J10" & icd10chr3 <= "J18") | 
         (icd10chr3 >= "J20" & icd10chr3 <= "J22") | 
         (icd10chr3 >= "J00" & icd10chr3 <= "J06")),"Acute resp infections", Cause))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Small area estimate of income quartile based on census tract at death
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# income_stub is California census tracts by income quartile

deaths <- merge(deaths, income_stub, by = "CT", all.x = T)
deaths$d <- 1

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Income Quartile aggregations 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# (1) by MPO regions, Cause, Sex, Age
income_region_sex_age <- aggregate(d ~ income4tile + Region + Cause +
                    Sex + Age, data = deaths, sum, na.rm=TRUE, drop = F)


library(dplyr)
income_junk <- deaths %>% group_by(income4tile, Region, Cause, Sex, Age) %>% summarise(N=n())
head(income_junk)

library(summarytools)
freq(deaths$income4tile)
freq(deaths$Region)



# (2) All California
income_ca_sex_age <- aggregate(d ~ income4tile + Cause + Sex + Age, 
                       data = deaths, sum, na.rm=TRUE, drop = F)
income_ca_sex_age$Region <- "California"

# (3) by MPO regions, All Cause, Sex, Age
income_region_allcause_sex_age <- aggregate(d ~ income4tile + Region + 
                            Sex + Age, data = deaths, sum, na.rm=TRUE, drop = F)
income_region_allcause_sex_age$Cause <- "All Causes"

# (4) by California, All Cause, Sex, Age
income_ca_allcause_sex_age <- aggregate(d ~ income4tile + Sex + Age, 
                              data = deaths, sum, na.rm=TRUE, drop = F)
income_ca_allcause_sex_age$Cause <- "All Causes"
income_ca_allcause_sex_age$Region <- "California"

income_region_sex_age <- rbind(income_region_sex_age, income_ca_sex_age,
                                   income_region_allcause_sex_age,
                                   income_ca_allcause_sex_age)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# For strata with no deaths, convert NAs to 0 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

income_region_sex_age[is.na(income_region_sex_age$d), "d"] <- 0

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Eliminate rows for Acute respiratory infections >= 5 years of age
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
income_region_sex_age <- income_region_sex_age[!(income_region_sex_age$Age 
                        != 0 & income_region_sex_age$Cause == 
                          "Acute resp infections"), ]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Reference Rate numerator Aggregate deaths by age, sex, for California
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# (1) by MPO regions, Cause, Sex, Age
region_sex_age <- aggregate(d ~ Region + Cause +
                     Sex + Age, data = deaths, sum, na.rm=TRUE, drop = F)

# (2) All California
ca_sex_age <- aggregate(d ~ Cause + Sex + Age, 
                               data = deaths, sum, na.rm=TRUE, drop = F)
ca_sex_age$Region <- "California"

# (3) by MPO regions, All Cause, Sex, Age
region_allcause_sex_age <- aggregate(d ~ Region + 
                            Sex + Age, data = deaths, sum, na.rm=TRUE, drop = F)
region_allcause_sex_age$Cause <- "All Causes"

# (4) by California, All Cause, Sex, Age
ca_allcause_sex_age <- aggregate(d ~ Sex + Age, 
                            data = deaths, sum, na.rm=TRUE, drop = F)
ca_allcause_sex_age$Cause <- "All Causes"
ca_allcause_sex_age$Region <- "California"

region_sex_age <- rbind(region_sex_age, ca_sex_age, region_allcause_sex_age,
                               ca_allcause_sex_age)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# For strata with no deaths, convert NAs to 0 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

region_sex_age[is.na(region_sex_age$d), "d"] <- 0

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Eliminate rows for Acute respiratory infections >= 5 years of age
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
region_sex_age <- region_sex_age[!(region_sex_age$Age != 0 & 
                      region_sex_age$Cause == "Acute resp infections"), ]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combine income deaths with population for age-sex-region-income Q death rates
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

income_region_sex_age <- merge(income_region_sex_age, age_sex_region_income,
                           by = c("Region","income4tile","Sex", "Age"),
                           all.x = T)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combine with reference deaths with population for age-sex-region death rates
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

region_sex_age <- merge(region_sex_age, age_sex_region,
                               by = c("Region","Sex", "Age"),
                               all.x = T)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merge files and Create risk adjuster rate ratio (.x = income, .y = reference)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

income_region_sex_age <- merge(income_region_sex_age, region_sex_age,
                         by = c("Region","Cause", "Sex", "Age"), all.x =T )

# If deaths in income group < 10 then risk adjuster = 1
income_region_sex_age$RRadj <- with(income_region_sex_age,
                               ifelse(d.x < 10, 1, 
                                (d.x/pop.x)/(d.y/pop.y)))

# Re-order file
income_region_sex_age <- income_region_sex_age[order(
  income_region_sex_age$income4tile,
  income_region_sex_age$Region,
  income_region_sex_age$Cause,
  income_region_sex_age$Sex,
  income_region_sex_age$Age),]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Write out files for ITHIM
# Lowest income quartile
write.csv(income_region_sex_age[income_region_sex_age$income4tile==1,
              c("Region","Cause", "Sex", "Age","RRadj")],
          paste0("Q1Inc_RiskAdjuster", format(Sys.time(), "%Y-%m-%d"),
                ".csv"), row.names = F)

# Highest Income Quartile
write.csv(income_region_sex_age[income_region_sex_age$income4tile==4,
              c("Region","Cause", "Sex", "Age","RRadj")],
          paste0("Q4Inc_RiskAdjuster",format(Sys.time(), "%Y-%m-%d"),
                 ".csv"), row.names = F)

# Second Lowest Income Quartile
write.csv(income_region_sex_age[income_region_sex_age$income4tile==2,
              c("Region","Cause", "Sex", "Age","RRadj")],
          paste0("Q2Inc_RiskAdjuster",format(Sys.time(), "%Y-%m-%d"),
                 ".csv"), row.names = F)

# Third Income Quartile
write.csv(income_region_sex_age[income_region_sex_age$income4tile==3,
              c("Region","Cause", "Sex", "Age","RRadj")],
          paste0("Q3Inc_RiskAdjuster", format(Sys.time(), "%Y-%m-%d"),
                 ".csv"), row.names = F)
