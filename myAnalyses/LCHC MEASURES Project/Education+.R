##############
# Education API PULL
# Multiple year 1 year estimates with Sex
# ACS and resulting datasets are mutually exclusive unless otherwise noted
# Apr 24 2019
##############

#1 Setting Paths, and Packages
.packages	  <- c("tidycensus",    #load_variables, get_acs
                 "tidyr",         #spread
                 "dplyr")         #select
.inst       <- .packages %in% installed.packages() 
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst]) 
lapply(.packages, require, character.only=TRUE)           
.ckey 	<- "e3e74296ba28071ea63579af4b3c744b47012138"


#2 User Input Variables
#Variable Descriptions: https://www.census.gov/data/developers/data-sets.html
ACSYear     <- 2012
ACSSurvey   <- "acs1"  # acs5, or acs1 data
ACSGeo      <- "county"
Labels      <- load_variables(ACSYear,ACSSurvey) # view to see topics and labels
Labels      <- filter(Labels,grepl("B15002_",name))
#write.table(Labels, "G:/OHE/HRSU/Fusion Center/Community Burden of Disease/labels.txt", sep="\t")

#3 Data extraction
# ACS Table B15003 EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER (by grade level)
# B15002 "Bachelor's degree - Doctorate degree" / B15003_001 "Total"
# Related measures ------
# B15001_001	SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER (different age level breakdown)
# ------


getEd <- function(ACSYear = 2012) {
            get_acs(state = 06, geography = ACSGeo, survey = ACSSurvey,
                   year = ACSYear, variables = Labels$name, 
                         key=.ckey, moe_level=90)
}


ed2012 <- getEd()
ed2013 <- getEd(2013)

  
assign(paste0(ACSYear),education)

ACSYear     <- 2013
education <- get_acs(state = 06, geography = ACSGeo, survey = ACSSurvey,
                         year = ACSYear, variables = Labels$name, 
                         key=.ckey, moe_level=90)
assign(paste0(ACSYear),education)

ACSYear     <- 2014
education <- get_acs(state = 06, geography = ACSGeo, survey = ACSSurvey,
                         year = ACSYear, variables = Labels$name, 
                         key=.ckey, moe_level=90)
assign(paste0(ACSYear),education)

ACSYear     <- 2015
education <- get_acs(state = 06, geography = ACSGeo, survey = ACSSurvey,
                         year = ACSYear, variables = Labels$name, 
                         key=.ckey, moe_level=90)
assign(paste0(ACSYear),education)

ACSYear     <- 2016
education <- get_acs(state = 06, geography = ACSGeo, survey = ACSSurvey,
                         year = ACSYear, variables = Labels$name, 
                         key=.ckey, moe_level=90)
assign(paste0(ACSYear),education)

ACSYear     <- 2017
education <- get_acs(state = 06, geography = ACSGeo, survey = ACSSurvey,
                         year = ACSYear, variables = Labels$name, 
                         key=.ckey, moe_level=90)
assign(paste0(ACSYear),education)

education <- mget(ls(pattern='20+'))
education <- bind_rows(education,.id="Year")
education <- merge(Labels,education,by.x="name",by.y="variable")
education <- separate(education,label,sep="!!",c("TOTAL","ESTIMATE","temp1","temp2","temp3"))
education$temp1 <-replace_na(education$temp1,"Total")
education <- select(education,temp1,temp2,Year,GEOID,NAME,estimate,moe)
education <- education[order(education$GEOID,education$Year),]

#education <-
