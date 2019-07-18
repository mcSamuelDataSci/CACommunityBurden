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
getEd <- function(ACSYear=2012,ACSSurvey="acs1") {get_acs(state = 06, geography = ACSGeo, survey = ACSSurvey,
                         year = ACSYear, variables = Labels$name, 
                         key=.ckey, moe_level=90)}
t.ed1_2012 <- getEd(2012,"acs1")
t.ed1_2013 <- getEd(2013,"acs1")
t.ed1_2014 <- getEd(2014,"acs1")
t.ed1_2015 <- getEd(2015,"acs1")
t.ed1_2016 <- getEd(2016,"acs1")
t.ed1_2017 <- getEd(2017,"acs1")


# 'ed5_2005-2009' <- getEd(2009,"acs5")   error getting this file
t.ed5_2006.10 <- getEd(2010,"acs5")
t.ed5_2007.11 <- getEd(2011,"acs5")
t.ed5_2008.12 <- getEd(2012,"acs5")
t.ed5_2009.13 <- getEd(2013,"acs5")
t.ed5_2010.14 <- getEd(2014,"acs5")
t.ed5_2011.15 <- getEd(2015,"acs5")
t.ed5_2012.16 <- getEd(2016,"acs5")
t.ed5_2013.17 <- getEd(2017,"acs5")

#4 Combining
education <- mget(ls(pattern='t.ed+'))
education <- bind_rows(education,.id="interval")  # where does interval come from? # oh, I see , the file name
education <- merge(Labels,education,by.x="name",by.y="variable")

#5 Cleaning
education <- separate(education,label,sep="!!",c("TOTAL","ESTIMATE","temp1","temp2","temp3"))
education <- separate(education,interval,sep="_",c("interval","year"))

# education <- select(education,interval,year,sex=temp1,level=temp2,GEOID,NAME,estimate,moe)
# education <- education[order(education$interval,education$GEOID,education$year),]
# 
# education$sex <-replace_na(education$sex,"Total")
# education$level <-replace_na(education$level,"All levels")


education <- education %>%
               select(interval, year, sex=temp1,level=temp2, GEOID, NAME, estimate, moe)  %>%
               arrange(interval, GEOID, year)                                             %>%
               mutate(sex   = ifelse(is.na(sex),"Total",sex),
                      level = ifelse(is.na(level),"All levels",level)  )



#6 Aggregating
education$group <- case_when(
  education$level == "Nursery to 4th grade"  ~ 1,
  education$level == "5th and 6th grade"     ~ 1,
  education$level == "7th and 8th grade"     ~ 1,
  education$level == "9th grade"             ~ 2,
  education$level == "10th grade"            ~ 2,
  education$level == "11th grade"            ~ 2,
  education$level == "12th grade, no diploma"~ 2,
  education$level == "High school graduate, GED, or alternative"~ 3,
  education$level == "Some college, less than 1 year"           ~ 4,
  education$level == "Some college, 1 or more years, no degree" ~ 4,
  education$level == "Associate's degree"        ~ 5,
  education$level == "Bachelor's degree"         ~ 6,
  education$level == "Master's degree"           ~ 7,
  education$level == "Professional school degree"~ 8,
  education$level == "Doctorate degree"          ~ 8,
  education$level == "No schooling completed"    ~ 9,
  education$level == "All levels"                ~ 10
  )
education <- education %>% 
  group_by(interval,year,sex,GEOID,group) %>%
  summarize(n=sum(estimate),
            moen=moe_sum(moe[which(estimate!=0)],which(estimate!=0,arr.ind=T)))
