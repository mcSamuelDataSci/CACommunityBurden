##############
# Education API PULL
# Multiple year 1 year estimates with Sex
# ACS and resulting datasets are mutually exclusive unless otherwise noted
# Apr 24 2019
##############

myDrive <- getwd()  
myPlace <- paste0(myDrive,"/myCBD") 
upPlace <- paste0(myDrive,"/myUpstream") 


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
Labels      <- load_variables(2012,"acs1") # view to see topics and labels
Labels      <- filter(Labels,grepl("B15002_",name))
#write.table(Labels, "G:/OHE/HRSU/Fusion Center/Community Burden of Disease/labels.txt", sep="\t")

#3 Data extraction
# ACS Table B15003 EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER (by grade level)
# B15002 "Bachelor's degree - Doctorate degree" / B15003_001 "Total"
# Related measures ------
# B15001_001	SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER (different age level breakdown)
# ------

getEd <- function(ACSYear=2012,ACSSurvey="acs1") 
                   {rbind(
                     get_acs(state = 06, geography = "county", survey = ACSSurvey,
                             year = ACSYear, variables = Labels$name,
                             key=.ckey, moe_level=90),
                     get_acs(state = 06, geography = "state", survey = ACSSurvey,
                             year = ACSYear, variables = Labels$name,
                             key=.ckey, moe_level=90)
                   )}


t1.ed1_2012 <- getEd(2012,"acs1")
t1.ed1_2013 <- getEd(2013,"acs1")
t1.ed1_2014 <- getEd(2014,"acs1")
t1.ed1_2015 <- getEd(2015,"acs1")
t1.ed1_2016 <- getEd(2016,"acs1")
t1.ed1_2017 <- getEd(2017,"acs1")
t1.ed1_2018 <- getEd(2017,"acs1")  ##### USING 2017 AS 2018 unitl 2018 is available !!!!! ####



# t.ed5_2005.09 <- getEd(2009,"acs5")   # generates ERROR
# t.ed5_2006.10 <- getEd(2010,"acs5")
# t.ed5_2007.11 <- getEd(2011,"acs5")
# t.ed5_2008.12 <- getEd(2012,"acs5")
# t.ed5_2009.13 <- getEd(2013,"acs5")
# t.ed5_2010.14 <- getEd(2014,"acs5")
# t.ed5_2011.15 <- getEd(2015,"acs5")
# t.ed5_2012.16 <- getEd(2016,"acs5")
# t.ed5_2013.17 <- getEd(2017,"acs5")



#4 Combining
education <- mget(ls(pattern='t1.ed+'))   # ONLY USING ONE-YEAR FILES FOR NOW
education <- bind_rows(education,.id="interval")  # bind_rows turns list in this form to a dataframe!!
education <- merge(Labels,education,by.x="name",by.y="variable")


#5 Cleaning
education <- separate(education,label,sep="!!",c("TOTAL","ESTIMATE","temp1","temp2","temp3"))
education <- separate(education,interval,sep="_",c("interval","year"))

education <- education %>%
              select(interval, year, sex=temp1,level=temp2, GEOID, NAME, estimate, moe)  %>%
              arrange(interval, GEOID, year)                                             %>%
              mutate(sex   = replace_na(sex,"Total"),
                     level = replace_na(level,"All levels") 
                     )

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
  education$level == "No schooling completed"    ~ 1,
  education$level == "All levels"                ~ 10
  )


education <- education %>% 
  filter(group != 10)   %>% # removes education = "All levels" and removes  sex = Total which (oddly) only occures where education = "All levels"
  group_by(interval,year,sex,GEOID,group) %>%
  summarize(n=sum(estimate),
            moen=moe_sum(moe[which(estimate!=0)],which(estimate!=0,arr.ind=T))) %>%
  ungroup() %>%
  mutate(eduCode=group) %>%
  select(-interval,-group)

educationTot <- education %>% 
  group_by(year,GEOID,eduCode) %>%
  summarize(n=sum(n),
            moen=moe_sum(moen[which(n!=0)],which(n!=0,arr.ind=T))) %>%
  mutate(sex="Total")

education <- bind_rows(education,educationTot) 
            

library(readxl)
geoMap     <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/County Codes to County Names Linkage.xlsx")))  %>%
                   mutate(GEOID=paste0("06",FIPSCounty)) %>%
                   select(county=countyName,GEOID) %>%
                   bind_rows(c("county"="California","GEOID"="06"))

education <- full_join(education,geoMap,by="GEOID")

saveRDS(education,  file = paste0(upPlace,"/upData/popCounty_Education.RDS"))


