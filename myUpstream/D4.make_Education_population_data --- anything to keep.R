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
#write.table(Labels, "G:/OHE/HRSU/Fusion Center/Community Burden of Disease/labels.txt", sep="\t")


#3 Data Extraction Function
getEd <- function(ACSYear=2012,ACSSurvey="acs1",ACSLabels=LabelsUsed$name) 
{rbind(
  get_acs(state = 06, geography = "county", survey = ACSSurvey,
          year = ACSYear, variables = ACSLabels,
          key=.ckey, moe_level=90),
  get_acs(state = 06, geography = "state", survey = ACSSurvey,
          year = ACSYear, variables = ACSLabels,
          key=.ckey, moe_level=90)
)}


# 4 Data Extraction
# ACS Table B15003 EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER (by grade level)
LabelsUsed  <- filter(Labels,grepl("B15002_",name))
t.ed1_2012 <- getEd(2012,"acs1")
t.ed1_2013 <- getEd(2013,"acs1")
t.ed1_2014 <- getEd(2014,"acs1")
t.ed1_2015 <- getEd(2015,"acs1")
t.ed1_2016 <- getEd(2016,"acs1")
t.ed1_2017 <- getEd(2017,"acs1")

# t.ed5_2005.09 <- getEd(2009,"acs5")   # generates ERROR
t.ed5_2006.10 <- getEd(2010,"acs5")
t.ed5_2007.11 <- getEd(2011,"acs5")
t.ed5_2008.12 <- getEd(2012,"acs5")
t.ed5_2009.13 <- getEd(2013,"acs5")
t.ed5_2010.14 <- getEd(2014,"acs5")
t.ed5_2011.15 <- getEd(2015,"acs5")
t.ed5_2012.16 <- getEd(2016,"acs5")
t.ed5_2013.17 <- getEd(2017,"acs5")


#5 Combining and Cleaning
education.t <- mget(ls(pattern='t.ed1+'))   # ONLY USING ONE-YEAR FILES FOR NOW
education.t <- education.t %>% bind_rows(.id="interval") %>%  # bind_rows turns list in this form to a dataframe!!
  merge(Labels,by.x="variable",by.y="name")

education.t <- education.t %>%
  separate(label,    sep="!!", c(NA,NA,"sex","level",NA)) %>%
  separate(interval, sep="_",  c("interval","year")) %>%
  select(-variable,-concept)
education.t <- education.t %>%
  arrange(interval, GEOID, year) %>%
  mutate(sex   = replace_na(sex,"Total"),
         level = replace_na(level,"All levels") 
  )


#6 Aggregating
EdGroups <- data.frame(label=c( "No schooling completed_1",
                                  "Nursery to 4th grade_1",
                                     "5th and 6th grade_1",
                                     "7th and 8th grade_1",
                                             "9th grade_2",
                                            "10th grade_2",
                                            "11th grade_2",
                                "12th grade, no diploma_2",
             "High school graduate, GED, or alternative_3",
                        "Some college, less than 1 year_4",
              "Some college, 1 or more years, no degree_4",
                                    "Associate's degree_5",
                                     "Bachelor's degree_6",
                                       "Master's degree_7",
                            "Professional school degree_8",
                                      "Doctorate degree_8",
                                            "All levels_9"  ) ) %>%
separate(label,sep="_",c("level","eduCode"))

education.t <- merge(education.t,EdGroups) %>%
  select(-level) %>% 
  filter(eduCode != 9) %>% # removes education = "All levels" and removes  sex = Total which (oddly) only occures where education = "All levels"
  group_by(interval,year,sex,GEOID,eduCode) %>%
  summarize(n=sum(estimate),
            moen=moe_sum(moe[which(estimate!=0)],which(estimate!=0,arr.ind=T))) %>%
  ungroup() %>%
  select(-interval)
educationTot <- education.t %>% # Adding Total for sex at each education level
  group_by(year,GEOID,eduCode) %>%
  summarize(n=sum(n),
            moen=moe_sum(moen[which(n!=0)],which(n!=0,arr.ind=T))) %>%
  mutate(sex="Total")
  
education.t <- bind_rows(education.t,educationTot)

library(readxl)
geoMap     <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/County Codes to County Names Linkage.xlsx")))  %>%
                   mutate(GEOID=paste0("06",FIPSCounty)) %>%
                   select(county=countyName,GEOID) %>%
                   bind_rows(c("county"="California","GEOID"="06"))

education.t <- full_join(education.t,geoMap,by="GEOID")

saveRDS(education.t,  file = paste0(upPlace,"/upData/popCounty_Education.RDS"))