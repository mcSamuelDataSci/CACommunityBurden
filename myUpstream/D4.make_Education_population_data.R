##############
# Education API PULL
# Multiple year 1 year estimates with Sex
# ACS and resulting datasets are mutually exclusive unless otherwise noted
# Apr 24 2019
##############


myDrive <- getwd()  
myPlace <- paste0(myDrive,"/myCBD") 
upPlace <- paste0(myDrive,"/myUpstream") 



#1a Setting Paths, and Packages
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


#4a Data Extraction
# ACS Table B15001	SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER

LabelsUsed  <- filter(Labels,grepl("B15001_",name))

a.ed1_2012 <- getEd(2012,"acs1")
a.ed1_2013 <- getEd(2013,"acs1")
a.ed1_2014 <- getEd(2014,"acs1")
a.ed1_2015 <- getEd(2015,"acs1")
a.ed1_2016 <- getEd(2016,"acs1")
a.ed1_2017 <- getEd(2017,"acs1")
a.ed1_2018 <- getEd(2018,"acs1")   
a.ed1_2019 <- getEd(2019,"acs1")   ## Using 2018 data for 2019 data for now !!!!!!!!!!!!!!!!!


#a.ed5_2005.09 <- getEd(2009,"acs5")   # generates ERROR
# a.ed5_2006.10 <- getEd(2010,"acs5")
# a.ed5_2007.11 <- getEd(2011,"acs5")
# a.ed5_2008.12 <- getEd(2012,"acs5")
# a.ed5_2009.13 <- getEd(2013,"acs5")
# a.ed5_2010.14 <- getEd(2014,"acs5")
# a.ed5_2011.15 <- getEd(2015,"acs5")
# a.ed5_2012.16 <- getEd(2016,"acs5")
# a.ed5_2013.17 <- getEd(2017,"acs5")


#5a Combining and Cleaning
education.a <- mget(ls(pattern='a.ed1+'))
education.a <- education.a %>% bind_rows(.id="interval") %>%  # bind_rows turns list into data frame!
  merge(Labels,by.x="variable",by.y="name")

education.a <- education.a %>%
  separate(label,sep="!!",c(NA,NA,"sex","age","level")) %>%
  separate(interval,sep="_",c("interval","year")) %>%
  mutate(year    = as.numeric(year)
         ) %>%
  select(-variable,-concept)

education.a <- education.a %>%
  arrange(interval, GEOID, year) %>%
  filter(!is.na(sex),!is.na(age),!is.na(level))

  
  
  # 
  # mutate(sex   = replace_na(sex,"Total"),                # BEN - have you triple-checked that this
  #        age   = replace_na(age,"18 years and over"),    #  NA reassingment is correct?
  #        level = replace_na(level,"All levels")
  #        )

### THE NA for sex only occur for a subset -- net to get rid of them....



#6b Aggregating
EdGroups <- data.frame(label=c( "Less than 9th grade_1",
                      "9th to 12th grade, no diploma_2",
          "High school graduate, GED, or alternative_3",
                            "Some college, no degree_4",
                                 "Associate's degree_5",
                                  "Bachelor's degree_6",
                    "Graduate or professional degree_7",
                                         "All levels_9")) %>%
               separate(label,sep="_",c("level","eduCode"))


AgeGroups <- data.frame(label=c( "18 to 24 years_18 - 24",
                                 "25 to 34 years_25 - 34",
                                 "35 to 44 years_35 - 44",
                                 "45 to 64 years_45 - 65",
                                 "65 years and over_65 - 999")) %>%
                             separate(label,sep="_",c("age","ageG_EDU"))

education.a <- left_join(education.a,EdGroups,by="level") %>% select(-level) %>%
               left_join(AgeGroups,by="age")  %>% select(-age) %>% 
               filter(ageG_EDU != "18 - 24")  %>% # analysis only for age 25 or older
               rename(population = estimate) %>%
               mutate(eduCode = as.numeric(eduCode))
             

library(readxl)
geoMap     <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/County Codes to County Names Linkage.xlsx")))  %>%
  mutate(GEOID=paste0("06",FIPSCounty)) %>%
  select(county=countyName,GEOID) %>%
  bind_rows(c("county"="California","GEOID"="06"))
education.a <- full_join(education.a,geoMap,by="GEOID")  %>% select(-GEOID,-NAME,-interval) %>%
               filter(!is.na(population))   # remove 18 counties without data for now


sexTotal <-  education.a %>%
             group_by(year,county,eduCode,ageG_EDU) %>%
             summarize(population=sum(population),
                       moe=moe_sum(moe[which(population != 0)],which(population != 0,arr.ind=T))) %>%
             mutate(sex="Total")


education.a <- bind_rows(education.a,sexTotal) 


ageTotal <- education.a %>%
            group_by(year,county,eduCode,sex) %>%
            summarize(population=sum(population),
            moe=moe_sum(moe[which(population != 0)],which(population != 0,arr.ind=T))) %>%
            mutate(ageG_EDU = "Total")


education.a <- bind_rows(education.a,ageTotal) 



saveRDS(education.a,  file = paste0(upPlace,"/upData/popCounty_Education.RDS"))





# OTHER CODING etc. as needed ======================================================


# ACS Table B15003 EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER (by grade level)
# LabelsUsed  <- filter(Labels,grepl("B15002_",name))

# EdGroups <- data.frame(label=c( "No schooling completed_0",
#                                 "Nursery to 4th grade_1",
#                                 "5th and 6th grade_1",
#                                 "7th and 8th grade_1",
#                                 "9th grade_2",
#                                 "10th grade_2",
#                                 "11th grade_2",
#                                 "12th grade, no diploma_2",
#                                 "High school graduate, GED, or alternative_3",
#                                 "Some college, less than 1 year_4",
#                                 "Some college, 1 or more years, no degree_4",
#                                 "Associate's degree_5",
#                                 "Bachelor's degree_6",
#                                 "Master's degree_7",
#                                 "Professional school degree_8",
#                                 "Doctorate degree_8",
#                                 "All levels_9"  ) ) %>%
#   separate(label,sep="_",c("level","eduCode"))
# 










