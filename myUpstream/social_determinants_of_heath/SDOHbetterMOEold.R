#1 Setting Paths, and Packages
myDrive <-  getwd()  

#  .path   <- paste0(myDrive,"/cbd")
# setwd(.path)

.packages	  <- c("tidycensus",    #load_variables, get_acs
                 "tidyr",         #spread
                 "dplyr",         #select
                 "readr",         #read_file
                 "data.table")    #
.inst       <- .packages %in% installed.packages() 
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst]) 
lapply(.packages, require, character.only=TRUE)           

library(tidycensus)   # load_variables, get_acs
library(tidyr)        # spread
library(dplyr)        # select
library(readr)        # read_file
library(data.table)   # used?


# DISCUSS
# .censuskey 	<- read_file("census.api.key.txt")

#2 User Input Variables
#Variable Descriptions: https://www.census.gov/data/developers/data-sets.html

ACSYear     <- 2017
ACSSurvey   <- "acs5" # 5 year (acs5), or 1 year (acs1) data
Labels      <- load_variables(ACSYear,ACSSurvey) # view to see topics and labels


# shorter variable names, and "snake_case" p_Internet p_Internet_moe
# need to make "roll up" to communities and counties easy...



acs.netuse<-get_acs(state = 06, geography = "tract", survey = ACSSurvey,
                    year = ACSYear, variables = c("B28002_001","B28002_002"), key=.ckey, moe_level=90)%>%  
  gather(descriptor,value,estimate,moe) %>%
  unite(temp,variable,descriptor) %>%
  spread(temp,value) %>%
  mutate(percentinternet=B28002_002_estimate/B28002_001_estimate,
         percentinternet_moe=moe_ratio(B28002_002_estimate,B28002_001_estimate,B28002_002_moe,B28002_001_moe))

acs.poverty<-get_acs(state = 06, geography = "tract", survey = ACSSurvey,
                     year = ACSYear, variables = c("B17001_001","B17001_002"), key=.ckey, moe_level=90) %>% 
  gather(descriptor,value,estimate,moe) %>%
  unite(temp,variable,descriptor) %>%
  spread(temp,value) %>%
  mutate(percentpoverty=B17001_002_estimate/B17001_001_estimate,
         percentpoverty_moe=moe_ratio(B17001_002_estimate,B17001_001_estimate,B17001_002_moe,B17001_001_moe))

acs.education<-get_acs(state = 06, geography = "tract", survey = ACSSurvey,
                       year = ACSYear, variables = c("B15003_001","B15003_022"), key=.ckey, moe_level=90) %>% 
  gather(descriptor,value,estimate,moe) %>%
  unite(temp,variable,descriptor) %>%
  spread(temp,value) %>%
  mutate(percentbachelors=B15003_022_estimate/B15003_001_estimate,
         percentbachelors_moe=moe_ratio(B15003_022_estimate,B15003_001_estimate,B15003_022_moe,B15003_001_moe))

acs.housing<-get_acs(state = 06, geography = "tract", survey = ACSSurvey,
                     year = ACSYear, variables = c("B25105_001","B25106_002","B25106_006","B25106_010",
                                                   "B25106_014","B25106_018"), key=.ckey, moe_level=90) %>% 
  gather(descriptor,value,estimate,moe) %>%
  unite(temp,variable,descriptor) %>%
  spread(temp,value) %>%
  rename(medianmonthlyhousingcost=B25105_001_estimate,
         medianmonthlyhousingcost_moe=B25105_001_moe) %>%
  mutate(housingcost30percentincome00to20k=B25106_006_estimate/B25106_002_estimate,
         housingcost30percentincome20to34k=B25106_010_estimate/B25106_002_estimate,
         housingcost30percentincome35to49k=B25106_014_estimate/B25106_002_estimate,
         housingcost30percentincome50to75k=B25106_018_estimate/B25106_002_estimate,
         housingcost30percentincome00to20k_moe=moe_ratio(B25106_006_estimate,B25106_002_estimate,B25106_006_moe,B25106_002_moe),
         housingcost30percentincome20to34k_moe=moe_ratio(B25106_010_estimate,B25106_002_estimate,B25106_010_moe,B25106_002_moe),
         housingcost30percentincome35to49k_moe=moe_ratio(B25106_014_estimate,B25106_002_estimate,B25106_014_moe,B25106_002_moe),
         housingcost30percentincome50to75k_moe=moe_ratio(B25106_018_estimate,B25106_002_estimate,B25106_018_moe,B25106_002_moe))



# GREAT, AND would likely use simpel hard code....
combined<-Reduce(function(x,y) merge(x,y,all=TRUE),mget(ls(pattern='acs.+')))

