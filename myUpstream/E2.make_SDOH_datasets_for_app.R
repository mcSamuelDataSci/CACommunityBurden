# =====================================================================================
# "sdohProcessor.R" file                                                              |
#            Read and process Social Determiants of Health (SDOH) data                |
#            load packages                                                            |
#                                                                                     |   
# =====================================================================================

##############
# SDOH API PULL
# Education, Netuse, Poverty, Rent
# ACS and resulting datasets are mutually exclusive unless otherwise noted
# Apr 24 2019
##############


# -- Designate locations, load packages, read info files -----------------------------------------------

library(fs)
library(dplyr)
library(readxl)
library(readr)
library(tidyr)        # spread etc.
library(tidycensus)   # load_variables, get_acs


myDrive <- getwd()  
myPlace <- paste0(myDrive,"/myCBD") 
upPlace <- paste0(myDrive,"/myUpstream") 

.ckey 	<- read_file(paste0(upPlace,"/upstreamInfo/census.api.key.txt"))

county.map <- as.data.frame(read_xlsx(paste0(myPlace,"/myInfo/County Codes to County Names Linkage.xlsx")))
cbdLinkCA  <- read.csv(paste0(myPlace,"/myInfo/Tract to Community Linkage.csv"),colClasses = "character")  # file linking MSSAs to census 
comName    <- unique(cbdLinkCA[,c("comID","comName")])                                    # dataframe linking comID and comName


# ACS PART ===========================================================================================
# ====================================================================================================
# ====================================================================================================

#Variable Descriptions: https://www.census.gov/data/developers/data-sets.html
ACSYear     <- 2017
ACSSurvey   <- "acs5" # 5 year (acs5), or 1 year (acs1) data
Labels      <- load_variables(ACSYear,ACSSurvey) # view to see topics and labels
#write.table(Labels, "G:/OHE/HRSU/Fusion Center/Community Burden of Disease/labels.txt", sep="\t")


#3 Data extraction and estimate calculations

# ACS Table B28002 PRESENCE AND TYPES OF INTERNET SUBSCRIPTIONS IN HOUSEHOLD
# B28002_002 "With an Internet subscription" / B28002_001 "Total"
# Related measures ------

acs.netuse<-get_acs(state = 06, geography = "tract", survey = ACSSurvey,
                    year = ACSYear, variables = c("B28002_001","B28002_002"), key=.ckey,  moe_level=90) %>%  
  gather(descriptor,value,estimate,moe) %>%
  unite(temp,variable,descriptor) %>%
  spread(temp,value) %>%
  rename(n_net=B28002_002_estimate,N_net=B28002_001_estimate,
         moen_net=B28002_002_moe,moeN_net=B28002_001_moe) %>%
  mutate(est_net=n_net/N_net,
         moe_net=moe_ratio(n_net,N_net,moen_net,moeN_net)
         ,NAME=NULL)

# ACS Table B17001 POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE (by race)
# B17001_002 "Income in the past 12 months below poverty level" / B17001_001 "Total"
# Related measures ------
acs.poverty<-get_acs(state = 06, geography = "tract", survey = ACSSurvey,
                     year = ACSYear, variables = c("B17001_001","B17001_002"), key=.ckey, moe_level=90) %>% 
  gather(descriptor,value,estimate,moe) %>%
  unite(temp,variable,descriptor) %>%
  spread(temp,value) %>%
  rename(n_pov=B17001_002_estimate,N_pov=B17001_001_estimate,
         moen_pov=B17001_002_moe,moeN_pov=B17001_001_moe) %>%
  mutate(est_pov=n_pov/N_pov,
         moe_pov=moe_ratio(n_pov,N_pov,moen_pov,moeN_pov),
         NAME=NULL)

# ACS Table B15003 EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER (by grade level)
# B15003_022-25 "Bachelor's degree - Doctorate degree" / B15003_001 "Total"
# Related measures ------
acs.education<-get_acs(state = 06, geography = "tract", survey = ACSSurvey,
                       year = ACSYear, variables = c("B15003_001", sprintf("B15003_0%02d",22:25)), 
                       key=.ckey, moe_level=90) %>% 
  mutate(edu= ifelse( variable=="B15003_001","total","edu"),
         NAME=NULL) %>%
  #check this
  
  group_by(GEOID,edu) %>%
  #summarize(n=sum(estimate),moe=moe_sum(moe,estimate,na.rm=F)) %>%
  summarize(n=sum(estimate),moen=moe_sum(moe[which(estimate!=0)],which(estimate!=0,arr.ind=T))) %>%  #,na.rm=F
  gather(descriptor,value,n,moen) %>%
  unite(temp,descriptor,edu) %>%
  spread(temp,value) %>%
  rename(N_edu=n_total,moeN_edu=moen_total) %>%
  mutate(est_edu=n_edu/N_edu,
         moe_edu=moe_ratio(n_edu,N_edu,moen_edu,moeN_edu))

# raw measure is percnet in categories. Aggretated here for interpretablhy and consistany with Alameda measures.

# ACS Table B25070 GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
# Non-mutually exclusive resultant table:
# B25070_007-010 "30.0 to 50.0 percent or more" / B25070_001 "Total" &
# B25070_010     "50.0 percent or more"         / B25070_001 "Total"
# Related measures ------

acs.rent<-get_acs(state = 06, geography = "tract", survey = ACSSurvey,
                  year = ACSYear, variables = c("B25070_001",(sprintf("B25070_0%02d",7:10))),
                  key=.ckey, moe_level=90) %>% 
  mutate(rent= ifelse( variable=="B25070_001",
                       "N_rent","rent30up"),
         NAME=NULL)
acs.rent <- rbind(acs.rent,filter(acs.rent,variable=="B25070_010") %>% mutate(rent="rent50up")) %>%
  group_by(GEOID,rent) %>%
  #summarize(n=sum(estimate),moe=moe_sum(moe,estimate,na.rm=F)) %>%
  summarize(n=sum(estimate),moen=moe_sum(moe[which(estimate!=0)],which(estimate!=0,arr.ind=T))) %>%  #,na.rm=F
  gather(descriptor,value,n,moen) %>%
  unite(temp,descriptor,rent) %>%
  spread(temp,value) %>%
  rename(N_rent=n_N_rent, moeN_rent=moen_N_rent) %>%
  mutate(est_rent30up = n_rent30up/N_rent,
         est_rent50up = n_rent50up/N_rent,
         moe_rent30up = moe_ratio(n_rent30up, N_rent,moen_rent30up,moeN_rent),
         moe_rent50up = moe_ratio(n_rent50up, N_rent,moen_rent50up,moeN_rent)
  ) 


# BEN add short (a couple of phrases) descriptions of how these line works
# Ben here
sdoh_ACS_Tract <- Reduce(function(x,y) merge(x,y,all=TRUE),mget(ls(pattern='acs.+'))) 

sdoh_ACS_County <-  left_join(sdoh_ACS_Tract,cbdLinkCA,by="GEOID") %>%
  select(matches('GEOID|county|n_|N_')) %>%  # Ben here
  gather(table,obs,-county,-GEOID) %>%       # Ben here
  mutate(type= ifelse( grepl('moe',table),"moe","n")) %>%
  spread(type,obs) %>%
  group_by(county,table) %>%
  summarize(n    = sum(n,na.rm=TRUE),
            moe = moe_sum(moe,n,na.rm=TRUE)) %>%
  mutate(n= ifelse(grepl('moe',table),moe,n),moe=NULL) %>% # Ben here
  spread(table,n) %>%
  mutate(est_edu = round(100*( n_edu/N_edu ),2),
         moe_edu = round(100*( moe_ratio(n_edu,N_edu,moen_edu,moeN_edu) ),2),
         est_net = round(100*( n_net/N_net ),2),
         moe_net = round(100*( moe_ratio(n_net,N_net,moen_net,moeN_net) ),2),
         est_pov = round(100*( n_pov/N_pov ),2),
         moe_pov = round(100*( moe_ratio(n_pov,N_pov,moen_pov,moeN_pov) ),2),
         est_rent30up = round(100*( n_rent30up/N_rent ),2),
         moe_rent30up = round(100*( moe_ratio(n_rent30up,N_rent,moen_rent30up,moeN_rent) ),2),
         est_rent50up = round(100*( n_rent50up/N_rent ),2),
         moe_rent50up = round(100*( moe_ratio(n_rent50up,N_rent,moen_rent50up,moeN_rent) ),2) ) %>%
  select(-matches('n_|N_')) # Ben here

sdoh_ACS_Comm <- left_join(sdoh_ACS_Tract,cbdLinkCA,by="GEOID") %>%
  select(matches('GEOID|comID|n_|N_')) %>%
  gather(table,obs,-comID,-GEOID) %>%
  mutate(type= ifelse( grepl('moe',table),"moe","n")) %>%
  spread(type,obs) %>%
  group_by(comID,table) %>%
  summarize(n    = sum(n,na.rm=TRUE),
            moe = moe_sum(moe,n,na.rm=TRUE)) %>%
  mutate(n= ifelse(grepl('moe',table),moe,n),moe=NULL) %>%
  spread(table,n) %>%
  mutate(est_edu = round(100*( n_edu/N_edu ),2),
         moe_edu = round(100*( moe_ratio(n_edu,N_edu,moen_edu,moeN_edu) ),2),
         est_net = round(100*( n_net/N_net ),2),
         moe_net = round(100*( moe_ratio(n_net,N_net,moen_net,moeN_net) ),2),
         est_pov = round(100*( n_pov/N_pov ),2),
         moe_pov = round(100*( moe_ratio(n_pov,N_pov,moen_pov,moeN_pov) ),2),
         est_rent30up = round(100*( n_rent30up/N_rent ),2),
         moe_rent30up = round(100*( moe_ratio(n_rent30up,N_rent,moen_rent30up,moeN_rent) ),2),
         est_rent50up = round(100*( n_rent50up/N_rent ),2),
         moe_rent50up = round(100*( moe_ratio(n_rent50up,N_rent,moen_rent50up,moeN_rent) ),2) ) %>%
  select(-matches('n_|N_'))  



# HPI PART ===========================================================================================
# ====================================================================================================
# ====================================================================================================


# maybe change to read_csv....
HPIdat     <- read.csv(path(upPlace,"/upData/HPI2_MasterFile_2018-04-04.csv"),as.is=TRUE)


# hpiScore      	 Percentile ranking of HPI score                                                            	              0-100 (most-least advantaged)
# insured	         Percentage of adults aged 18 to 64 years currently insured	                                                0-100 (least-most advantaged)
# inpreschool	     Percentage of 3 and 4 year olds enrolled in school                                           	            0-100 (least-most advantaged)
# bachelorsed	     Percentage of population over age 25 with a bachelor's education or higher                               	0-100 (least-most advantaged)
# abovePoverty     Percent of the population with an income exceeding 200% of federal poverty level	                          0-100 (least-most advantaged)
# parkaccess	     Percentage of the population living within a half-mile of a park, beach, or open space greater than 1 acre	0-100 (least-most advantaged)
# houserepair	     Percent of households with kitchen facilities and plumbing	                                                0-100 (least-most advantaged)

sdoh_HCI_Tract     <- mutate(HPIdat,
                         year         = 2015,
                         geoLev       = "Census Tract",
                         GEOID        = paste0("0",CensusTract),
                         pop          = pop2010,
                         comID        = cbdLinkCA[match(GEOID,cbdLinkCA[,"GEOID"]),"comID"],
                         region       = county.map[match(County_Name,county.map[,"countyName"]),"REGION_HEALTHYCA"],
                         regionF      = as.numeric(as.factor(region))
                     ) %>%
               transform(county=County_Name)    %>% 
                   select(year, county, geoLev, GEOID, pop, comID, region, regionF, hpi2score, insured, inpreschool, parkaccess, houserepair)  %>%
                   filter(county !="Alpine")  # one Alpine tract has no usable data

# abovepoverty,
# bachelorsed,

# why are there NA's ?
sdoh_HCI_Comm    <- sdoh_HCI_Tract %>% group_by(region,county,comID) %>%
  summarize(POP          = sum(pop),
            geoLev       = "Communuity",
            hpi2score    = sum(hpi2score*pop,na.rm=TRUE)/POP,
            insured      = sum(insured *pop,na.rm=TRUE)/POP,
            inpreschool  = sum(inpreschool*pop,na.rm=TRUE)/POP,
           # bachelorsed  = sum(bachelorsed*pop,na.rm=TRUE)/POP,
           # abovepoverty = sum(abovepoverty*pop,na.rm=TRUE)/POP,
            parkaccess   = sum(parkaccess*pop,na.rm=TRUE)/POP,
            houserepair  = sum(houserepair*pop,na.rm=TRUE)/POP)  %>% transform(pop=POP)

sdoh_HCI_County    <- sdoh_HCI_Tract %>% group_by(region,county) %>%
  summarize(POP          = sum(pop),
            geoLev       = "County",
            hpi2score    = sum(hpi2score*pop,na.rm=TRUE)/POP,
            insured      = sum(insured *pop,na.rm=TRUE)/POP,
            inpreschool  = sum(inpreschool*pop,na.rm=TRUE)/POP,
          #  bachelorsed  = sum(bachelorsed*pop,na.rm=TRUE)/POP,
          #  abovepoverty = sum(abovepoverty*pop,na.rm=TRUE)/POP,
            parkaccess   = sum(parkaccess*pop,na.rm=TRUE)/POP,
            houserepair  = sum(houserepair*pop,na.rm=TRUE)/POP)  %>% transform(pop=POP)


# MERGE PARTS AND EXPORT =============================================================================
# ====================================================================================================
# ====================================================================================================

sdohTract  <- full_join(sdoh_ACS_Tract,  sdoh_HCI_Tract,  by="GEOID")
sdohComm   <- full_join(sdoh_ACS_Comm,   sdoh_HCI_Comm,   by="comID")
sdohCounty <- full_join(sdoh_ACS_County, sdoh_HCI_County, by="county")


save(sdohTract,  file= path(myPlace,"/myData/","sdohTract.R"))
save(sdohComm,   file= path(myPlace,"/myData/","sdohComm.R"))
save(sdohCounty, file= path(myPlace,"/myData/","sdohCounty.R"))

