##############
# SDOH API PULL
# Education, Netuse, Poverty, Rent
# ACS and resulting datasets are mutually exclusive unless otherwise noted
# Apr 24 2019
##############

#1 Setting Paths, and Packages
myDrive <-  getwd()  
myPlace <- paste0(myDrive,"/myCBD/")  
upPlace <- paste0(myDrive,"/myUpstream/")  
.packages	  <- c("tidycensus",    #load_variables, get_acs
                 "tidyr",         #spread
                 "dplyr",         #select
                 "readr")         #read_file
.inst       <- .packages %in% installed.packages() 
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst]) 
lapply(.packages, require, character.only=TRUE)           
.ckey 	<- read_file(paste0(upPlace,"upstreamInfo/census.api.key.txt"))


#2 User Input Variables
#Variable Descriptions: https://www.census.gov/data/developers/data-sets.html
ACSYear     <- 2017
ACSSurvey   <- "acs5" # 5 year (acs5), or 1 year (acs1) data
Labels      <- load_variables(ACSYear,ACSSurvey) # view to see topics and labels
#write.table(Labels, "G:/OHE/HRSU/Fusion Center/Community Burden of Disease/labels.txt", sep="\t")


#3 Data extraction and estimate calculations

# ACS Table B28002 PRESENCE AND TYPES OF INTERNET SUBSCRIPTIONS IN HOUSEHOLD
# B28002_002 "With an Internet subscription" / B28002_001 "Total"
# Related measures ------
# B28003 PRESENCE OF A COMPUTER AND TYPE OF INTERNET SUBSCRIPTION IN HOUSEHOLD
# B28004 HOUSEHOLD INCOME IN THE LAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS) BY PRESENCE AND TYPE OF INTERNET SUBSCRIPTION IN HOUSEHOLD
# B28005 AGE BY PRESENCE OF A COMPUTER AND TYPES OF INTERNET SUBSCRIPTION IN HOUSEHOLD
# B28006 EDUCATIONAL ATTAINMENT BY PRESENCE OF A COMPUTER AND TYPES OF INTERNET SUBSCRIPTION IN HOUSEHOLD
# B28007 LABOR FORCE STATUS BY PRESENCE OF A COMPUTER AND TYPES OF INTERNET SUBSCRIPTION IN HOUSEHOLD
# B28008 PRESENCE OF A COMPUTER AND TYPE OF INTERNET SUBSCRIPTION IN HOUSEHOLD
# B28009 PRESENCE OF A COMPUTER AND TYPE OF INTERNET SUBSCRIPTION IN HOUSEHOLD (by race, mutually exclusive)
# ------
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
# B17003 POVERTY STATUS IN THE PAST 12 MONTHS OF INDIVIDUALS BY SEX BY EDUCATIONAL ATTAINMENT
# B17004 POVERTY STATUS IN THE PAST 12 MONTHS OF INDIVIDUALS BY SEX BY WORK EXPERIENCE
# B17005 POVERTY STATUS IN THE PAST 12 MONTHS OF INDIVIDUALS BY SEX BY EMPLOYMENT STATUS
# B17006 POVERTY STATUS IN THE PAST 12 MONTHS OF RELATED CHILDREN UNDER 18 YEARS BY FAMILY TYPE BY AGE OF RELATED CHILDREN UNDER 18 YEARS
# B17007 POVERTY STATUS IN THE PAST 12 MONTHS OF UNRELATED INDIVIDUALS 15 YEARS AND OVER BY SEX BY AGE
# B17008 AGGREGATE INCOME DEFICIT (DOLLARS) IN THE PAST 12 MONTHS OF UNRELATED INDIVIDUALS BY SEX
# B17009 POVERTY STATUS BY WORK EXPERIENCE OF UNRELATED INDIVIDUALS BY HOUSEHOLDER STATUS
# B17010 POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS BY AGE OF RELATED CHILDREN
# B17011 AGGREGATE INCOME DEFICIT (DOLLARS) IN THE PAST 12 MONTHS FOR FAMILIES BY FAMILY TYPE
# B17012 POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY HOUSEHOLD TYPE BY NUMBER OF RELATED CHILDREN UNDER 18 YEARS
# B17013 POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY HOUSEHOLD TYPE BY NUMBER OF PERSONS IN FAMILY
# B17014 POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY HOUSEHOLD TYPE BY NUMBER OF WORKERS IN FAMILY
# B17015 POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY TYPE BY SOCIAL SECURITY INCOME BY SUPPLEMENTAL SECURITY INCOME (SSI) AND CASH PUBLIC ASSISTANCE INCOME
# ------
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
# B15001_001	SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER (by age grouping and grade level)
# B15002_001	SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER (by grade level)
# ------
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
# B25001_001	HOUSING UNITS
# B25003_001	TENURE (by race, mutually exclusive)
# B25007_001	TENURE BY AGE OF HOUSEHOLDER
# B25008_001	TOTAL POPULATION IN OCCUPIED HOUSING UNITS BY TENURE
# B25009_001	TENURE BY HOUSEHOLD SIZE
# B25010_001	AVERAGE HOUSEHOLD SIZE OF OCCUPIED HOUSING UNITS BY TENURE
# B25011_001	TENURE BY HOUSEHOLD TYPE (INCLUDING LIVING ALONE) AND AGE OF HOUSEHOLDER
# B25012_001	TENURE BY FAMILIES AND PRESENCE OF OWN CHILDREN
# B25013_001	TENURE BY EDUCATIONAL ATTAINMENT OF HOUSEHOLDER
# B25014_001	TENURE BY OCCUPANTS PER ROOM (by race, mutually exclusive)
# B25015_001	TENURE BY AGE OF HOUSEHOLDER BY OCCUPANTS PER ROOM
# B25016_001	TENURE BY PLUMBING FACILITIES BY OCCUPANTS PER ROOM
# B25020_001	TENURE BY ROOMS
# B25021_001	MEDIAN NUMBER OF ROOMS BY TENURE
# B25022_001	AGGREGATE NUMBER OF ROOMS BY TENURE
# B25026_001	TOTAL POPULATION IN OCCUPIED HOUSING UNITS BY TENURE BY YEAR HOUSEHOLDER MOVED INTO UNIT
# B25031_001	MEDIAN GROSS RENT BY BEDROOMS
# B25032_001	TENURE BY UNITS IN STRUCTURE (by race, mutually exclusive)
# B25033_001	TOTAL POPULATION IN OCCUPIED HOUSING UNITS BY TENURE BY UNITS IN STRUCTURE
# B25034_001	YEAR STRUCTURE BUILT
# B25035_001	MEDIAN YEAR STRUCTURE BUILT
# B25036_001	TENURE BY YEAR STRUCTURE BUILT
# B25037_001	MEDIAN YEAR STRUCTURE BUILT BY TENURE
# B25038_001	TENURE BY YEAR HOUSEHOLDER MOVED INTO UNIT
# B25039_001	MEDIAN YEAR HOUSEHOLDER MOVED INTO UNIT BY TENURE
# B25042_001	TENURE BY BEDROOMS
# B25043_001	TENURE BY TELEPHONE SERVICE AVAILABLE BY AGE OF HOUSEHOLDER
# B25044_001	TENURE BY VEHICLES AVAILABLE
# B25045_001	TENURE BY VEHICLES AVAILABLE BY AGE OF HOUSEHOLDER
# B25046_001	AGGREGATE NUMBER OF VEHICLES AVAILABLE BY TENURE
# B25049_001	TENURE BY PLUMBING FACILITIES
# B25053_001	TENURE BY KITCHEN FACILITIES
# B25054_001	KITCHEN FACILITIES BY MEALS INCLUDED IN RENT
# B25055_001	AGE OF HOUSEHOLDER BY MEALS INCLUDED IN RENT
# B25056_001	CONTRACT RENT
# B25057_001	LOWER CONTRACT RENT QUARTILE (DOLLARS)
# B25058_001	MEDIAN CONTRACT RENT (DOLLARS)
# B25059_001	UPPER CONTRACT RENT QUARTILE (DOLLARS)
# B25060_001	AGGREGATE CONTRACT RENT (DOLLARS)
# B25061_001	RENT ASKED (by amount asked grouping)
# B25062_001	AGGREGATE RENT ASKED (DOLLARS)
# B25063_001	GROSS RENT
# B25064_001	MEDIAN GROSS RENT (DOLLARS)
# B25065_001	AGGREGATE GROSS RENT (DOLLARS)
# B25066_001	AGGREGATE GROSS RENT (DOLLARS) BY UNITS IN STRUCTURE
# B25067_001	AGGREGATE GROSS RENT (DOLLARS) BY MEALS INCLUDED IN RENT
# B25068_001	BEDROOMS BY GROSS RENT
# B25069_001	INCLUSION OF UTILITIES IN RENT
# B25070_001	GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
# B25071_001	MEDIAN GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS (DOLLARS)
# B25072_001	AGE OF HOUSEHOLDER BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
# B25074_001	HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS

# ------
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




# Code to explore MOE calculation in different situations
if (1==2){

junk <-get_acs(state = 06, geography = "tract", survey = ACSSurvey,
                year = ACSYear, variables = sprintf("B25106_0%02d",3:6),
                key=.ckey, moe_level=90) %>% 
  gather(descriptor,value,estimate,moe) %>%
  unite(temp,variable,descriptor) %>%
  spread(temp,value)  %>%  
  mutate(junkSum  =          (B25106_004_estimate + B25106_005_estimate + B25106_006_estimate + B25106_003_estimate),
         dumbSum  =         sum(B25106_004_estimate,B25106_005_estimate, B25106_006_estimate, B25106_003_estimate),
         smartSum =         select(., c(B25106_004_estimate,B25106_005_estimate, B25106_006_estimate, B25106_003_estimate)) %>% apply(1, sum, na.rm=TRUE), 
         sillyMOE  =        select(., c(B25106_004_moe,B25106_005_moe, B25106_006_moe, B25106_003_moe)) %>% apply(1,sum,na.rm=TRUE),
         maybeMOE1 =        select(., c(B25106_004_moe,B25106_005_moe, B25106_006_moe, B25106_003_moe)) %>% apply(1,moe_sum,estimate = NULL,na.rm=TRUE) ,
         maybeMOE2 =        sqrt( B25106_004_moe^2 + B25106_005_moe^2 + B25106_006_moe^2 + B25106_003_moe^2) 
  )
}



if (1==2){
acs.own<-get_acs(state = 06, geography = "tract", survey = ACSSurvey,
                  year = ACSYear, variables = sprintf("B25106_0%02d",3:6),
                  key=.ckey, moe_level=90) %>% 
  gather(descriptor,value,estimate,moe) %>%
  unite(temp,variable,descriptor) %>%
  spread(temp,value) %>%
  rename(n_own00to19 = B25106_004_estimate,
         n_own20to29 = B25106_005_estimate,
         n_own30up   = B25106_006_estimate,
         N_own       = B25106_003_estimate) %>%
    mutate(est_own00to19 = n_own00to19/N_own,
           est_own20to29 = n_own20to29/N_own,
           est_own30up   = n_own30up/N_own,
           moe_own00to19 = moe_ratio(n_own00to19,N_own,B25106_004_moe,B25106_003_moe),
           moe_own20to29 = moe_ratio(n_own20to29,N_own,B25106_005_moe,B25106_003_moe),
           moe_own30up   = moe_ratio(n_own30up  ,N_own,B25106_006_moe,B25106_003_moe),
           B25106_003_moe=NULL,B25106_004_moe=NULL,B25106_005_moe=NULL,B25106_006_moe=NULL) %>%
  select(-NAME)


acs.renth<-get_acs(state = 06, geography = "tract", survey = ACSSurvey,
                   year = ACSYear, variables = c("B25074_001",sprintf("B25074_0%02d",3:9),
                                                 sprintf("B25074_0%02d",12:18)),
                   key=.ckey, moe_level=90) %>%
  mutate(renth= ifelse( variable=="B25074_001",
                        "N_renth",
                        ifelse( variable=="B25074_003" | variable=="B25074_012",
                        "renth00to19",
                        ifelse( variable=="B25074_004" | variable=="B25074_005" |
                                variable=="B25074_013" | variable=="B25074_014"  ,
                                "renth20to29",
                                "renth30up"))))%>%
  group_by(GEOID,NAME,renth) %>%
  #summarize(n=sum(estimate),moe=moe_sum(moe,estimate,na.rm=F)) #%>%
  #,na.rm=F
  summarize(n=sum(estimate),moe=moe_sum(moe[which(estimate!=0)],which(estimate!=0,arr.ind=T))) %>%
  gather(descriptor,value,n,moe) %>%
  unite(temp,descriptor,renth) %>%
  spread(temp,value) %>%
  rename(N_renth=n_N_renth, moe_renth=moe_N_renth) %>%
  mutate(est_renth00to19 = n_renth00to19/N_renth,
         est_renth20to29 = n_renth20to29/N_renth,
         est_renth30up   = n_renth30up/N_renth,
         moe_renth00to19 = moe_ratio(n_renth00to19 , N_renth,moe_renth00to19,moe_renth),
         moe_renth20to29 = moe_ratio(n_renth20to29,N_renth,moe_renth20to29,moe_renth),
         moe_renth30up   = moe_ratio(n_renth30up  ,N_renth,moe_renth30up  ,moe_renth))
#
#
acs.mortg<-get_acs(state = 06, geography = "tract", survey = ACSSurvey,
                  year = ACSYear, variables = sprintf("B25101_0%02d",3:6),
                  key=.ckey, moe_level=90) %>%
  gather(descriptor,value,estimate,moe) %>%
  unite(temp,variable,descriptor) %>%
  spread(temp,value) %>%
  rename(n_mortg00to19 = B25101_003_estimate,
         n_mortg20to29 = B25101_004_estimate,
         n_mortg30up   = B25101_005_estimate,
         N_mortg       = B25101_006_estimate) %>%
  mutate(est_mortg00to19 = n_mortg00to19/N_mortg,
         est_mortg20to29 = n_mortg20to29/N_mortg,
         est_mortg30up   = n_mortg30up/N_mortg,
         moe_mortg00to19 = moe_ratio(n_mortg00to19,N_mortg,B25101_004_moe,B25101_003_moe),
         moe_mortg20to29 = moe_ratio(n_mortg20to29,N_mortg,B25101_005_moe,B25101_003_moe),
         moe_mortg30up   = moe_ratio(n_mortg30up  ,N_mortg,B25101_006_moe,B25101_003_moe)) %>%
   select(-B25101_003_moe, -B25101_004_moe,-B25101_005_moe,-B25101_006_moe,-NAME)
} # end if 1==2



sdoh_dat_tract <- Reduce(function(x,y) merge(x,y,all=TRUE),mget(ls(pattern='acs.+')))

cbdLinkCA  <- read.csv(paste0(myPlace,"/myInfo/Tract to Community Linkage.csv"),colClasses = "character")  # file linking MSSAs to census 
                                  # dataframe linking comID and comName

#bens           
sdoh_dat_countyB <-  left_join(sdoh_dat_tract,cbdLinkCA,by="GEOID") %>%
                    select(matches('GEOID|county|n_|N_')) %>%
                    gather(table,obs,-county,-GEOID) %>%
                    mutate(type= ifelse( grepl('moe',table),"moe","n")) %>%
                    spread(type,obs) %>%
                    group_by(county,table) %>%
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


sdoh_dat_community <- left_join(sdoh_dat_tract,cbdLinkCA,by="GEOID") %>%
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
  
                




