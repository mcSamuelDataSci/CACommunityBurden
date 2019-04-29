




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


sdoh_dat_tract <- Reduce(function(x,y) merge(x,y,all=TRUE),mget(ls(pattern='acs.+')))


# file linking MSSAs to census 
# dataframe linking comID and comName
cbdLinkCA      <- read.csv(paste0(myPlace,"/myInfo/Tract to Community Linkage.csv"),colClasses = "character") 

       
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
  
                




