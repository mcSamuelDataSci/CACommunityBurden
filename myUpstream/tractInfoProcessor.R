#Read and merge shape, death, pop and other files

library(fs)
library(readr)
library(dplyr)

myDrive  <- "e:"                            
myPlace  <- paste0(myDrive,"/0.CBD/myUpstream")  

popACS  <- readRDS(path(myPlace,"/upData/popTract2013.RDS")) %>%
                    filter(ageG == "Total",sex=="Total")     %>%
                    mutate(pop2013ACS = pop,
                           inACSpop2013  = 1) %>%
                    select(GEOID,county,inACSpop2013,pop2013ACS)


# R detects character (c) vs. numeric (n) automatically based on first 1000 rows but making sure with col_types below 
# Use pipes (%>%) to neatly select variables and other changes specified

d.correct <- read_csv(path(myPlace,"upData/tractInfoData","Deaths_County_Corrected.csv"), col_types = "ncncn") %>%
  mutate(GEOID        = paste0("0",GEOID), #Add "0" to beginning of GEOID as it's shown in some databases
         wrong_county1 = wrong_deaths_co1,
         inDEATHS   = 1)  # this creates the indicator variable for this data set

d.mssa00  <- read_csv(path(myPlace,"upData/tractInfoData","mssa00.csv"))  %>%
  mutate(GEOID      = paste0("0",GEOID),
         county     = COUNTY,
         inMSSA00   = 1) %>%  
  select(GEOID,county,inMSSA00, POP2000,POP2000CIV)


# POP2013 and POP2013CIV are already upper case
d.mssa13 <- read_csv(path(myPlace,"upData/tractInfoData","mssa13.csv")) %>%
  mutate(GEOID      = paste0("0",GEOID),
         county     = COUNTY,
         inMSSA13   = 1,
         POP2013    = pop2013,
         POP2013CIV = pop2013civ
  ) %>%
  select(GEOID,county,inMSSA13,POP2013,POP2013CIV)

# COUNTY is uppercase?
d.pov <- read_csv(path(myPlace,"upData/tractInfoData","pov_2006_10.csv")) %>%
  mutate(GEOID      = paste0("0",GEOID),
         inPOV      = 1) %>%  
  select(GEOID,county,inPOV)

d.group <- read_csv(path(myPlace,"upData/tractInfoData","SVI_CDC_group_living.csv")) %>%
  mutate(GEOID      = paste0("0",GEOID),
         county     = COUNTY,
         inGROUP    = 1) %>% 
  select(GEOID,county,inGROUP,tot_pop_grp,e_groupQ)


#Have to use col_types below or R gets confused by one very big area of water way down the list

d.shape <- read_csv(path(myPlace,"upData/tractInfoData","tracts_tiger.csv"),col_types = "nnnnnc") %>%
  mutate(GEOID      = paste0("0",GEOID),
         inSHAPE    = 1,
         percentH20 = AWATER/(AWATER+ALAND),
         allH20  = ifelse(percentH20 ==1 ,1,0)) %>% 
  select(GEOID,county,inSHAPE,percentH20,allH20,ALAND,AWATER)


# Match on GEOID and county
mergeVec <- c("GEOID" , "county")
merged <- full_join(d.correct, d.mssa00, by = mergeVec)  %>%
  full_join(d.mssa13, by = mergeVec)             %>%
  full_join(d.pov,    by = mergeVec)             %>%
  full_join(d.group,  by = mergeVec)             %>%
  full_join(d.shape,  by = mergeVec)             %>%
  full_join(popACS,   by = mergeVec)


merged <- merged %>%  mutate(inCA          = ifelse(substr(GEOID,1,2) == "06",T,F),
                             somePop       = ifelse(POP2013 != 0,T,F),
                             percentInst   = (POP2013-POP2013CIV)/POP2013, 
                             noPopNoDeaths = ifelse(pop2013ACS==0 & is.na(deaths1),T,F)
                             )


nrow(merged)  # ALL merged 9781
table(merged$inCA,useNA = "ifany")   # 590 not in CA 9191 in CA
work <- filter(merged,inCA==TRUE)    # ONLY California
table(work$inSHAPE,useNA = "ifany")  # 1150 not in shape file
junk <- filter(work, is.na(inSHAPE)) # not in shape file  
table(junk$deaths1,useNA = "ifany")  # among these, no deaths 
table(junk$inMSSA00,useNA = "ifany") # 1112 from MSSA00, 38 from somewhere else (NONE in MSSA13)
junk <- filter(junk,is.na(inMSSA00)) 
junk$GEOID[!(is.na(junk$inPOV))]          # 17 inPov and nowhere else --- DAVE, check these
junk$GEOID[!(is.na(junk$inACSpop2013))]   # 21 inACSpop2013 and nowhere else (and 0 population for all) --- DAVE, check these

work <- filter(work,!is.na(inSHAPE))  #8041
write_csv(work,path(myPlace,"/upData/tractInformation.csv"))


table(work$allH20,useNA = "ifany")
work <- filter(work,allH20==0)        #8036

summary(work$deaths1)      # some 0's, 25 NAs
summary(work$pop2013ACS)   # some 0's, no NAs

junk <- filter(work,deaths1==0 )   #--> just one record, see "wrong deaths county"


junk <- filter(work,pop2013ACS==0 & is.na(deaths1)) 
# one tract, 06095980000 has 0 pop2012ACS but 236 pop in pop1013 -- DAVE CHECK
# 11 tracts with no population and no deaths
# "06037980005" "06037980022" "06037980006" "06037980030" "06037980020" "06037980001" "06037980002" "06065980004" "06071980100" "06075980401" "06081984300" "06095980000"

# bad <- c("06081990100","06001990000","06037137000", #  WHY?
#         "06075980401") # 0 population and 0 deaths   


work <- filter(work, !(pop2013ACS==0 & is.na(deaths1))) #8024 (NO REASON TO FILTER???)  12 tracts with no Pop and no deaths

junk <- filter(work,pop2013ACS==0) # 13 tracts with 0 pop and some deaths
junk$GEOID
# "06037320000" "06037504102" "06037980003" "06037980004" "06037980007" "06037980013" "06037980014" "06037980018" "06037980021" "06037980025" "06037980028" "06037980033" "06073009902"


#Guessing for now we're using the corrected deaths file with totals for all years as before, 
#but CCB shows for each year as in the raw file below, so why did we do that? 
#d.raw      <- read_csv(path(myPlace,"upData/tractInfoData","rawDeaths.csv"),col_types = "ncnc") %>%
#               group_by(GEOID,county) %>%
#              summarize(n=sum(Ndeaths))
