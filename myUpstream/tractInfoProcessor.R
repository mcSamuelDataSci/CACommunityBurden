#Read and merge shape, death, pop and other files

library(fs)
library(readr)
library(dplyr)
#library(tigris) #Added this one per Zev's instructions below

myDrive  <- "e:"                            
myPlace  <- paste0(myDrive,"/0.CBD/myUpstream")  




popACS  <- readRDS(path(myPlace,"/upData/popTract2013.RDS")) %>%
                    filter(ageG == "Total",sex=="Total")     %>%
                    mutate(pop2013ACS = pop,
                           inACSpop2013  = 1) %>%
                    select(GEOID,county,inACSpop2013,pop2013ACS)


# R detects character (c) vs. numeric (n) automatically based on first 1000 rows but making sure with col_types below 
# Use pipes (%>%) to neatly select variables and other changes specified

d.correct <- read_csv(path(myPlace,"censusTracts/myData","Deaths_County_Corrected.csv"), col_types = "ncncn") %>%
  mutate(GEOID        = paste0("0",GEOID), #Add "0" to beginning of GEOID as it's shown in some databases
         wrong_county1 = wrong_deaths_co1,
         inDEATHS   = 1)  # this creates the indicator variable for this data set


d.mssa00  <- read_csv(path(myPlace,"censusTracts/myData","mssa00.csv"))  %>%
  mutate(GEOID      = paste0("0",GEOID),
         county     = COUNTY,
         inMSSA00   = 1) %>%  
  select(GEOID,county,inMSSA00, POP2000,POP2000CIV)


# POP2013 and POP2013CIV are already upper case
d.mssa13 <- read_csv(path(myPlace,"censusTracts/myData","mssa13.csv")) %>%
  mutate(GEOID      = paste0("0",GEOID),
         county     = COUNTY,
         inMSSA13   = 1,
         POP2013    = pop2013,
         POP2013CIV = pop2013civ
  ) %>%
  select(GEOID,county,inMSSA13,POP2013,POP2013CIV)


# COUNTY is uppercase?
d.pov <- read_csv(path(myPlace,"censusTracts/myData","pov_2006_10.csv")) %>%
  mutate(GEOID      = paste0("0",GEOID),
         inPOV      = 1) %>%  
  select(GEOID,county,inPOV)

d.group <- read_csv(path(myPlace,"censusTracts/myData","SVI_CDC_group_living.csv")) %>%
  mutate(GEOID      = paste0("0",GEOID),
         county     = COUNTY,
         inGROUP    = 1) %>% 
  select(GEOID,county,inGROUP,tot_pop_grp,e_groupQ)


#Have to use col_types below or R gets confused by one very big area of water way down the list

d.shape <- read_csv(path(myPlace,"censusTracts/myData","tracts_tiger.csv"),col_types = "nnnnnc") %>%
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


merged <- merged %>%
  mutate(inCA         = ifelse(substr(GEOID,1,2) == "06",T,F),
         somePop      = ifelse(POP2013 != 0,T,F),
         percentInst  = (POP2013-POP2013CIV)/POP2013) 

table( merged$inCA , merged$somePop,useNA = "ifany")  

nopop <- filter(merged,inCA & !somePop)

hist(merged$percentInst,breaks=100)


censusWorkMaybe <- merged %>%
  filter( inCA & somePop) %>%
  select(-POP2000,-POP2000CIV)




junk1 <- filter(merged,allH20 == 1)





#Guessing for now we're using the corrected deaths file with totals for all years as before, 
#but CCB shows for each year as in the raw file below, so why did we do that? 
#d.raw      <- read_csv(path(myPlace,"censusTracts/myData","rawDeaths.csv"),col_types = "ncnc") %>%
#               group_by(GEOID,county) %>%
#              summarize(n=sum(Ndeaths))
