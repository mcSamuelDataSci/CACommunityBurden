# title: pop.make.r
# purpose: datasets of population denominators for CBD project (state, county, tract, mssa)
# notes:
#		acs package: acs::read.acs can read downloaded files from AFF, csv/zip/etc. in case Census API down.
#					 it is not used herein because of CBD project design preference for simple data objects.
#		county and state level use DOF data; tract and MSSA level use ACS data
#		other necessary datasets for life tables/YLLs:
#			- standard populations
#			- age standards for survival
#		mortality (CDPH/NCHS/SEER)
#!		to prepare for race/eth specific results, perhaps introduce placeholder values of race (eg, =0)
#!		do the county labels need to be added to the population datasets at this time or later?

# 0 ACS years

myACS_year1 <- 2011 # For 2007-2011
myACS_year2 <- 2016 # For 2012-2016
myACS_year3 <- 2019 # Temporarily use this for 2017-2021... 2020 5-year ACS scheduled to release in December 2021

# 1 Setting Paths, and Packages
.packages	  <- c("tidycensus",    #load_variables, get_acs
                 "tidyr",         #spread
                 "dplyr",         #select
                 "readxl",
                 "readr")
.inst       <- .packages %in% installed.packages() 
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst]) 
lapply(.packages, require, character.only=TRUE) 

server <- F
if (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
if (!server) source("G:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")

.ckey   <- read_file(paste0(ccbUpstream,"upstreamInfo/census.api.key.txt")) 

# 2 User Input Variables
#Variable Descriptions: https://www.census.gov/data/developers/data-sets.html
Labels      <- load_variables(2019,"acs5") # view to see topics and labels

# 3 Data Extraction Function
getPop <- function(ACSYear=2018,ACSSurvey="acs5",ACSLabels=LabelsUsed$name) 
{get_acs(state = 06, geography = "tract", survey = ACSSurvey,
         year = ACSYear, variables = ACSLabels,
         key=.ckey, moe_level=90)
}

# 4 Data Extraction
##  ACS data: vector of variable names which contain tract-level population by age/sex
##		table S0101	contains sex by ages: 5 year age groups, expressed as % of tract total population
##		table B01001 contains sex by ages: 0-4,5-9,10-14,15-17,18-19,20,21,22-24,25-29,
##				30-34,35-39,40-44,45-49,50-54,55-59,60-61,62-64,65-66,67-69,70-74,75-79,80-84,85+
##		tables B01001A-B01001I contain table B01001 for races: WNH, W, B, N, A, P, O, MR
##				with limited ages: 0-4,5-9, 10-14, 15-17, 18-19, 20-24, 25-29, 30-34, 35-44, 45-54, 55-64, 75-84, 85+
##		table B01001_002 through B01001_025 refer to males; B01001_026 through B01001_049 refer to females
LabelsUsed  <- filter(Labels,grepl("B01001_",name)) %>%
  subset(!(name %in% c("B01001_001","B01001_002","B01001_026"))) #removes totals


# `a.pop5_2008-2012` <- getPop(2012) 
`a.pop5_2007-2011` <- getPop(myACS_year1) %>% mutate(GEOID = ifelse(GEOID == "06037930401", 
                                                                    "06037137000", GEOID))
`a.pop5_2012-2016` <- getPop(myACS_year2)
`a.pop5_2017-2021` <- getPop(myACS_year3)


# 5 Combining and cleaning
acs.pop.tracts <- mget(ls(pattern='a.pop5+'))  
acs.pop.tracts <- acs.pop.tracts %>% bind_rows(.id="interval") %>%
  merge(Labels,by.x="variable",by.y="name")

acs.pop.tracts <- acs.pop.tracts %>%
  separate(label,sep="!!",c(NA,NA,"sex","age")) %>%
  separate(interval,sep="_",c("interval","yearG5")) %>%
  select(-variable,-concept) %>%
  mutate(sex = gsub(":", "", sex)) # JASPO added this to take out ':'

acs.pop.tracts <- acs.pop.tracts %>%
  arrange(interval, GEOID, yearG5) %>%
  mutate(sex   = replace_na(sex,"Total"),
         age   = replace_na(age,"0 to 999 years"),
         age   = replace(age,age=="Under 5 years","0 to 4 years"),
         age   = replace(age,age=="20 years","20 to 20 years"),
         age   = replace(age,age=="21 years","21 to 21 years"),
         age   = replace(age,age=="85 years and over","85 to 999 years")
  ) %>%
  separate(age,sep=" ",c("agell",NA,"ageul")) %>%
  select(-NAME)


# ===========================================================================================================================

ageMap  <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/Age Group Standard and US Standard 2000 Population.xlsx"),sheet = "data"))
aL      <-      ageMap$lAge   # lower age ranges
aU      <- c(-1,ageMap$uAge)  # upper age ranges, plus inital value of "-1" to make all functions work properly

aMark                    <- findInterval(acs.pop.tracts$agell,aU,left.open = TRUE)  # vector indicating age RANGE value of each INDIVIDUAL age value
aLabs                    <- ageMap$ageLabel
acs.pop.tracts$ageGroup  <- aLabs[aMark] 

cbd.link <- read_csv(paste0(myPlace,"/myInfo/Tract to Community Linkage.csv")) 

linker              <- as.data.frame(cbd.link)[,-1] # removes year
acs.pop.tracts <- merge(acs.pop.tracts,linker,by=c("GEOID"),all=TRUE) %>%
                            select(-c(agell,ageul,comName))


# NOT ALL NEEDED -- CHECK:
popAgeSex            <- acs.pop.tracts %>% group_by(yearG5,county,GEOID,comID,ageGroup,sex) %>% summarise(pop=sum(estimate))
popAge               <- acs.pop.tracts %>% group_by(yearG5,county,GEOID,comID,ageGroup)     %>% summarise(pop=sum(estimate)) %>% mutate(sex = "Total")
popSex               <- acs.pop.tracts %>% group_by(yearG5,county,GEOID,comID,sex)      %>% summarise(pop=sum(estimate)) %>% mutate(               ageGroup = "Total")
pop                  <- acs.pop.tracts %>% group_by(yearG5,county,GEOID,comID)          %>% summarise(pop=sum(estimate)) %>% mutate(sex = "Total", ageGroup = "Total")

popTractSexAgeGTotal  <- bind_rows(pop,popSex,popAge,popAgeSex) %>%
                                    select(yearG5,county,GEOID,comID,sex,ageGroup,pop) %>%
                                    arrange(yearG5,county,GEOID,comID) %>%
                                    ungroup()

saveRDS(popTractSexAgeGTotal, file=paste0(ccbUpstream,"upData/popTract.RDS"))
