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


yearGrp <- "2013-2017"

## 1    SETUP		----------------------------------------------------------------------

## 1.1  packages
.pkg	<- c("data.table","tidycensus","readr") 
.inst   <- .pkg %in% installed.packages() 
if(length(.pkg[!.inst]) > 0) install.packages(.pkg[!.inst]) 
lapply(.pkg, require, character.only=TRUE)           

## 1.2  path and globals
#   .ckey 	<- read_file("census.api.key")						# raw text file containing API key.
#   .path   <- "d:/Users/fieshary/projects/ccb_lifetables"		# !! update path
#   setwd(.path)

#MCS

myDrive <- "E:"  
myPlace <- paste0(myDrive,"/0.CBD/myCBD") 
upPlace <- paste0(myDrive,"/0.CBD/myUpstream")

.ckey <- read_file(paste0(upPlace,"/upstreamInfo/census.api.key.txt")) 

## 2	DATASETS	----------------------------------------------------------------------

## 2.1 	tract-to-MSSA crosswalk
cbd.link <- setDT(
	read_csv(paste0(myPlace,"/myInfo/Tract to Community Linkage.csv")),                    # !! update path
	key="GEOID"	                                  # set key for merging later
)

## 2.2  ACS data: vector of variable names which contain tract-level population by age/sex
##		table S0101	contains sex by ages: 5 year age groups, expressed as % of tract total population
##		table B01001 contains sex by ages: 0-4,5-9,10-14,15-17,18-19,20,21,22-24,25-29,
##				30-34,35-39,40-44,45-49,50-54,55-59,60-61,62-64,65-66,67-69,70-74,75-79,80-84,85+
##		tables B01001A-B01001I contain table B01001 for races: WNH, W, B, N, A, P, O, MR
##				with limited ages: 0-4,5-9, 10-14, 15-17, 18-19, 20-24, 25-29, 30-34, 35-44, 45-54, 55-64, 75-84, 85+
##		table B01001_002 through B01001_025 refer to males; B01001_026 through B01001_049 refer to females
acs.varlist <- sprintf("B01001_%03dE",c(3:25,27:49)) 		# B01001_*E except totals   MCS FIXED FROM ---- c(3:26,28:49))

## 2.3 	ACS variable descriptive labels (e.g., "B01001_003E = Estimates of Male Population Under 5 years ... " )
##		downloaded from API documentation page and pasted into a .csv
acs.labels <- setDT (
	read_csv(paste0(myPlace,"/myInfo/B01001_labels.csv")),             
	key="Name"                                    # define Name as the key for this file
)

## 2.4 	ACS data: tract-level population by age/sex (long format, count and MOE in columns)
acs.pop.tracts <- 	get_acs(state = 06, geography = "tract",  # all tracts in CA (FIPS state code = 06)
						survey = "acs5", year = 2016,         # ACS options
						variables = acs.varlist,              # ACS variable requested (B01003_001 = total population)
						key=.ckey, moe_level=90               # global variable containing Census API key
					)

##	3	ANALYSIS	----------------------------------------------------------------------

##	3.1		ACS datasets (tract and MSSA)

## 	3.1.1 	cleanup ACS labels read from external file
acs.labels[, c("type","strata","sex","age") 
		   := tstrsplit(Label, "!!")] 		                     # parse elements of descriptive label using "!!"
# acs.labels[, sex:=toupper(sex)]                                  # capitalize sex for consistency  #No, better as is
acs.labels <- acs.labels[, c("Name","sex","age")]                # retain only needed elements
acs.labels[, Name := substring(Name,0,10)]                       # remove "E" suffix from label for clean merging
acs.labels[age=="Under 5 years", age := "0 to 4 years"
		   ][age=="85 years and over", age := "85 to 999 years"] # relabel age groups to provide upper, lower ages
acs.labels[, c("agell","ageul") 
		   := tstrsplit(age, " ", keep=c(1,3))]                  # parse age ranges out of age label
acs.labels[is.na(ageul), ageul 
		   := agell]                                             # replace NA values of age with the lower age
acs.labels[, age:=NULL]                                          # drop full text age label

## 	3.1.2 	add age and sex labels for ACS county data
setDT(acs.pop.tracts) 
acs.pop.tracts[, NAME:=NULL]                                     # drop geography label (use GEOID only)
names(acs.pop.tracts)[names(acs.pop.tracts) 
					  == "variable"] = "Name"                    # rename variable to Name (to merge with labels)
setkey(acs.pop.tracts,"Name")                                    # define Name as the key to merge 
acs.pop.tracts<-merge(
					acs.pop.tracts,
					acs.labels, 
					all.x=TRUE                                   # keeping even if no match in acs.labels
				)                                                # merge ACS tract dataset with with age, sex labels
acs.pop.tracts <- acs.pop.tracts[, 
					c("GEOID","sex","agell","ageul","estimate")] # keep needed variables only
acs.pop.tracts[, yearG := yearGrp] 	                                 # add year to dataset


# ===========================================================================================================================
# ----- Michael Work --------------------------------------------------------------------------------------------------------
library(dplyr)

popTractWork <- as.data.frame(acs.pop.tracts)

library(readxl)
ageMap  <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/Age Groups and Standard US 2000 pop.xlsx"),sheet = "data"))
aL      <-      ageMap$lAge   # lower age ranges
aU      <- c(-1,ageMap$uAge)  # upper age ranges, plus inital value of "-1" to make all functions work properly

aMark                     <- findInterval(popTractWork$agell,aU,left.open = TRUE)  # vector indicating age RANGE value of each INDIVIDUAL age value
aLabs                     <- paste(aL,"-",aU[-1])                           # make label for ranges
popTractWork$ageG  <- aLabs[aMark] 

linker              <- as.data.frame(cbd.link)[,-1] # removes year
popTractWork <- merge(popTractWork,linker,by=c("GEOID"),all=TRUE) %>%
                            select(-c(agell,ageul,comName))
# may need to add comName back?

# NOT ALL NEEDED -- CHECK:
popAgeSex            <- popTractWork %>% group_by(yearG,county,GEOID,comID,ageG,sex) %>% summarise(pop=sum(estimate))
popAge               <- popTractWork %>% group_by(yearG,county,GEOID,comID,ageG)     %>% summarise(pop=sum(estimate)) %>% mutate(sex = "Total")
popSex               <- popTractWork %>% group_by(yearG,county,GEOID,comID,sex)      %>% summarise(pop=sum(estimate)) %>% mutate(               ageG = "Total")
pop                  <- popTractWork %>% group_by(yearG,county,GEOID,comID)          %>% summarise(pop=sum(estimate)) %>% mutate(sex = "Total", ageG = "Total")

popTractSexAgeGTotal2013  <- bind_rows(pop,popSex,popAge,popAgeSex) %>%
                                    select(yearG,county,GEOID,comID,sex,ageG,pop) %>%
                                    arrange(yearG,county,GEOID,comID) %>%
                                 ungroup()

saveRDS(popTractSexAgeGTotal2013, file=paste0(upPlace,"/upData/popTract2013.RDS"))
