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

.ckey <- read_file(paste0(upPlace,"/census.api.key.txt")) 

## 2	DATASETS	----------------------------------------------------------------------

## 2.1 	tract-to-MSSA crosswalk
cbd.link <- setDT(
	read_csv(paste0(myPlace,"/myInfo/cbdLinkCA.csv")),                    # !! update path
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
	read_csv(paste0(myPlace,"/myInfo/B01001_labels.csv")),                # !! update path
	key="Name"                                    # define Name as the key for this file
)

## 2.4 	ACS data: tract-level population by age/sex (long format, count and MOE in columns)
acs.pop.tracts <- 	get_acs(state = 06, geography = "tract",  # all tracts in CA (FIPS state code = 06)
						survey = "acs5", year = 2013,         # ACS options
						variables = acs.varlist,              # ACS variable requested (B01003_001 = total population)
						key=.ckey, moe_level=90               # global variable containing Census API key
					)


## 2.5 	ACS data: county-level population by age/sex (long format, count and MOE in columns)
# acs.pop.county <- get_acs(state = 06, geography = "county",
# 			survey = "acs5", year = 2015,
# 			variables = acs.varlist,
# 			key=.ckey, moe_level=90
# )

## 2.6	NCHS data: county-level population by sex/age
##		several options here. (1): SEER file with 19 age groups
##							  (2): SEER file with 86 single years of age (topcode 85+)
##		sex: 1=male 2=female
seer.age86<-"https://seer.cancer.gov/popdata/yr1990_2016.singleages/ca.1990_2016.singleages.txt.gz"
seer.age19<-"https://seer.cancer.gov/popdata/yr1990_2016.singleages/ca.1990_2016.singleages.txt.gz"
seer.pop.county <- 	read_fwf(seer.age19,
						fwf_cols(year=c(1,4), fips=c(7,11), race4=14, hisp=15, sex=16, age=c(17,18), pop=c(19,26)),
						col_types="ii__iii"
					)

## 2.7	DOF data: county-level population by county/sex/age
##		several	options here. (1): P-3 file (compressed CSV file with complete age/race/sex/ethnic detail)
##							  (2): P-2 file (5-year age groups by sex, xlsx file)
##							  (3): data.ca.gov portal via API (sex, age); SKIP county text label and pop_total
##		sex: string
dof.pop.county <- 	read_csv("https://data.ca.gov/sites/default/files/dof_dru_pop_1970_2050_csya_wide.csv",
			 			col_types="i_iiii_"
			 		)

##	3	ANALYSIS	----------------------------------------------------------------------

##	3.1		ACS datasets (tract and MSSA)

## 	3.1.1 	cleanup ACS labels read from external file
acs.labels[, c("type","strata","sex","age") 
		   := tstrsplit(Label, "!!")] 		                     # parse elements of descriptive label using "!!"
acs.labels[, sex:=toupper(sex)]                                  # capitalize sex for consistency
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
acs.pop.tracts[, yearG := "2011-2015"] 	                                 # add year to dataset


# ===========================================================================================================================
# ----- Michael Work --------------------------------------------------------------------------------------------------------
library(dplyr)

popTractAgeG2013 <- as.data.frame(acs.pop.tracts)

# unique(popTractAgeG2013$agell)
# "0"  "5"  "10" "15" "18" "20" "21" "22" "25" "30" "35" "40" "45" "50" "55" "60" "62" "65" "67" "70" "75" "80" "85"
 
aL                 <- c(   0, 5,15,25,35,45,55,65,75,85)
aU                 <- c(-1,4,14,24,34,44,54,64,74,84,999)
aMark              <- findInterval(popTractAgeG2013$agell,aU,left.open = TRUE)  # vector indicating age RANGE value of each INDIVIDUAL age value
aLabs              <- paste(aL,"-",aU[-1])                           # make label for ranges
popTractAgeG2013$ageG  <- aLabs[aMark] 

linker       <- as.data.frame(cbd.link)[,-1] # removes year
popTractAgeG2013 <- merge(popTractAgeG2013,linker,by=c("GEOID"),all=TRUE)
popTractAgeG2013 <- popTractAgeG2013 %>% group_by(yearG,GEOID,comID,ageG,sex) %>% summarise(pop=sum(estimate))

saveRDS(popTractAgeG2013, file=paste0(upPlace,"/upData/popTractAgeG2013.RDS"))

# --- END Michael
# ===========================================================================================================================

# 	3.1.3	collapse tract data into MSSAs
#!! 		this method does not export tracts which do not match, which previously were exported to subTracts.csv
#!! 		choose whether to merge and collapse to MSSA level or merely merge MSSA labels
setkey(acs.pop.tracts,"GEOID") # set GEOID as key for this file for merging
#acs.pop.mssa <- acs.pop.tracts[cbd.link,nomatch=0                # merge tracts data with cbd.link	
#							   ][,.(estimate=sum(estimate)),     
#							     by=.(comID,comName,county,year)] # calculate the sum of estimates by MSSA id variables
acs.pop.mssa <- acs.pop.tracts[cbd.link,nomatch=0                 # merge tracts data with cbd.link	
							   ][,.(estimate=sum(estimate)),     
							     by=.(comID,year,sex,agell,ageul)] # calculate the sum of estimates by MSSA id variables

## 	3.2 	SEER datasets
##	3.2.1	SEER county

setDT(seer.pop.county) # added by MCS

seer.pop.county <- seer.pop.county[inrange(year,2000,2016)]      # subset data to years 2000-2016
seer.pop.county[, GEOID
				:=sprintf("%05d000000",seer.pop.county$fips)]    # convert 4-digit FIPS into 11-character GEOID
seer.pop.county[, fips:=NULL]                                    # drop fips variable, now that using GEOID
seer.pop.county[, sex := as.character(sex)
				][sex == "1", sex := "MALE"
				  ][sex == "2", sex := "FEMALE"]                 # convert sex to string for consistency
seer.pop.county <- seer.pop.county[,.(estimate=sum(pop)),
								   by=.(year,sex,age,GEOID)]     # collapse race/ethnic detail
##	3.2.2	SEER state
seer.pop.state <- seer.pop.county[,.(estimate=sum(estimate)),
								  by=.(year,sex,age)]	         # collapse county detail
seer.pop.state[, GEOID:="06000000000"]                           # add new GEOID for state level dataset

##	3.3 	DOF datasets
##	3.3.1	DOF county

setDT(dof.pop.county)  # added by MCS

dof.pop.county <- dof.pop.county[inrange(year,2000,2016)]        # subset data to years 2000-2016
dof.pop.county[, GEOID
			   :=sprintf("%05d000000",dof.pop.county$fips)]      # convert 4-digit FIPS into 11-character GEOID
dof.pop.county[,fips:=NULL]                                      # drop fips variable, now that GEOID has been added
dof.pop.county 		  <- melt.data.table(dof.pop.county, 
							variable.name="sex",
					   		id.vars=c("year","age","GEOID"), 
					   		value="estimate"
						 )                                       # convert dataset from wide to long format
dof.pop.county[sex == "pop_male", sex := "MALE"
			   ][sex == "pop_female", sex := "FEMALE"]           # convert sex to consistent string
##	3.3.2	DOF state
dof.pop.state <- dof.pop.county[,.(estimate=sum(estimate)),
								by=.(year,sex,age)]              # collapse county detail
dof.pop.state[,GEOID:="06000000000"]                             # new GEOID for state level dataset




##	4	CLEANUP		----------------------------------------------------------------------

##	4.1	export results: county, tract, MSSA level datasets
#!! 	consider whether to use rda (regular rdata file with many objects) or rds (single object datafile, assignable)
#!!		a pro of the RDS format is that it is a single object, reassignable when read
#!! 	either way, recommend updating other file suffixes using rda or rds to distinguish from r code
#		SAVE AS RDA
#		save(acs.pop.mssa, file=paste0(upPlace,"/upData/popCensusCom.rda"))  save R dataset using load()
#		SAVE AS RDS
# 		saveRDS(acs.pop.county, file=paste0(upPlace,"/upData/popCensusCounty.rds")) # save R data object using readRDS()

saveRDS(acs.pop.tracts, file="popTract.rds")  		# GEOID year sex (char) age23 estimate        ### TYPO HERE was "tract" not "tracts"
saveRDS(acs.pop.mssa, file="popMSSA.rds")  			# comID year sex (char) age23 estimate
saveRDS(dof.pop.county, file="popCounty.rds") 		# GEOID year sex (char) age111 estimate
saveRDS(dof.pop.state, file="popState.rds")  		# GEOID year sex (char) age111 estimate

# END
