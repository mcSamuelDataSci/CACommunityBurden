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



## 1.2  packages
.pkg	<- c("data.table","tidycensus","readr","stringr","fs") 
.inst   <- .pkg %in% installed.packages() 
if(length(.pkg[!.inst]) > 0) install.packages(.pkg[!.inst]) 
lapply(.pkg, require, character.only=TRUE)           

## 1.1  path and globals
.path      <- "e:/"
setwd(.path)
myPlace    <- path(.path,"0.CBD/myCBD")
upPlace    <- path(.path,"0.CBD/myUpstream")
.ckey 	   <- read_file(path(upPlace,"census.api.key.txt"))						# raw text file containing API key.
.ckey 	   <- read_file(path(upPlace,"censusKey.txt"))						# raw text file containing API key.


## 2	DATASETS	----------------------------------------------------------------------

## 2.1 	tract-to-MSSA crosswalk
cbd.link <- setDT(
	read_csv(path(myPlace,"myInfo","cbdLinkCA.csv")),                    # !! update path
	key="GEOID"	                                  # set key for merging later
)

## 2.2  ACS data: vector of variable names which contain tract-level population by age/sex
##		table S0101	contains sex by ages: 5 year age groups, expressed as % of tract total population
##		table B01001 contains sex by ages: 0-4,5-9,10-14,15-17,18-19,20,21,22-24,25-29,
##				30-34,35-39,40-44,45-49,50-54,55-59,60-61,62-64,65-66,67-69,70-74,75-79,80-84,85+
##		tables B01001A-B01001I contain table B01001 for races: WNH, W, B, N, A, P, O, MR
##				with limited ages: 0-4,5-9, 10-14, 15-17, 18-19, 20-24, 25-29, 30-34, 35-44, 45-54, 55-64, 75-84, 85+
##		table B01001_002 through B01001_025 refer to males; B01001_026 through B01001_049 refer to females
##		get full list from https://api.census.gov/data/2016/acs/acs5/variables.html
acs.varlist <- sprintf("B01001_%03dE",c(3:25,27:49)) 		# B01001_*E except totals

## 2.3 	ACS variable descriptive labels (e.g., "B01001_003E = Estimates of Male Population Under 5 years ... " )
##		downloaded from API documentation page and pasted into a .csv
acs.labels <- setDT (
	read_csv(path(upPlace,"upinfo","B01001_labels.csv")),                # !! update path
	key="Name"                                    # define Name as the key for this file
)

## 2.4 	ACS data: tract-level population by age/sex (long format, count and MOE in columns)
.acsyears <- 2009:2016 # first year of 5-yr ACS is 2009
doGetAcs <- function(year) {
	acs.pop.tracts <- 	get_acs(state = 06, geography = "tract",  # all tracts in CA (FIPS state code = 06)
							   survey = "acs5", year = year,         # ACS options
							   variables = acs.varlist,              # ACS variable requested (B01003_001 = total population)
							   key=.ckey, moe_level=90               # global variable containing Census API key
	)
	acs.pop.tracts$year<-year 									  # add in the year of the extract
	return(acs.pop.tracts)	
}
acs.pop.tracts <- lapply(.acsyears, doGetAcs)					# loop over multiple years
acs.pop.tracts <- rbindlist(acs.pop.tracts)						# convert from list to frame

## 2.5 	ACS data: county-level population by age/sex (long format, count and MOE in columns)
# acs.pop.county <- get_acs(state = 06, geography = "county",
# 			survey = "acs5", year = 2015,
# 			variables = acs.varlist,
# 			key=.ckey, moe_level=90
# )

## 2.6	DOF data: county-level population by county/sex/age
##		several	options here. (1): P-3 file (compressed CSV file with complete age/race/sex/ethnic detail)
##							  (2): P-2 file (5-year age groups by sex, xlsx file)
##							  (3): data.ca.gov portal via API (sex, age); SKIP county text label and pop_total
##		sex: string
dof.pop.county <- 	setDT(
	read_csv("https://data.ca.gov/sites/default/files/dof_dru_pop_1970_2050_csya_wide.csv",
			 col_types="i_iiii_"
	)
)

## 2.7 	Census variable list
##		several options here. (1): PCT012 files contain sex/single year of age to 100 (+100-104,105-109,110+)
##                            by race: all / NH: w b n a p i o m / H: w b n a p i o m
##							  (2): P012 files contain sex by age (49 groups) by race (w b n a p i o m h wnh)
##							  (3): PCT013 files contain as P012, but for household population only.
##							  get list from https://api.census.gov/data/2010/sf1/variables.html
dec.varlist <- sprintf("PCT012%04d",c(3:104,106:209))          # PCT012* except totals by sex

## 2.8 	Census variable descriptive labels (e.g., "PCT0120002 = ")
## 		beware: as of 2018, the API is returning erroneous labels for the female total pop. 
##		https://api.census.gov/data/2010/sf1/variables.html. the csv file is fixed.
dec.labels <- setDT(
	read_csv(path(upPlace,"upinfo","PCT012_labels.csv"), col_names=TRUE),              # !! update path
	key="Name"                                					# define Name as the key for this file
)

## 2.9	Census data: tract, county, state-level population by age/sex
dec.pop.tracts <-	get_decennial(state = 06, geography="tract",  # all tracts in CA (FIPS code 06)
								variables = dec.varlist,                  # DEC variable requested (PCT012 = pop by sex/age)
								key=.ckey
)
dec.pop.county <-	get_decennial(state = 06, geography="county", # all counties in CA (FIPS code 06)
								variables = dec.varlist,                  # DEC variable requested (PCT012 = pop by sex/age)
								key=.ckey
)
dec.pop.state <-	get_decennial(state = 06, geography="state",  # CA (FIPS code 06)
							   variables = dec.varlist,                  # DEC variable requested (PCT012 = pop by sex/age)
							   key=.ckey
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
		   ][age=="85 years and over", age := "85 to 199 years"] # relabel age groups to provide upper, lower ages
acs.labels[, c("agell","ageul") 
		   := tstrsplit(age, " ", keep=c(1,3))]                  # parse age ranges out of age label
acs.labels[is.na(ageul), ageul 
		   := agell]                                             # replace NA values of age with the lower age
acs.labels[, age:=NULL]                                          # drop full text age label

## 	3.1.2 	add age and sex labels for ACS tracts data
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

##	3.1.3	recode age categories for ACS tracts data
##			collapse 23 ACS age groups into the same 18 groups (drop 18 21 22 62 67)
.maxage<-85
acs.pop.tracts[, agell:=as.integer(agell)]				# eventually move this fix to pop.make.r
acs.pop.tracts[, agell:=5*floor(agell/5)]						 # round to 5-yr ages (17 groups, no '1-4')
acs.pop.tracts[agell<.maxage, ageul:=agell+4]						 # update new age brackets
acs.pop.tracts[agell>=.maxage, ageul:=199]						 # update new age brackets
acs.pop.tracts[, ageul:=as.numeric(ageul)]
acs.pop.tracts<-acs.pop.tracts[,.(estimate=sum(estimate)),
							   by=.(GEOID,year,sex,agell,ageul)]				 # collapse

# 	3.1.4	collapse ACS tract data into MSSAs
#!! 		this method does not export tracts which do not match, which previously were exported to subTracts.csv
#!! 		choose whether to merge and collapse to MSSA level or merely merge MSSA labels
setkey(acs.pop.tracts,"GEOID") 									 # set GEOID as key for this file for merging
acs.pop.mssa <- acs.pop.tracts[cbd.link,nomatch=0                # merge tracts data with cbd.link	
							   ][,.(estimate=sum(estimate)),     
							     by=.(comID,year,sex,agell,ageul)] 	 # calculate the sum of estimates by MSSA id variables

##	3.2 	DOF datasets
##	3.2.1	DOF county
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
##	3.2.2	DOF state
dof.pop.state <- dof.pop.county[,.(estimate=sum(estimate)),
								by=.(year,sex,age)]              # collapse county detail
dof.pop.state[,GEOID:="06000000000"]                             # new GEOID for state level dataset

## 	3.3 	ACS estimates controlled to DOF total population
##			method: 
##			!! TBD


##  3.4		DEC datasets (tract and MSSA)
##			ages 0,1,5...100+

##  3.4.1	cleanup DEC labels read from external file
dec.labels <- dec.labels[grepl("PCT0120",Name,fixed=TRUE)]		# keep PCT120 only (sex by age)
dec.labels[, c("type","strata","sex","age")						# parse elements of Label string using "!!"
		   := tstrsplit(Label, "!!")]
dec.labels[, sex:=toupper(sex)]                                 # capitalize sex for consistency
dec.labels <- dec.labels[, c("Name","sex","age")][!is.na(age)]  # retain only needed columns, and drop TOTAL rows
dec.labels[age==" Under 1 year", age := " 0"]  			        # relabel age groups to provide upper, lower ages
dec.labels[, age := strtoi(word(age,2))]						# watch this carefully -- "age" is padded with a space, so it's the '2nd word'
dec.labels[, sex := gsub(' |:', '', word(sex,1))]				# take the first word (total/male/female) and strip spaces and colons

## 	3.4.2	add age and sex labels for DEC tract data.
setDT(dec.pop.tracts)
.maxage=100														# topcode ages over maxage
dec.pop.tracts[, NAME := NULL]									# drop name
dec.pop.tracts[, year := 2010]									# add 2010 year
names(dec.pop.tracts)[names(dec.pop.tracts) 
					  == "variable"] = "Name"                   # rename variable to Name (to merge with varlabels)
setkey(dec.pop.tracts,"Name")									# sort and merge on name
dec.pop.tracts<-merge(dec.pop.tracts,
					  dec.labels)[!is.na(age)]					# add labels to dataset, drop values w/o labels.
dec.pop.tracts[age==0, c("agell","ageul") := 0]					# create age categories
dec.pop.tracts[age>=1 & age<=4, ':=' (agell=1, ageul=4)]
dec.pop.tracts[age>=5 & age<.maxage, agell := (5*floor(age/5))]
dec.pop.tracts[age>=5 & age<.maxage, ageul := agell+4]
dec.pop.tracts[age>=.maxage, ':=' (agell=.maxage, ageul=199)]
setkey(dec.pop.tracts,"GEOID")
dec.pop.tracts<-dec.pop.tracts[, .(nx=sum(value)), 
							   by=.(GEOID,year,sex,agell,ageul)]				# collapse sum of tract pop by age groups
# they may have to be collapsed further
# if used for lifetables to accord to abridged LT.
# but caution in doing that b/c county and state 
# are generated from aggregating tracts, so
# topcoding age in tracts here will topcode county/state also.
# there is however some age heaping, other issues at 90+

## 	3.4.3	DEC totals by agegroup/sex/tract and MSSA
dec.pop.mssa <- dec.pop.tracts[cbd.link,nomatch=0,on='GEOID'        # merge tracts data with cbd.link	
							   ][,.(nx=sum(nx)),     
							     by=.(comID,year,sex,agell,ageul)] 	# calculate the sum of estimates by MSSA id variables
# dec.pop.mssa[agell>=90 & nx==0]									# inspect for empty age groups

## 	3.4.4	DEC county
##### 		DEC COUNTY/STATE CAN BE RE-DONE FROM TIDYCENSUS PULLS OF THOSE GEOGRAPHIES.
##### 		TO DO THAT BEST, HAVE TO CONVERT THE CODE TO CLEAN TRACTS INTO SCRIPTS.
##### 		otherwise, the tract->county->state results should be checked.
dec.pop.county <- dec.pop.tracts[,.(nx=sum(nx)),					# collapse county detail from tracts
								 by=.(GEOID=sprintf("%05s000000",substring(GEOID,1,5)),year,sex,agell,ageul)]	    
# sum(dec.pop.county[grepl("6003",GEOID)]$nx)						# check ALPINE pop.

## 	3.4.5	DEC state
##### 		DEC COUNTY/STATE CAN BE RE-DONE FROM TIDYCENSUS PULLS OF THOSE GEOGRAPHIES.
##### 		TO DO THAT BEST, HAVE TO CONVERT THE CODE TO CLEAN TRACTS INTO SCRIPTS.
##### 		otherwise, the tract->county->state results should be checked.
dec.pop.state <- dec.pop.county[,.(nx=sum(nx)),						# collapse state detail from counties 
								by=.(GEOID=rep("0600000000",2552),year,sex,agell,ageul)]	        
# sum(dec.pop.state$nx)												# check STATE pop.

##	4	CLEANUP		----------------------------------------------------------------------
#!! 	consider whether to use rda (regular rdata file with many objects) or rds (single object datafile, assignable)
#!!		a pro of the RDS format is that it is a single object, reassignable when read
#!! 	either way, recommend updating other file suffixes using rda or rds to distinguish from r code
#		SAVE AS RDA: save(acs.pop.mssa, file=paste0(upPlace,"/upData/popCensusCom.rda"))  save R dataset using load()
#		SAVE AS RDS: saveRDS(acs.pop.county, file=paste0(upPlace,"/upData/popCensusCounty.rds")) # save R data object using readRDS()

##	4.2	export results for CCB calculations: county, tract, MSSA level datasets
##		age17: 0,5,10,15,...85+
##		age18: 0,1,5,10,15,...85+
##		age23: 0,5,10,15,20,21,22,25,...60,62,65,67,70,...85+
##	  	age 111: 0,1,2,3,4,...110+
saveRDS(acs.pop.tracts, file=path(upPlace,"lifeTables","lifeWork","popTract.rds"))  		# GEOID year sex (char) age17 estimate
saveRDS(acs.pop.mssa,   file=path(upPlace,"lifeTables","lifeWork","popMSSA.rds"))  		# comID year sex (char) age17 estimate
saveRDS(dof.pop.county, file=path(upPlace,"lifeTables","lifeWork","popCounty.rds")) 		# GEOID year sex (char) age111 estimate
saveRDS(dof.pop.state,  file=path(upPlace,"lifeTables","lifeWork","popState.rds"))  		# GEOID year sex (char) age111 estimate

##	4.3	export datasets for lifetables using DEC.
saveRDS(dec.pop.tracts, file=path(upPlace,"lifeTables","lifeWork","decTract.rds"))		# GEOID year sex (char) age23 nx (for LT)
saveRDS(dec.pop.mssa,   file=path(upPlace,"lifeTables","lifeWork","decMSSA.rds"))			# comID year sex (char) age23 nx (for LT)
saveRDS(dec.pop.county, file=path(upPlace,"lifeTables","lifeWork","decCounty.rds"))		# GEOID year sex (char) age23 nx (for LT)
saveRDS(dec.pop.state,  file=path(upPlace,"lifeTables","lifeWork","decState.rds"))		# GEOID year sex (char) age23 nx (for LT)

# END
