# title: death.make-ccb.r
# purpose: datasets of deaths (numerators) for CCB project (state, county, tract, mssa)
# notes:
#	death data should be updated to include month of death
#	also the GEOID is problematic, many times it's wrong or missing compared to zip/county
# history:
#	2018/7/19: fixing ccb death tracts to move mock data to 2.2 (was 3.1); 
#			   summarizing ages to age17 (in common with ACS)
#	2018.5.25: forked from death.make.v01 (to use CCB project death dataset)
#	2018.5.14: first working version. (using CDPH data held by DOF)
#	2018.5.08: project began

## 1    SETUP		----------------------------------------------------------------------

## 1.1  packages
.pkg	<- c("data.table","readr","fs") ### MICHAEL ADDED fs 
.inst   <- .pkg %in% installed.packages() 
if(length(.pkg[!.inst]) > 0) install.packages(.pkg[!.inst]) 
lapply(.pkg, library, character.only=TRUE)           

## 1.2  path and globals
#.deaths <- "forEthan.RDS"											# !! update filename

#.path   <- "d:/Users/fieshary/projects/mortality_ccb_lt/ccb_lifetables"		# !! update path to working dir

.path  <- "e:/0.CBD"  ### MICHAEL

setwd(.path)

### MICHAEL
myPlace    <- path(.path,"myCBD")
upPlace    <- path(.path,"myUpstream")

# REAL !!!
load("f:/0.Secure.Data/myData/cbdDat0FULL.R")    
.deaths <- cbdDat0FULL
# FAKE!
#.deaths <- load(paste0(upPlace,"/upData/cbdDat0SAMP.R"))    
# load(paste0(upPlace,"/upData/cbdDat0SAMP.R"))  
# .deaths <- cbdDat0SAMP



## 2	DATASETS	----------------------------------------------------------------------

## 2.1 	tract-to-MSSA crosswalk
##		contains these variables: year GEOID county comID comName, where GEOID is the tract.
cbd.link <- setDT(
	read_csv(path(myPlace,"myInfo","cbdLinkCA.csv"), col_types="icccc"),                   # !! update path
	key="GEOID"	                                  					# set key for merging later
)
cbd.link[, countyFIPS:=substr(GEOID,1,5)]

## 2.2	CDPH deaths microdata -- CCB datasets
##		contains these variables: year state county zip GEOID countyFIPS stateFIPS age
##		!! update when "month" field is added so that it is not overwritten.
##		!! there seem to be problems with the GEOID!!
ccb.dead.tracts <- setDT(.deaths, key="GEOID")  # MICHAEL
ccb.dead.tracts[, month:=sample(1:12, 
						length(ccb.dead.tracts$year), replace=T)]	# set a random month (later, remove this)
ccb.dead.tracts[, year:=year+1]										# fake 2016 data, since using 2016 ACS

## 3. 	analysis

## 3.1 	clean death files
##		remove bad/missing GEO/age data (later, fix)
ccb.dead.tracts<-ccb.dead.tracts[countyFIPS<=105 & 					# keep positively geocoded CA counties only
								 sex %in% c("M","F") &				# nonmissing sex
								 !is.na(age)]						# nonmissing age
ccb.dead.tracts[, date := sprintf("%d-%02d", year, month)]			# combine year-month into one date
ccb.dead.tracts[, c("year","month"):=NULL]							# drop year-month (keep date only)
ccb.dead.tracts[, dx := 1]											# generate a count variable (1=person level file)
.maxage=85															# topcode ages above maxage
ccb.dead.tracts[age<.maxage, agell := 5*floor(age/5)]
ccb.dead.tracts[age<.maxage, ageul := agell+4]
ccb.dead.tracts[age>=.maxage, ':=' (agell=85,ageul=199)]			# open-ended age group
ccb.dead.tracts[sex=="F",sex:="FEMALE"]								# change sex string to be unambiguous
ccb.dead.tracts[sex=="M",sex:="MALE"]								# and to match with population files

## 3.2 MSSA death totals 
ccb.dead.mssa <- ccb.dead.tracts[cbd.link,nomatch=0,on='GEOID'][	# merge tracts data with cbd.link	
					!is.na(as.numeric(GEOID))][,  					# drop missing GEOID
					.(dx=sum(dx)),     
					by=c("comID","date","sex","agell","ageul")] 
setkeyv(ccb.dead.mssa,c("comID","sex","agell"))						# sort 

## 3.3 county death totals
ccb.dead.county <- ccb.dead.tracts[cbd.link,nomatch=0,on='GEOID'][
					!is.na(countyFIPS)][,
					.(dx=sum(dx)),
					by=c("countyFIPS","date","sex","agell","ageul")]	
ccb.dead.county[, GEOID:=sprintf("06%03s000000",countyFIPS)]		# create a GEOID from countyFIPS
ccb.dead.county[,countyFIPS:=NULL]									# drop other identifier
setkeyv(ccb.dead.county,c("GEOID","sex","agell"))					# sort 

## 3.4 state death totals
ccb.dead.state<-ccb.dead.tracts[,
						.(dx=sum(dx)), 
						by=c("sex","date","agell","ageul")]			# collapse deaths 
ccb.dead.state[, GEOID:="06000000000"]								# add GEOID
setkeyv(ccb.dead.state,c("GEOID","sex","agell"))					# sort

## 3.5 summarize tract deaths
ccb.dead.tracts<-ccb.dead.tracts[!is.na(as.numeric(GEOID))][,		# drop missing GEOID
						.(dx=sum(dx)), 								# sum of deaths
						by=c("GEOID","date","sex","agell","ageul")]	# by age groups, yyyy-mm and GEOID

## 	4	cleanup and export

##  4.1 export datasets
## 		!!! there are only 66% of deaths in the file Michael sent that are counted.


# MICHAEL
.outPath <- path(upPlace,"lifeTables","lifeWork")

saveRDS(ccb.dead.tracts, file=path(.outPath,"dxTract.rds"))		# GEOID date sex (char) age17 dx (for LT)
saveRDS(ccb.dead.mssa,   file=path(.outPath,"dxMSSA-ccb.rds"))		# comID date sex (char) age17 dx (for LT)
saveRDS(ccb.dead.county, file=path(.outPath,"dxCounty.rds"))		# GEOID date sex (char) age17 dx (for LT)
saveRDS(ccb.dead.state,  file=path(.outPath,"dxState.rds"))		# GEOID date sex (char) age17 dx (for LT)




