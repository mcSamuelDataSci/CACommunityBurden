# title: death.make-ccb.r
# purpose: datasets of deaths (numerators) for CCB project (state, county, tract, mssa)
# notes:
#	death data should be updated to include month of death
#	once death data for 2015+ are included, edit the code to stop changing the year of death to year+2
#	also the GEOID is problematic, many times it's wrong or missing compared to zip/county
# history:
#	2018.10.4: simplifying code to work with last 5-yr acs. fixing residence condition.
#	2018/7/19: fixing ccb death tracts to move mock data to 2.2 (was 3.1); 
#			   summarizing ages to age17 (in common with ACS)
#	2018.5.25: forked from death.make.v01 (to use CCB project death dataset)
#	2018.5.14: first working version. (using CDPH data held by DOF)
#	2018.5.08: project began

## 1    SETUP		----------------------------------------------------------------------

## 1.1  packages
.pkg	<- c("data.table","readr") 
.inst   <- .pkg %in% installed.packages() 
if(length(.pkg[!.inst]) > 0) install.packages(.pkg[!.inst]) 
lapply(.pkg, library, character.only=TRUE)           

## 1.2  path and globals
.path      	<- "c:/users/iisan7/desktop/mortality_ccb_lt/ccb_lifetables/"
.deaths		<- "data/forEthan.RDS"
.cbdlink	<- "data/cbdLinkCA.csv"
.dxtract	<- "data/dxTract.rds"
.dxmssa		<- "data/dxMSSA.rds"
.dxcounty	<- "data/dxCounty.rds"
.dxstate	<- "data/dxState.rds"
setwd(.path)

## 2	DATASETS	----------------------------------------------------------------------

## 2.1 	tract-to-MSSA crosswalk
cbd.link <- setDT(
	read_csv(.cbdlink, col_types="icccc"),
	key="GEOID"	                                  					# set key for merging later
)
cbd.link[, countyFIPS:=substr(GEOID,1,5)]

## 2.2	CDPH deaths microdata -- CCB datasets
## 		!!! there are only 66% of deaths in the file Michael sent that have valid GEOID.
ccb.dead.tracts <- setDT(readRDS(.deaths), key="GEOID")
ccb.dead.tracts[, month:=sample(1:12, 
						length(ccb.dead.tracts$year), replace=T)]	# ERASE LATER
ccb.dead.tracts[, year:=year+2]										# ERASE LATER
ccb.dead.tracts<-ccb.dead.tracts[as.numeric(GEOID)>=06000000000 & 
								 as.numeric(GEOID)<=06999999999 &	# nonmissing tract
								 sex %in% c("M","F") &				# nonmissing sex
								 !is.na(age)]						# nonmissing age

## 3	ANALYSIS	----------------------------------------------------------------------

## 3.1	clean/summarize deaths
##		convert ages to age groups
ccb.dead.tracts[age==0, ':=' (agell=0,ageul=1)]
ccb.dead.tracts[age>=1 & age<=4, ':=' (agell=1,ageul=4)]
ccb.dead.tracts[age>=5 & age<85, agell := 5*floor(age/5)]
ccb.dead.tracts[age>=5 & age<85, ageul := agell+4]
ccb.dead.tracts[age>=85, ':=' (agell=85,ageul=199)]					# open-ended age group
ccb.dead.tracts[sex=="F",sex:="FEMALE"]								# change sex string to be unambiguous
ccb.dead.tracts[sex=="M",sex:="MALE"]								# and to match with population files
##		summarize; generate "ALL" sex
ccb.dead.tracts[, dx := 1]											# generate a count variable (1=person level file)
ccb.dead.tracts<-ccb.dead.tracts[,.(dx=sum(dx)),
							by=.(year,GEOID,sex,agell,ageul)]		# collapse
ccb.dead.tracts<-rbind(ccb.dead.tracts,ccb.dead.tracts[,.(dx=sum(dx)),
							by=.(year,GEOID,agell,ageul)],fill=TRUE)
ccb.dead.tracts[is.na(sex),sex:="ALL"]								# both sexes

##		summarize countes/states
ccb.dead.tracts[,countyFIPS:=substr(GEOID,1,5)]
ccb.dead.county<-ccb.dead.tracts[,.(dx=sum(dx)),
							by=.(year,countyFIPS,sex,agell,ageul)]	# collapse
ccb.dead.county[, GEOID:=sprintf("%05s000000",countyFIPS)]			# create a GEOID from countyFIPS
ccb.dead.county[,countyFIPS:=NULL]
ccb.dead.state<-ccb.dead.county[,.(dx=sum(dx)), 
						by=c("sex","year","agell","ageul")]			# collapse deaths 
ccb.dead.state[, GEOID:="06000000000"]								# add GEOID
##		collapse lowest age group and summarize tracts
ccb.dead.tracts[agell>=0 & agell<=4, ':=' (agell=0,ageul=4)]
ccb.dead.tracts<-ccb.dead.tracts[,.(dx=sum(dx)),
							by=.(year,GEOID,sex,agell,ageul)]	 	# collapse

## 3.2 	MSSA death totals 
ccb.dead.mssa <- ccb.dead.tracts[cbd.link,nomatch=0,on='GEOID'][	# merge tracts data with cbd.link	
					!is.na(as.numeric(GEOID))][,  					# drop missing GEOID
					.(dx=sum(dx)),     
					by=c("comID","year","sex","agell","ageul")] 
setkeyv(ccb.dead.mssa,c("comID","sex","agell"))						# sort 

## 4	EXPORT		----------------------------------------------------------------------

##  4.1 export datasets
saveRDS(ccb.dead.tracts, file=.dxtract)
saveRDS(ccb.dead.mssa,   file=.dxmssa)
saveRDS(ccb.dead.county, file=.dxcounty)
saveRDS(ccb.dead.state,  file=.dxstate)

# END
