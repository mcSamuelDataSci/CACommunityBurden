# title: death.make.r
# purpose: calculate deaths for CCB project life tables (state, county, tract, mssa)
# notes:
#	if death data for 2015+ are included, edit the code to stop changing the year of death to year+2
#	also the GEOID is problematic, many times it's wrong or missing compared to zip/county
# history:
#	2019.02.01: moved to GitHub; changing paths; removed version number from filename.
#				not using GEOID in rawdata for county/state (using least-missing variable instead)
#	2018.10.04: simplifying code to work with last 5-yr acs. fixing residence condition.
#	2018.07.19: fixing ccb death tracts to move mock data to 2.2 (was 3.1); 
#			   	summarizing ages to age17 (in common with ACS)
#	2018.05.25: forked from death.make.v01 (to use CCB project death dataset)
#	2018.05.14: first working version. (using CDPH data held by DOF)
#	2018.05.08: project began

## 1    SETUP		----------------------------------------------------------------------

## 1.1  packages
.pkg	<- c("data.table","readr","readxl") 
.inst   <- .pkg %in% installed.packages() 
if(length(.pkg[!.inst]) > 0) install.packages(.pkg[!.inst]) 
lapply(.pkg, library, character.only=TRUE)           

## 1.2  path and globals
.path       <- "c:/users/fieshary/projects/CACommunityBurden" # project directory
.deaths		<- paste0(.path,"/myUpstream/upData/cbdDat0SAMP.R") # was 'forEthan.RDS'; raw file with deaths
.cbdlink	<- paste0(.path,"/myCBD/myInfo/Tract to Community Linkage.csv") # map tract level GEOID to comID
.countylink <- paste0(.path,"/myCBD/myInfo/County Codes to County Names Linkage.xlsx") # map county names to codes
.dxtract	<- paste0(.path,"/data/dxTract.rds") # output deaths by tract
.dxmssa		<- paste0(.path,"/data/dxMSSA.rds") # output deaths by mssa
.dxcounty	<- paste0(.path,"/data/dxCounty.rds") # output deaths by county
.dxstate	<- paste0(.path,"/data/dxState.rds") # output deaths by state
setwd(.path)

## 2	DATASETS	----------------------------------------------------------------------

## 2.1 	tract-to-MSSA crosswalk
cbd.link <- setDT(
	read_csv(.cbdlink, col_types="icccc"),
	key="GEOID"
)
cbd.link[, countyFIPS:=substr(GEOID,1,5)]

## 2.2  name-to-code county labels
county.link <- setDT(
	read_xlsx(.countylink),
	key="countyName"
)
county.link[, GEOID:=paste0("06",FIPSCounty,"000000")]

## 2.3	CDPH deaths microdata -- CCB datasets
load(.deaths) # named cbdDat0SAMP
colMeans(!is.na(cbdDat0SAMP)) # % nonmissing: stateFIPS 100%, county 99.9%, GEOID 73%
setDT(cbdDat0SAMP)
setnames(cbdDat0SAMP, "county", "countyName")

## 3	ANALYSIS	----------------------------------------------------------------------

## 3.1	clean/summarize function -- note that DT does not return an object; edits in place.
ageCats <- function(dat) {
	dat[age==0 & age<=4, ':=' (agell=0,ageul=4)]				# first age group is 0-4, per ACS
	dat[age>=5 & age<85, agell := 5*floor(age/5)]
	dat[age>=5 & age<85, ageul := agell+4]
	dat[age>=85, ':=' (agell=85,ageul=199)]						# open-ended age group
	dat[sex=="F",sex:="FEMALE"]									# change sex string to be unambiguous
	dat[sex=="M",sex:="MALE"]								    # and to match with population files
}

# tract
dx.tract <- copy(setkey(cbdDat0SAMP, "GEOID"))
dx.tract[, month:=sample(1:12, length(dx.tract$year), replace=T)]	# fake months; ERASE LATER
dx.tract[, year:=year+2]										    # fake years; ERASE LATER
ageCats(dx.tract) 
dx.tract[, dx := 1]													# generate a count variable (1=person level file)
dx.tract<-dx.tract[,.(dx=sum(dx)), 
				   by=.(year,GEOID,sex,agell,ageul)]				# collapse
dx.tract<-rbind(dx.tract,dx.tract[,.(dx=sum(dx)),                   # both sexes combined totals
				   by=.(year,GEOID,agell,ageul)],fill=TRUE) 
dx.tract[is.na(sex),sex:="TOTAL"]								
dx.tract<-dx.tract[,.(dx=sum(dx)),
				   by=.(year,GEOID,sex,agell,ageul)] 				# collapse
dx.tract<-dx.tract[as.numeric(GEOID)>=06000000000 & 
				   	as.numeric(GEOID)<=06999999999 &	            # nonmissing tract
				   	sex %in% c("MALE","FEMALE","TOTAL") &			# nonmissing sex
				   	!is.na(agell)]									# nonmissing age
# county
dx.county <- copy(setkey(cbdDat0SAMP, "countyName"))
ageCats(dx.county)
dx.county[, dx := 1]												# generate a count variable (1=person level file)
dx.county<-dx.county[,.(dx=sum(dx)), 
				   by=.(year,countyName,sex,agell,ageul)]			# collapse
dx.county<-rbind(dx.county,dx.county[,.(dx=sum(dx)),                # both sexes combined totals
					by=.(year,countyName,agell,ageul)],fill=TRUE) 
dx.county[is.na(sex),sex:="TOTAL"]								
dx.county <- dx.county[county.link,nomatch=0,on='countyName'][,	    # merge to get 'GEOID' for county	
						.(dx=sum(dx)),     
						by=c("GEOID","year","sex","agell","ageul")] # collapse
dx.county<-dx.county[as.numeric(GEOID)>=06000000000 & 
					 	as.numeric(GEOID)<=06115000000 &	        # nonmissing county
					 	sex %in% c("MALE","FEMALE","TOTAL") &		# nonmissing sex
					 	!is.na(agell)]								# nonmissing age
# state
dx.state<-copy(setkey(cbdDat0SAMP, "stateFIPS"))
ageCats(dx.state)
dx.state[, dx := 1]													# generate a count variable (1=person level file)
dx.state<-dx.state[,.(dx=sum(dx)), 
				   by=.(year,sex,agell,ageul)]						# collapse
dx.state<-rbind(dx.state,dx.state[,.(dx=sum(dx)),                   # both sexes combined totals
								  by=.(year,agell,ageul)],fill=TRUE) 
dx.state[is.na(sex),sex:="TOTAL"]								
dx.state<-dx.state[,.(dx=sum(dx)),
				   by=.(year,sex,agell,ageul)] 						# collapse
dx.state[, GEOID:="06000000000"]									# add GEOID
dx.state<-dx.state[sex %in% c("MALE","FEMALE") & !is.na(agell)]		# nonmissing sex + age
# mssa
dx.mssa<-dx.tract[cbd.link,nomatch=0,on='GEOID'][	                # merge tracts data with cbd.link	
					!is.na(as.numeric(GEOID))][,  					# drop if missing GEOID
					.(dx=sum(dx)), 
					by=c("comID","year","sex","agell","ageul")] 
setkeyv(dx.mssa,c("comID","sex","agell"))							# sort 

## 4	EXPORT		----------------------------------------------------------------------

##  4.1 export datasets
saveRDS(dx.tract,  file=.dxtract)
saveRDS(dx.mssa,   file=.dxmssa)
saveRDS(dx.county, file=.dxcounty)
saveRDS(dx.state,  file=.dxstate)

# END
