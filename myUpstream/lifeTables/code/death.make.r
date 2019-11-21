# title: death.make.r
# purpose: summarize deaths for CCB project life tables (state, county, tract, mssa)

## 1    SETUP		----------------------------------------------------------------------

## 1.1  packages
.pkg	<- c("data.table","readr","readxl","readstata13") 
.inst   <- .pkg %in% installed.packages() 
if(length(.pkg[!.inst]) > 0) install.packages(.pkg[!.inst]) 
lapply(.pkg, library, character.only=TRUE)           

## 1.2  options
whichData     <- "dof"   # "real" or "fake" or "dof"

## 1.3 paths 
myDrive <- getwd()
myPlace <- paste0(myDrive,"/myCBD") 
upPlace <- paste0(myDrive,"/myUpstream") 

## 1.4 links
if (whichData=="fake") .deaths		<- paste0(upPlace,"/upData/cbdDat0SAMP.R") # raw file with deaths
if (whichData=="real") .deaths		<- "h:/0.Secure.Data/myData/cbdDat0FULL.R" 
if (whichData=="dof")  .deaths 		<- "c:/users/fieshary/desktop/dofDat0FULL.dta"
.cbdlink	<- paste0(myPlace,"/myInfo/Tract to Community Linkage.csv") # map tract level GEOID to comID
.countylink <- paste0(myPlace,"/myInfo/County Codes to County Names Linkage.xlsx") # map county names to codes

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
if (whichData=="fake" | whichData=="real") load(.deaths) # named cbdDat0SAMP
if (whichData=="real") cbdDat0SAMP <- cbdDat0FULL # rename cbdDat0SAMP to work with rest of code
if (whichData=="dof")  cbdDat0SAMP <- read.dta13(.deaths)

## 2.4 	inspect missingness
setDT(cbdDat0SAMP)
cbdDat0SAMP[GEOID=="",GEOID:=NA] # recode empty GEOID to missing
colMeans(!is.na(cbdDat0SAMP[year %in% 2010:2017])) # % nonmissing: stateFIPS 100%, county 99.9%, GEOID 96.9%

## 2.5  inspect tracts coded in different county
cbdDat0SAMP[GEOID!="" & countyFIPS!=substr(GEOID,3,5)]  # deaths may be coded to tracts not in the same county

## 2.6 rename county->countyName
setnames(cbdDat0SAMP, "county", "countyName")

## 3	ANALYSIS	----------------------------------------------------------------------

## 3.1	clean/summarize function -- note that DT does not return an object; edits in place.
ageCats <- function(dat,first=NULL) {
	if (first=="1-4") {
		dat[age==0 & age<=4, ':=' (agell=0,ageul=4)]		# first age group is 0-4, per ACS
	}
	if (first=="0") {
		dat[age==0, ':=' (agell=0,ageul=0)]					# first age group is 0 only, per DOF
		dat[age>=1 & age<=4, ':=' (agell=1,ageul=4)] 
	}
	dat[age>=5 & age<85, agell := 5*floor(age/5)]
	dat[age>=5 & age<85, ageul := agell+4]
	dat[age>=85, ':=' (agell=85,ageul=199)]					# open-ended age group
	dat[sex=="F",sex:="FEMALE"]								# change sex string to be unambiguous
	dat[sex=="M",sex:="MALE"]								# and to match with population files
}

# 3.2 extract tract data
dx.tract <- copy(setkey(cbdDat0SAMP, "GEOID"))
ageCats(dx.tract,first="1-4") 
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

# 3.3 extract county data
dx.county <- copy(setkey(cbdDat0SAMP, "countyName"))
ageCats(dx.county, first="0")
dx.county[, dx := 1]												# generate a count variable (1=person level file)
dx.county<-dx.county[,.(dx=sum(dx)), 
				   by=.(year,countyName,sex,agell,ageul)]			# collapse
dx.county<-rbind(dx.county,dx.county[,.(dx=sum(dx)),                # both sexes combined totals
					by=.(year,countyName,agell,ageul)],fill=TRUE) 
dx.county[is.na(sex),sex:="TOTAL"]								
dx.county <- dx.county[county.link,nomatch=0,on='countyName'][,	    # merge using countyName to get 'GEOID' for county	
						.(dx=sum(dx)),     
						by=c("GEOID","year","sex","agell","ageul")] # collapse
dx.county<-dx.county[as.numeric(GEOID)>=06000000000 & 
					 	as.numeric(GEOID)<=06115000000 &	        # nonmissing county
					 	sex %in% c("MALE","FEMALE","TOTAL") &		# nonmissing sex
					 	!is.na(agell)]								# nonmissing age

# 3.4 extract state data
dx.state<-copy(setkey(cbdDat0SAMP, "stateFIPS"))
ageCats(dx.state, first="0")
dx.state[, dx := 1]													# generate a count variable (1=person level file)
dx.state<-dx.state[,.(dx=sum(dx)), 
				   by=.(year,sex,agell,ageul)]						# collapse
dx.state<-rbind(dx.state,dx.state[,.(dx=sum(dx)),                   # both sexes combined totals
								  by=.(year,agell,ageul)],fill=TRUE) 
dx.state[is.na(sex),sex:="TOTAL"]								
dx.state<-dx.state[,.(dx=sum(dx)),
				   by=.(year,sex,agell,ageul)] 						# collapse
dx.state[, GEOID:="06000000000"]									# add GEOID
dx.state<-dx.state[sex %in% c("MALE","FEMALE","TOTAL") & 
				   	!is.na(agell)]									# nonmissing sex + age

# 3.5 extract mssa data
dx.mssa<-dx.tract[cbd.link,nomatch=0,on='GEOID'][	                # merge tracts data with cbd.link	
					!is.na(as.numeric(GEOID))][,  					# drop if missing GEOID
					.(dx=sum(dx)), 
					by=c("comID","year","sex","agell","ageul")] 
setkeyv(dx.mssa,c("comID","sex","agell"))							# sort 

## 4	EXPORT		----------------------------------------------------------------------

##  4.1 export datasets

if (whichData == "fake"  ) {
 .midPath <- "/lifeTables/dataOut/"
 saveRDS(dx.tract,  file=paste0(upPlace,.midPath,"dxTract.rds"))   # output deaths by tract
 saveRDS(dx.mssa,   file=paste0(upPlace,.midPath,"dxMSSA.rds"))    # output deaths by mssa
 saveRDS(dx.county, file=paste0(upPlace,.midPath,"dxCounty.rds"))  # output deaths by county
 saveRDS(dx.state,  file=paste0(upPlace,.midPath,"dxState.rds"))   # output deaths by state
}
if (whichData == "real"  ) {
 .midPath <- "h:/0.Secure.Data/myData/"
 saveRDS(dx.tract,  file=paste0(.midPath,"dxTract.rds"))   # output deaths by tract
 saveRDS(dx.mssa,   file=paste0(.midPath,"dxMSSA.rds"))    # output deaths by mssa
 saveRDS(dx.county, file=paste0(.midPath,"dxCounty.rds"))  # output deaths by county
 saveRDS(dx.state,  file=paste0(.midPath,"dxState.rds"))   # output deaths by state
}
if (whichData == "dof"  ) {
	.midPath <- "c:/users/fieshary/desktop/"
	saveRDS(dx.county, file=paste0(.midPath,"dxCounty.rds"))  # output deaths by county
	saveRDS(dx.state,  file=paste0(.midPath,"dxState.rds"))   # output deaths by state
}


# END
