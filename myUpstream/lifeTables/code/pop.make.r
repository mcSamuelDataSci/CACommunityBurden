# title: pop.make.r
# purpose: datasets of population denominators for CBD project (state, county, tract, mssa)
# note! ACS weighted to same vintage PEP. E.g. 2013-17 5-yr ACS pop = avg of 2017 vintage PEP population for 2013-2017.

## 1    SETUP		----------------------------------------------------------------------

## 1.1  packages
.pkg	<- c("data.table","readr","stringr","tidycensus") 
.inst   <- .pkg %in% installed.packages() 
if(length(.pkg[!.inst]) > 0) install.packages(.pkg[!.inst]) 
lapply(.pkg, require, character.only=TRUE)           

## 1.2  options
doAppend<-c(TRUE,2018) # whether to add missing years up to the year YYYY
controlPop<-TRUE # whether to control ACS to DOF pop totals

## 1.3 paths 
myDrive <- getwd()
myPlace <- paste0(myDrive,"/myCBD") 
upPlace <- paste0(myDrive,"/myUpstream") 

## 1.4 links
.cbdlink	<- paste0(myPlace,"/myInfo/Tract to Community Linkage.csv") # map tract level GEOID to comID
.countylink <- paste0(myPlace,"/myInfo/County Codes to County Names Linkage.xlsx") # map county names to codes
.ckey	    <- read_file(paste0(upPlace,"/upstreamInfo/census.api.key.txt")) # census API key
.clabels    <- paste0(myPlace,"/myInfo/B01001_labels.csv") # labels for fields in B01001 table.
.acsurl		<- paste0(upPlace,"/lifeTables/dataIn/acs5_B01001_tracts.csv.zip") # ACS-5yr pop by tract, pooled 2009-17
.nchsurl	<- paste0(upPlace,"/lifeTables/dataIn/nchsPOP.csv.zip")
.dofurl		<- "https://data.ca.gov/dataset/7a8c03d3-ed86-498a-acdb-8ea09ccb4130/resource/2c217b79-4625-4ab2-86b3-6fc5d66f0409/download/population-estimates-and-projections-by-county-age-and-sex-california-1970-2050.csv"
.afacturl   <- paste0(upPlace,"/lifeTables/dataIn/afact.csv") # ACS-5yr tract share of county population
.nxtract	<- paste0(upPlace,"/lifeTables/dataOut/nxTract.rds") # output deaths by tract
.nxmssa		<- paste0(upPlace,"/lifeTables/dataOut/nxMSSA.rds") # output deaths by mssa
.nxcounty	<- paste0(upPlace,"/lifeTables/dataOut/nxCounty.rds") # output deaths by county
.nxstate	<- paste0(upPlace,"/lifeTables/dataOut/nxState.rds") # output deaths by state

## 2	DATASETS	----------------------------------------------------------------------

## 2.1 	tract-to-MSSA crosswalk
cbd.link <- setDT(read_csv(.cbdlink), key="GEOID")

#
# census api does not guarantee uptime or stability. for example, on 3/22/19 can only access 2015+ 5-year acs.
# therefore, it is preferable to download all B01001 tables from AFF or FTP. 
# as of right now, this is done in acs5_B01001_tracts.do // containing GEOID year sex agell ageul nx for 2009-17
acs.pop.tracts <- setDT(read_csv(.acsurl,col_types="ciciii"))
acs.pop.tracts[,year:=year-2] # population data refer to midpoint of 5-year span

# ## 2.2  ACS data: vector of variable names which contain tract-level population by age/sex
# ##		table S0101	contains sex by ages: 5 year age groups, expressed as % of tract total population
# ##		table B01001 contains sex by ages: 0-4,5-9,10-14,15-17,18-19,20,21,22-24,25-29,
# ##				30-34,35-39,40-44,45-49,50-54,55-59,60-61,62-64,65-66,67-69,70-74,75-79,80-84,85+
# ##		tables B01001A-B01001I contain table B01001 for races: WNH, W, B, N, A, P, O, MR
# ##				with limited ages: 0-4,5-9, 10-14, 15-17, 18-19, 20-24, 25-29, 30-34, 35-44, 45-54, 55-64, 75-84, 85+
# ##		table B01001_002 through B01001_025 refer to males; B01001_026 through B01001_049 refer to females
# ##		get full list from https://api.census.gov/data/2016/acs/acs5/variables.html
#
# ## 2.2.1 list of ACS fields to request
# acs.varlist <- sprintf("B01001_%03dE",c(3:25,27:49)) 		# B01001_*E except totals
# 
# ## 2.2.2 	ACS variable descriptive labels (e.g., "B01001_003E = Estimates of Male Population Under 5 years ... " )
# ##		downloaded from API documentation page and pasted into a .csv; one typo fixed.
# acs.labels <- setDT(read_csv(.clabels),
# 					key="Name"                                    
# )
# 
# ## 2.2.3 	cleanup ACS labels read from external file
# acs.labels[, c("type","strata","sex","age") 
# 		   := tstrsplit(Label, "!!")] 		                     # parse elements of descriptive label using "!!"
# acs.labels[, sex:=toupper(sex)]                                  # capitalize sex for consistency
# acs.labels <- acs.labels[, c("Name","sex","age")]                # retain only needed elements
# acs.labels[, Name := substring(Name,0,10)]                       # remove "E" suffix from label for clean merging
# acs.labels[age=="Under 5 years", age := "0 to 4 years"
# 		   ][age=="85 years and over", age := "85 to 199 years"] # relabel age groups to provide upper, lower ages
# acs.labels[, c("agell","ageul") 
# 		   := tstrsplit(age, " ", keep=c(1,3))]                  # parse age ranges out of age label
# acs.labels[is.na(ageul), ageul 
# 		   := agell]                                             # replace NA values of age with the lower age
# acs.labels[, age:=NULL]                                          # drop full text age label
# acs.labels[, agell:=as.integer(agell)]				# eventually move this fix to pop.make.r
# acs.labels[, ageul:=as.integer(ageul)]				# eventually move this fix to pop.make.r
# 
# ## 2.2.4 	ACS data: tract-level population by age/sex (long format, count and MOE in columns)
# # check if already have these data in memory before pulling again from census
# #
# .acsyears <- 2010:2017 # first year of 5-yr ACS is 2009
# doGetAcs <- function(year) {
# 	acs.pop.tracts <- 	get_acs(state = 06, geography = "tract",  # all tracts in CA (FIPS state code = 06)
# 							   survey = "acs5", year = year,         # ACS options
# 							   variables = acs.varlist,              # ACS variable requested (B01003_001 = total population)
# 							   key=.ckey, moe_level=90               # global variable containing Census API key
# 	)
# 	acs.pop.tracts$year<-year 									  # add in the year of the extract
# 	return(acs.pop.tracts)
# }
# acs.pop.tracts <- lapply(.acsyears, doGetAcs)					# loop over multiple years
# acs.pop.tracts <- rbindlist(acs.pop.tracts)						# convert from list to frame
# 
# ## 2.2.5	merge labels for ACS tracts data
# setDT(acs.pop.tracts) 
# acs.pop.tracts[, NAME:=NULL]                                     # drop geography label (use GEOID only)
# names(acs.pop.tracts)[names(acs.pop.tracts) 
# 					  == "variable"] = "Name"                    # rename variable to Name (to merge with labels)
# setkey(acs.pop.tracts,"Name")                                    # define Name as the key to merge 
# acs.pop.tracts<-merge(
# 	acs.pop.tracts,
# 	acs.labels, 
# 	all.x=TRUE                                   # keeping even if no match in acs.labels
# )                                                # merge ACS tract dataset with with age, sex labels
# 
# ## 2.2.6	recode age categories for ACS tracts data
# ##			collapse 23 ACS age groups into the same 18 groups (drop 18 21 22 62 67)
# ##			ACS data users guide suggests geometric mean (sqrt of sum of squared MOEs) when combining estimates
# .maxage<-85
# acs.pop.tracts[, agell:=(5*floor(agell/5))]	# round to 5-yr ages (17 groups, no '1-4')
# acs.pop.tracts[agell<.maxage, ageul:=agell+4]			# update new age brackets
# acs.pop.tracts[agell>=.maxage, ageul:=199]				# update new age brackets
# acs.pop.tracts<-acs.pop.tracts[,.(nx=sum(estimate), 
# 								  moe=sqrt(sum(moe^2))),
# 							   by=.(GEOID,year,sex,agell,ageul)]				 # collapse

# 	2.2.7	collapse ACS tract data into MSSAs
#!! 		this method does not export tracts which do not match, which previously were exported to subTracts.csv
#!! 		choose whether to merge and collapse to MSSA level or merely merge MSSA labels
setkey(acs.pop.tracts,"GEOID") 									 # set GEOID as key for this file for merging
acs.pop.mssa <- acs.pop.tracts[cbd.link,nomatch=0                # merge tracts data with cbd.link	
							   ][,.(nx=sum(nx)),     
							     	by=.(comID,year,sex,agell,ageul)] 	 # calculate the sum of estimates by MSSA id variables

## 2.3	DOF data: county & state population by sex/age

## 2.3.1	county data from data.ca.gov portal; SKIP county text label and pop_total
dof.pop.county <- 	setDT(read_csv(.dofurl))                     # download 
dof.pop.county[, GEOID
			   :=sprintf("%05d000000",dof.pop.county$fips)]      # convert 4-digit FIPS into 11-character GEOID
setkey(dof.pop.county,"GEOID")									 # key for later merging
dof.pop.county[,fips:=NULL]                                      # drop and use GEOID instead
dof.pop.county[,county:=NULL]                                    # drop and use GEOID instead

## 2.3.2 	recode age to categories
dof.pop.county[age==0, ':=' (agell=0,ageul=0)]
dof.pop.county[age>=1 & age<=4, ':=' (agell=1,ageul=4)]
dof.pop.county[age>=5 & age<85, agell := 5*floor(age/5)]
dof.pop.county[age>=5 & age<85, ageul := agell+4]
dof.pop.county[age>=85, ':=' (agell=85,ageul=199)]				 # open-ended age group
dof.pop.county[,age:=NULL]                                       # drop age variable

## 2.3.3	summarize
dof.pop.county  <- 	melt.data.table(dof.pop.county, 
							variable.name="sex",
					   		id.vars=c("year","agell","ageul","GEOID"), 
					   		value="nx"
						 )                                       # convert dataset from wide to long format
dof.pop.county[,sex:=toupper(substr(sex,5,10))]
dof.pop.county<-dof.pop.county[,.(nx=sum(nx)),
							by=.(year,GEOID,sex,agell,ageul)]	 # collapse
# 2.3.4		collapse to state
dof.pop.state<-dof.pop.county[,.(nx=sum(nx)),
							by=.(year,sex,agell,ageul)]			 # collapse
dof.pop.state[,GEOID:="06000000000"]                             # new GEOID for state level dataset

## 2.4 	NCHS data
## 2.4.1 	county and state pop by age/sex via WWW or zip
#nchs.pop.county <- setDT(read_csv(.nchsurl,col_types="icciii"))
## 2.4.2	collapse to state
#nchs.pop.state<-nchs.pop.county[,.(nx=sum(nx)),
#							  by=.(year,sex,agell,ageul)]			 # collapse
#nchs.pop.state[,GEOID:="06000000000"]                             # new GEOID for state level dataset

##	3	ANALYSIS	----------------------------------------------------------------------

## 	3.1 	ACS tract estimates controlled to DOF total county population
##  3.2  	ACS tract estimates extrapolated (carry forward last share OR last pop) to most current year
if (controlPop | doAppend[1]) {
	# acs tract pop
	tract.tmp<-copy(acs.pop.tracts)
	tract.tmp[,fips:=substr(GEOID,1,5)]
	setkey(tract.tmp,fips,year,sex,agell)
	# acs county pop
	county.tmp<-copy(acs.pop.tracts)
	county.tmp[,fips:=substr(GEOID,1,5)]
	county.tmp<-county.tmp[,.(nx=sum(nx)),
						   by=.(fips,year,sex,agell,ageul)]
	setkey(county.tmp,fips,year,sex,agell)
	# merge acs tract pop with acs county pop; calc shr
	tract.tmp[county.tmp,shr:=nx/i.nx]
	#tract.tmp[fips=="06001"&sex=="TOTAL"&year==2009&agell==0,sum(shr)] # confirm==1
	# dof county pop formatted like acs tract pop
	county.tmp<-copy(dof.pop.county)
	county.tmp[agell==1,agell:=0]
	county.tmp[ageul==0,ageul:=4]
	county.tmp[,fips:=substr(GEOID,1,5)]
	county.tmp<-county.tmp[,.(nx=sum(nx)),
							by=.(year,fips,sex,agell,ageul)] # collapse first 2 age groups
	setkey(county.tmp,fips,year,sex,agell)
	# extend ACS tract estimates share of county pop to specified year
	if (doAppend[1]) {
		lastYear<-max(unique(tract.tmp$year))
		targetYear<-as.numeric(doAppend[2])
		if (lastYear<targetYear) {
			tmp<-tract.tmp[year==lastYear] # last year in dataset
			numCopies<-targetYear-lastYear # n years need to add
			for (i in 1:numCopies) {
				tract.tmp<-rbind(tract.tmp,tmp[,year:=year+1]) # append tmp to end and increment year
			}
			rm(numCopies)
		}
		if (!controlPop) { # if not controlling to DOF total, then carryforward last ACS pop
			# update tracts
			acs.pop.tracts<-rbind(acs.pop.tracts,
								  tract.tmp[year > max(acs.pop.tracts$year),
								  		  c("GEOID","year","sex","agell","ageul","nx")])
			# update MSSA
			acs.pop.mssa <- acs.pop.tracts[cbd.link,nomatch=0                # merge tracts data with cbd.link	
										   ][,.(nx=sum(nx)),     
										     by=.(comID,year,sex,agell,ageul)] 	 # calculate the sum of estimates by MSSA id variables
		}
		rm(lastYear,targetYear) # drop
	}
	# control DOF and copy adjusted numbers into final data
	if (controlPop) {
		setkey(tract.tmp,fips,year,sex,agell)
		# merge acs tract shr with dof county pop; calc pop; (note that this method is not exact).
		tract.tmp[county.tmp,total:=round(shr*i.nx)] # merge county total pop into tract dataset
		#tract.tmp[fips=="06001"&year==2017&sex=="TOTAL",sum(nx)] # old sum of age 0
		#tract.tmp[fips=="06001"&year==2017&sex=="TOTAL",sum(total)] # new sum of age 0
		#county.tmp[fips=="06001"&year==2017&sex=="TOTAL",sum(nx)] # county "correct" total age 0
		# update tracts
		if (doAppend[1]) {
			acs.pop.tracts<-rbind(acs.pop.tracts,
							  tract.tmp[year > max(acs.pop.tracts$year),
							  		  c("GEOID","year","sex","agell","ageul","nx")])
		}
		setkey(tract.tmp,GEOID,year,sex,agell)
		setkey(acs.pop.tracts,GEOID,year,sex,agell)
		acs.pop.tracts[tract.tmp,adjnx:=i.total] # merge adjusted totals
		acs.pop.tracts[,nx:=adjnx] # replace
		acs.pop.tracts[,adjnx:=NULL] # drop
		# update MSSA
		acs.pop.mssa <- acs.pop.tracts[cbd.link,nomatch=0                # merge tracts data with cbd.link	
									   ][,.(nx=sum(nx)),     
									     by=.(comID,year,sex,agell,ageul)] 	 # calculate the sum of estimates by MSSA id variables
	}
	rm(tract.tmp,county.tmp)
}

##	4	CLEANUP		----------------------------------------------------------------------

##  4.1 drop future years from county pop.
dof.pop.county<-dof.pop.county[year<=(max(as.numeric(doAppend[2],acs.pop.tracts)))]
dof.pop.state<-dof.pop.state[year<=(max(as.numeric(doAppend[2],acs.pop.tracts)))]

##	4.2	export results for CCB calculations: county, tract, MSSA level datasets
##		ACS age17: 0,5,10,15,...85+
##		DOF age18: 0,1,5,10,15,...85+
##		variables: (GEOID or comID) year sex agell ageul estimate
saveRDS(acs.pop.tracts, file=.nxtract)  		# GEOID
saveRDS(acs.pop.mssa,   file=.nxmssa)  			# comID
saveRDS(dof.pop.county, file=.nxcounty) 		# GEOID
saveRDS(dof.pop.state,  file=.nxstate)  		# GEOID


# END
