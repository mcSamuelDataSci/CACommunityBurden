# title: pop.make.r
# purpose: datasets of population denominators for CBD project (state, county, tract, mssa)
# notes:
#		county and state level use DOF data; tract and MSSA level use ACS data
#		this code doesn't control ACS to DOF totals, but production code does.
#		acs/dof are mid-year populations.
# history:
#	v10 2018.10.4: using predownloaded NHGIS ACS files instead of API. dropping DEC.
#	v09 2018.9.27: revised. using code from CCB mainstream.
#	v08 2018.7.19: added ability to combine multiple 5-yr ACS tract-level files; 
#				   collapse ACS ages to common with deaths: age17.
#				   remove code to read/use SEER/NCHS
#	v07 2018.6.12: added month/datum to population datasets.
#	v06	2018.5.10: bug fixes: decennial population datasets (county/state).
#	v05 2018.5.8: added decennial population datasets (tract/MSSA).
#	2018.5.2: fixed bug in section 2.2 when reading data to acs.varlist
#	2018.3.5: first working version (population only)

## 1    SETUP		----------------------------------------------------------------------

## 1.1  pats and globals
.path      	<- "c:/users/iisan7/desktop/mortality_ccb_lt/ccb_lifetables/"
.path       <- "e:/0.CBD/myUpstream/lifeTables/"



.cbdlink	<- "data/cbdLinkCA.csv"
.acs		<- "data/acs5_b01001_2012_2016_ca.dta"
.dof		<- "https://data.ca.gov/sites/default/files/dof_dru_pop_1970_2050_csya_wide.csv"
.nxtract	<- "data/nxTract.rds"
.nxmssa 	<- "data/nxMSSA.rds"
.nxcounty	<- "data/nxCounty.rds"
.nxstate	<- "data/nxState.rds"
setwd(.path)
#myPlace    <- path(.path,"0.CBD/myCBD")
#upPlace    <- path(.path,"0.CBD/myUpstream")

## 1.2  packages
.pkg	<- c("data.table","readr","stringr","readstata13") 
.inst   <- .pkg %in% installed.packages() 
if(length(.pkg[!.inst]) > 0) install.packages(.pkg[!.inst]) 
lapply(.pkg, require, character.only=TRUE)           

## 2	DATASETS	----------------------------------------------------------------------

## 2.1 	tract-to-MSSA crosswalk
cbd.link <- setDT( read_csv(.cbdlink),
	key="GEOID"	                                  # set key for merging later
)

## 2.2  ACS data: tract level age-sex detail from file B01001 series
##		for ease of use, these have been pre-downloaded and saved locally
##		long format; variables: (acsyear GEOID agell ageul a f m)
acs.pop.tracts <- 	setDT(
						read.dta13(.acs),
						key="GEOID"
					)
acs.pop.tracts <- 	melt.data.table(acs.pop.tracts, 
							variable.name="sex",
					   		id.vars=c("year","agell","ageul","GEOID"), 
					   		value="nx"
						 )                                       # convert dataset from wide to long format
acs.pop.tracts[,sex:=toupper(substr(sex,5,10))]
setkey(acs.pop.tracts,"GEOID")									 # melt unkeys the DT

## 2.3	DOF data: county-level population by county/sex/age
##		several	options here. (1): P-3 file (compressed CSV file with complete age/race/sex/ethnic detail)
##							  (2): P-2 file (5-year age groups by sex, xlsx file)
##							  (3): data.ca.gov portal via API (sex, age); SKIP county text label and pop_total
dof.pop.county <- 	setDT(
						read_csv(.dof,
			 				col_types="i_iiii_"
						)
			 		)
dof.pop.county[,pop_all:=pop_female+pop_male]					 # total pop by age
dof.pop.county[, GEOID
			   :=sprintf("%05d000000",dof.pop.county$fips)]      # convert 4-digit FIPS into 11-character GEOID
setkey(dof.pop.county,"GEOID")									 # key for later merging
dof.pop.county[,fips:=NULL]                                      # drop fips variable, now that GEOID has been added
# recode age to categories
dof.pop.county[,agell:=age]
dof.pop.county[agell>=2 & agell<=4,agell:=1]
dof.pop.county[agell>=5 & agell<=85,agell:=as.integer(5*floor(agell/5))]
dof.pop.county[agell>=86,agell:=85]
dof.pop.county[agell==0,ageul:=1]
dof.pop.county[agell==1,ageul:=4]
dof.pop.county[agell>1 & agell<85,ageul:=agell+4]
dof.pop.county[agell==85,ageul:=199]
dof.pop.county[,age:=NULL]                                        # drop age variable, now that GEOID has been added
dof.pop.county  <- 	melt.data.table(dof.pop.county, 
							variable.name="sex",
					   		id.vars=c("year","agell","ageul","GEOID"), 
					   		value="nx"
						 )                                       # convert dataset from wide to long format
dof.pop.county[,sex:=toupper(substr(sex,5,10))]
dof.pop.county<-dof.pop.county[,.(nx=sum(nx)),
							by=.(year,GEOID,sex,agell,ageul)]	 # collapse
dof.pop.state<-dof.pop.county[,.(nx=sum(nx)),
							by=.(year,sex,agell,ageul)]			 # collapse
dof.pop.state[,GEOID:="06000000000"]                             # new GEOID for state level dataset

##	3	ANALYSIS	----------------------------------------------------------------------

## 	3.1 	ACS tract estimates controlled to DOF total county population
##			!! TBD

# 	3.2		collapse ACS tract data into MSSAs
#!! 		this method does not export tracts which do not match, which previously were exported to subTracts.csv
#!! 		choose whether to merge and collapse to MSSA level or merely merge MSSA labels
acs.pop.mssa <- acs.pop.tracts[cbd.link,nomatch=0                # merge tracts data with cbd.link	
							   ][,.(nx=sum(nx)),     
							     by=.(comID,year,sex,agell,ageul)] 	 # calculate the sum of estimates by MSSA id variables

##	4	CLEANUP		----------------------------------------------------------------------

##	4.2	export results for CCB calculations: county, tract, MSSA level datasets
##		ACS age17: 0,5,10,15,...85+
##		DOF age18: 0,1,5,10,15,...85+
##		variables: (GEOID or comID) year sex agell ageul estimate
saveRDS(acs.pop.tracts, file=.nxtract)  		# GEOID
saveRDS(acs.pop.mssa,   file=.nxmssa)  		# comID
saveRDS(dof.pop.county, file=.nxcounty) 		# GEOID
saveRDS(dof.pop.state,  file=.nxstate)  		# GEOID

# END
