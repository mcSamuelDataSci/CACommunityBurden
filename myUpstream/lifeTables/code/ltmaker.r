# title: ltmake.r
# purpose: produce lifetables for CA burden project
# author: ethan sharygin (github:sharygin)
# notes:
# - intention is for analyst to be able to generate life tables by using default population + deaths,
#		or inputting their own population + deaths.
# - ACS 5-yr datasets populations are weighted by age/sex to sum to CB PEP estimates from middle year.
# - ACS tract population tables: from B01001 = age/sex by tract; subtables by race/ethnicity.
# - combine years to get higher exposures for better tables: 
#		geo		years (total)	years (by race)	agegroups 			by-characteristics
#		state	1				1				0,1-4,5(5)85,199	GEOID,sex,race
#		county	3				5				0,1-4,5(5)85,199	GEOID,sex,race
#		mssa	5				NA  			0(5)85,199			GEOID,sex
# - GEOID = unique geography level code, tract and higher: SSCCCTTTTTT where S=state fips, C=county fips, T=tract.
# - race schema: WNH BNH APINH H. exclude MR, AIAN, and combine A+PI.
#		before 2000, no MR data, so issues in denominators for those years.
#		issues in matching numerators/denominators. possible solution -- bridged race.
# - Census tracts changed between 2009-2010-2013. MSSA boundaries changed between 2009 and 2013.
#		as a result, had to map 2000 and 2010 tracts both into 2013 MSSA boundaries.
# - MSSA issues: combined 0-4 age group combined + no tracts coded before 2005; 2005 onward about 3% missing.
# - unknown whether CA RESIDENCE criteria are correctly reflected in the death file.
# - dropped Bernoulli trials method for LTCI
# instructions:
# - set path
# - set options
# - required input files: 
#	(1) ACS population B01001* 2009-2017 5-year files by tract from NHGIS (acs5_B01001_tracts.dta)
#	(2) DOF population 2000-09 and 2010-2018 by county from WWW (dof_ic00pc10v19.dta)
#	(3) 2009 and 2010 tracts to 2013 MSSAs by ESRI ArcGIS from OSHPD+TIGER/LINE (trt00mssa13.dta + trt10mssa13.dta)
#	(4) 2013 MSSA to county maps from OSHPD (mssa13cfips.dta)
#	(5) a deaths microdata file, for example: cbdDat0SAMP.R, cbdDat0FULL.R, or dof_deaths_mi.dta
# TBD:
# - convert packaged inputs from stata to csv
# - repackage default deaths/population into a standard text format: e.g., HMD format. 
#		(easier for analysts to substitute their own deaths/population data)
#		(use the readHMDHFD package to sideload txt files?)
# - calculate nqx from better data + package an included set of ax values.

## 1    SETUP		----------------------------------------------------------------------

## 1.1  packages
.pkg	<- c("data.table","readr","readstata13","stringr","tidyr") 
.inst   <- .pkg %in% installed.packages() 
if(length(.pkg[!.inst]) > 0) install.packages(.pkg[!.inst]) 
lapply(.pkg, require, character.only=TRUE)           

## 1.2  options
controlPop<-TRUE # whether to control ACS to DOF pop totals
whichDeaths<-"dof" # source of deaths data (real,fake,dof)
whichPop<-"pep" # source of population data (dof,pep)
critNx<-10000
critDx<-700

## 1.3 	paths 
#setwd("C:/Users/fieshary/projects/CACommunityBurden")
myDrive <- getwd()
myPlace <- paste0(myDrive,"/myCBD") 
upPlace <- paste0(myDrive,"/myUpstream") 
mySecure <- "H:/0.Secure.Data/myData"
dofSecure <- "d:/users/fieshary/projects/vry-lt/dx"

## 1.4 	links
#.ckey	    <- read_file(paste0(upPlace,"/upstreamInfo/census.api.key.txt")) # census API key
.nxacs		<- ifelse(controlPop,
					paste0(upPlace,"/lifeTables/dataIn/acs5_mssa_adj.dta"), # ACS tract pop, collapsed to MSSA and controlled to DOF county 
					paste0(upPlace,"/lifeTables/dataIn/acs5_mssa.dta") # ACS tract pop collapsed to MSSA 
				) 
.trt00mssa	<- paste0(upPlace,"/lifeTables/dataIn/trt00mssa13.dta") # 2009 TIGER/LINE census tracts to 2013 MSSAs
.trt10mssa	<- paste0(upPlace,"/lifeTables/dataIn/trt10mssa13.dta") # 2010 TIGER/LINE census tracts to 2013 MSSAs
.mssacfips	<- paste0(upPlace,"/lifeTables/dataIn/mssa13cfips.dta") # 2013 MSSA to county
.countycfips <- paste0(upPlace,"/lifeTables/dataIn/countycfips.dta") # county name to county FIPS in GEOID format
if (whichDeaths=="fake") .deaths		<- paste0(upPlace,"/upData/cbdDat0SAMP.R") 
if (whichDeaths=="real") .deaths		<- paste0(mySecure,"/cbdDat0FULL.R" 
if (whichDeaths=="dof")  .deaths 		<- paste0(dofSecure,"/dof_deaths_mi.dta" 
if (whichPop=="dof") .pop 				<- paste0(upPlace,"/lifeTables/dataIn/dof_ic10pc19.dta")  
if (whichPop=="pep") .pop				<- paste0(upPlace,"/lifeTables/dataIn/pep_ic10pc18_special.dta") 

## 2	GEOGRAPHY	----------------------------------------------------------------------

## 2.1	load tract to MSSA maps
trt00mssa<-setDT(read.dta13(.trt00mssa))
trt10mssa<-setDT(read.dta13(.trt10mssa))
mssacfips<-setDT(read.dta13(.mssacfips))

## 2.2 	load county name to county FIPS code maps
countycfips<-setDT(read.dta13(.countycfips))

## 3	POPULATION	----------------------------------------------------------------------

## 3.1 	load 2000-09 intercensal + 2010-18 postcensal county + state population
nx.county<-setDT(read.dta13(.pop))
nx.county<-rbind(nx.county, # sex+race detail
			 	 nx.county[,.(Nx=sum(Nx)),by=.(year,GEOID,agell,ageul)], # sex=TOTAL, race=TOTAL
			 	 nx.county[,.(Nx=sum(Nx)),by=.(year,GEOID,sex,agell,ageul)], # race7=TOTAL
			 	 nx.county[,.(Nx=sum(Nx)),by=.(year,GEOID,race7,agell,ageul)], # sex=TOTAL
			 	 use.names=TRUE,fill=TRUE,idcol=TRUE) 
nx.county[.id==2, ':=' (sex="TOTAL",race7="TOTAL")]
nx.county[.id==3,race7:="TOTAL"]
nx.county[.id==4,sex:="TOTAL"]
nx.county[,.id:=NULL]
nx.county<-nx.county[GEOID!="06000000000"]
## state
nx.state<-copy(nx.county[,.(Nx=sum(Nx)),by=.(year,sex,race7,agell,ageul)])
nx.state[,GEOID:="06000000000"]

## 3.3 	load ACS 2005-2015 five-year samples from NHGIS, rolled up to MSSA level
nxacs<-setDT(read.dta13(.nxacs))
nxacs[,race7:="TOTAL"]
nxacs<-rbind(nxacs,
			 nxacs[,.(Nx=sum(Nx)),by=.(year,comID,race7,agell,ageul)], # sex=TOTAL
			 use.names=TRUE,fill=TRUE,idcol=TRUE)
nxacs[.id==2,sex:="TOTAL"]
nxacs[,.id:=NULL]

## 4	DEATHS ---------------------------------------------------------------------------

## 4.1	load selected deaths master file
if (whichDeaths=="dof") setDT(dofdeaths<-read.dta13(.deaths))
if (whichDeaths=="fake") { load(.deaths); setDT(cbdDat0SAMP); cbddeaths<-cbdDat0SAMP }
if (whichDeaths=="real") { load(.deaths); setDT(cbdDat0FULL); cbddeaths<-cbdDat0FULL }

## 4.2 	clean CBD deaths files
if (whichDeaths %in% c("real","fake")) {
	## MSSA
	dx.mssa<-copy(cbddeaths[sex %in% c("M","F") & 
						!is.na(age) & !is.na(year) & 
						as.numeric(substring(GEOID,1,5)) %in% 6001:6115]) # keep conditions 
	dx.mssa[,agell:=(5*floor(age/5))]
	dx.mssa[agell>85,agell:=85]
	dx.mssa[age<85,ageul:=agell+4]
	dx.mssa[age>=85,ageul:=199]
	dx.mssa[sex=="F",sex:="FEMALE"]
	dx.mssa[sex=="M",sex:="MALE"]
	dx.mssa<-merge(dx.mssa,trt10mssa,on=GEOID,all.x=TRUE) # merge tract->mssa; ONLY 2010 tracts are geocoded.
	dx.mssa<-rbind(dx.mssa[,.(Dx=.N),by=.(year,comID,sex,agell,ageul)], # sex detail
				   dx.mssa[,.(Dx=.N),by=.(year,comID,agell,ageul)], # sex=TOTAL
			 	   use.names=TRUE,fill=TRUE,idcol=TRUE) 
	dx.mssa[.id==2,sex:="TOTAL"]
	dx.mssa[,.id:=NULL]
	dx.mssa[,race7:="TOTAL"]
	## county
	dx.county<-copy(cbddeaths[sex %in% c("M","F") & 
						!is.na(age) & !is.na(year) & 
						!is.na(county)]) # keep conditions 
	dx.county[age==0,agell:=0]
	dx.county[age %in% 1:4,agell:=1]
	dx.county[age>=5,agell:=(5*floor(age/5))]
	dx.county[agell>85,agell:=85]
	dx.county[agell==0,ageul:=0]
	dx.county[agell==1,ageul:=4]
	dx.county[agell %in% 5:80,ageul:=agell+4]
	dx.county[age>=85,ageul:=199]
	dx.county[sex=="F",sex:="FEMALE"]
	dx.county[sex=="M",sex:="MALE"]
	dx.county[raceCode=="AIAN-NH",race7:="AIAN_NH"]
	dx.county[raceCode=="Asian-NH",race7:="ASIAN_NH"]
	dx.county[raceCode=="Black-NH",race7:="BLACK_NH"]
	dx.county[raceCode=="Hisp",race7:="HISPANIC"]
	dx.county[raceCode=="Multi-NH",race7:="MR_NH"]
	dx.county[raceCode=="NHPI-NH",race7:="NHPI_NH"]
	dx.county[raceCode=="White-NH",race7:="WHITE_NH"]
	dx.county[raceCode=="Other-NH",race7:="SOR_NH"]
	dx.county<-merge(dx.county,countycfips,on=county,all.x=TRUE) # merge cname->GEOID
	dx.county[,GEOID:=sprintf("%05d000000",cfips)]
	dx.county<-rbind(dx.county[,.(Dx=.N),by=.(year,GEOID,sex,race7,agell,ageul)], # sex+race detail
					 dx.county[,.(Dx=.N),by=.(year,GEOID,agell,ageul)], # sex=TOTAL, race=TOTAL
					 dx.county[,.(Dx=.N),by=.(year,GEOID,sex,agell,ageul)], # race7=TOTAL
					 dx.county[,.(Dx=.N),by=.(year,GEOID,race7,agell,ageul)], # sex=TOTAL
					 use.names=TRUE,fill=TRUE,idcol=TRUE) 
	dx.county[.id==2, ':=' (sex="TOTAL",race7="TOTAL")]
	dx.county[.id==3,race7:="TOTAL"]
	dx.county[.id==4,sex:="TOTAL"]
	dx.county[,.id:=NULL]
	## state
	dx.state<-copy(dx.county[,.(Dx=sum(Dx)),by=.(year,sex,race7,agell,ageul)])
	dx.state[,GEOID:="06000000000"]
}

## 4.3	clean DOF deaths files
if (whichDeaths == "dof") {
	dx.county<-copy(dofdeaths)
	dx.county<-rbind(dx.county, # sex+race detail
					 dx.county[,.(Dx=sum(Dx)),by=.(year,GEOID,agell,ageul)], # sex=TOTAL, race=TOTAL
					 dx.county[,.(Dx=sum(Dx)),by=.(year,GEOID,sex,agell,ageul)], # race7=TOTAL
					 dx.county[,.(Dx=sum(Dx)),by=.(year,GEOID,race7,agell,ageul)], # sex=TOTAL
					 use.names=TRUE,fill=TRUE,idcol=TRUE) 
	dx.county[.id==2, ':=' (sex="TOTAL",race7="TOTAL")]
	dx.county[.id==3,race7:="TOTAL"]
	dx.county[.id==4,sex:="TOTAL"]
	dx.county[,.id:=NULL]
	## state
	dx.state<-copy(dx.county[,.(Dx=sum(Dx)),by=.(year,sex,race7,agell,ageul)])
	dx.state[,GEOID:="06000000000"]
}

## 5	MORTALITY ----------------------------------------------------------------------

## 5.1	function to generate an extract of years by geo and merge pop + deaths
##		syntax: dx=deaths data, nx=pop data, nyrs=N neighborings years to combine, y=target year, level=geography
doExtract <- function(dx=NULL, nx=NULL, nyrs=NA, y=NA, level=NA) {
	if (level=="mssa") { 
		dx[,GEOID:=comID] 
		nx[,GEOID:=comID]
	}
	if (length(unique(nx[year>=y-nyrs & year<=y+nyrs,year]))<(2*nyrs+1)) { stop("Exposure data are missing for one or more years") }
	if (length(unique(dx[year>=y-nyrs & year<=y+nyrs,year]))<(2*nyrs+1)) { stop("Incidence data are missing for one or more years") }
	tmp<-merge(nx[year>=y-nyrs & year<=y+nyrs],dx[year>=y-nyrs & year<=y+nyrs],
				    on=c('GEOID','sex','year','agell','ageul','race7'),
		  			all.x=TRUE,all.y=TRUE) # merge pop+deaths (filtered years)
	tmp<-tmp[,.(Nx=sum(Nx),Dx=sum(Dx)),by=c('GEOID','sex','agell','ageul','race7')] # collapse
	tmp<-setDT(complete(tmp,GEOID,sex,race7,agell)) # (tidyr) rectangularize 
	tmp[is.na(Dx),Dx:=0] # convert implicit to explicit zero.
	tmp[,year:=y] # recode year
	if (level=="mssa") {
		tmp[,comID:=GEOID]
		tmp[,GEOID:=NULL]
		dx[,GEOID:=NULL]
		nx[,GEOID:=NULL]
	}
	return(tmp)
}

## 5.2	call doExtract for various geographies
## GEO by:	sex/age		race
## state	1 year 		1yr
## county	3 yr		5yr
## mssa		5 yr		-

## mssa
if (whichDeaths %in% c("real","fake")) { 
	range<-2009:2014 # or later if available. 'fake' has nx 2009-2018 and dx 2007-2014
	mx.mssa<-data.table(do.call(rbind,lapply(range,doExtract,dx=dx.mssa,nx=nxacs,nyrs=2,level="mssa")))
}
## county
mx.county<-rbind( # combine 3-year TOTAL race, 5-year race7
				data.table(do.call(rbind,lapply(2001:2017,doExtract,dx=dx.county,nx=nx.county,nyrs=1,level="county")))[race7=="TOTAL"],
				data.table(do.call(rbind,lapply(2002:2016,doExtract,dx=dx.county,nx=nx.county,nyrs=2,level="county")))[race7!="TOTAL"]
			)
## state
mx.state<-data.table(do.call(rbind,lapply(2000:2018,doExtract,dx=dx.state,nx=nx.state,nyrs=0,level="state")))

## 6	LIFE TABLES ----------------------------------------------------------------------

## 6.1	generic function to produce a life table 
## 		x is a vector of age groups, nx is the corresponding vector of pop, dx of deaths
##		sex is M or MALE or F or FEMALE (used to calc ax); ax is an optional vector of ax values
##		previously estimated ax values are avaliable from the UN WPP, USMDB, NCHS, including by race. 
##		values used here are from USMDB CA 1x10 or 5x10 (2010-17) by sex.
##		also exports LTCI from Chiang's method with adjusted final age group
## - D. Eayres and E.S. Williams. 2004. "Evaluation of methodologies for small area life 
##   expectancy estimation". J Epi Com Health 58(3). http://dx.doi.org/10.1136/jech.2003.009654.
doLTChiangCI <- function(x, Nx, Dx, sex, ax=NULL, level=0.95) {
	m <- length(x)							# get position of final age group by length of age vector
	mx  <- Dx/Nx							# mortality rate
	n <- c(diff(x), NA)						# n years between age groups
	# calculate ax
	if(is.null(ax)) {						# if no ax values provided, use hardcoded CA 2010-17 by sex.
		ax <- rep(0,m)
		ax <- n/2						# rule of thumb: 1/2 age interval
		# infant ages  					# from USMDB CA 5x10 life tables.
		if (n[1]==1) ax[1]<-0.06 
		if (n[2]==4) ax[2]<-1.64
		if (n[1]==5) ax[1]<-0.44 
		# final age interval
		ax[m] <- 1 / mx[m]				# rule of thumb: inverse of mx in final age interval
		if (is.na(ax[m])) {				# if cannot calculate, e.g. because mx==0
			if(grepl("F",sex[1])) {	 						# female
					if (x[m]==85) ax[m]<-7.58
					if (x[m]==90) ax[m]<-5.22
					if (x[m]==100) ax[m]<-2.47
			}
			if(!grepl("F",sex[1]) & !grepl("T",sex[1])) {	# male
					if (x[m]==85) ax[m]<-6.54
					if (x[m]==90) ax[m]<-4.50
					if (x[m]==100) ax[m]<-2.22
			}
			if(grepl("T",sex[1])) {							# total
					if (x[m]==85) ax[m]<-7.19
					if (x[m]==90) ax[m]<-4.97
					if (x[m]==100) ax[m]<-2.42
			}
		}
	}
	# Chiang standard elements
	qx  <- n*mx / (1+(n-ax)*mx)				# probablity of death (from mortality rate)
	qx[m] <- 1								# 100% at oldest age group
	px  <- 1-qx								# pr(survival)
	lx  <- cumprod(c(1,px))*100000			# 100,000 for radix
	dx  <- -diff(lx)						# deaths each age interval
	Lx  <- n*lx[-1] + ax*dx 				# PY lived in this age group
	lx 	<- lx[-(m+1)] 						# survivors
	Lx[m] <- lx[m]/mx[m] 					# PY lived in final age group
	Lx[is.na(Lx)|is.infinite(Lx)] <- 0		# in case of NA or Inf values from poorly formed LTs
	Tx  <- rev(cumsum(rev(Lx)))				# cumulative PY lived at this age and above
	ex  <- Tx/lx 							# life expectancy at this age
	# Chiang CI elements
	zcrit=1-((1-level)/2) 									# CI from normal distribution
	sp2<-((qx^2)*(1-qx))/Dx									# variance of survival probability
	sp2[is.na(sp2)]<-0 										# fix zero deaths case
	sp2[m]<-4/Dx[m]/mx[m]^2									# adjustment final age interval
	wsp2<-lx^2*((1-(ax/n))*n+c(tail(ex,-1),NA))^2*sp2		# weighted SP2 
	wsp2[m]<-(lx[m]/2)^2*sp2[m]								# adjustment final age interval
	Twsp2<-rev(cumsum(rev(wsp2)))							# sum of weighted sp2 rows below (like Tx)
	se2<-Twsp2/lx^2 										# sample variance of e0
	exlow<-ex-qnorm(zcrit)*sqrt(se2)						# CI low
	exhigh<-ex+qnorm(zcrit)*sqrt(se2)						# CI high
	# return
	return(data.table(x, n, Nx, Dx, mx, ax, qx, px, lx, dx, Lx, Tx, ex, sp2, wsp2, Twsp2, se2, exlow, exhigh))
}

## 6.2	add index to mx tables
mx.state[, i:=.GRP, by=c("GEOID","sex","race7","year")]	
setkeyv(mx.state,c("i","agell"))
mx.county[, i:=.GRP, by=c("GEOID","sex","race7","year")]	
setkeyv(mx.county,c("i","agell"))
if (whichDeaths %in% c("real","fake")) {
	mx.mssa[, i:=.GRP, by=c("comID","sex","race7","year")]	
	setkeyv(mx.mssa,c("i","agell"))
}

## 6.3	restrict using sum(Nx) & sum(Dx)
mx.state<-mx.state[, ':=' (sumNx=sum(Nx),sumDx=sum(Dx)), by=.(i)][sumNx>=critNx & sumDx>=critDx]
mx.county<-mx.county[, ':=' (sumNx=sum(Nx),sumDx=sum(Dx)), by=.(i)][sumNx>=critNx & sumDx>=critDx]
if (whichDeaths %in% c("real","fake")) { 
	mx.mssa<-mx.mssa[, ':=' (sumNx=sum(Nx),sumDx=sum(Dx)), by=.(i)][sumNx>=critNx & sumDx>=critDx]
}

## 6.4	Call LT routine by geography
## state
system.time({	lt.state<-mx.state[, doLTChiangCI(x=agell,Nx=Nx,Dx=Dx,sex=sex), by=c("i","GEOID","sex","race7","year")] }) 
setkeyv(lt.state,c("i","x"))
## county
system.time({	lt.county<-mx.county[, doLTChiangCI(x=agell,Nx=Nx,Dx=Dx,sex=sex), by=c("i","GEOID","sex","race7","year")] }) 
setkeyv(lt.county,c("i","x"))
## MSSA
if (whichDeaths %in% c("real","fake")) { 
	system.time({ lt.mssa<-mx.mssa[, doLTChiangCI(x=agell,Nx=Nx,Dx=Dx,sex=sex), by=c("i","comID","sex","race7","year")] }) 
	setkeyv(lt.mssa,c("i","x"))
}

## 7	REVIEW/EXPORT ----------------------------------------------------------------------

## 7.1	EXPORT
## full LT
saveRDS(lt.state,paste0(upPlace,"/lifeTables/dataOut/LTciState.rds"))
saveRDS(lt.county,paste0(upPlace,"/lifeTables/dataOut/LTciCounty.rds"))
if (whichDeaths %in% c("real","fake")) { 
	saveRDS(lt.mssa,paste0(upPlace,"/lifeTables/dataOut/LTciMSSA.rds"))
}
## e0 only
saveRDS(lt.state[x==0,c("GEOID","sex","race7","year","ex","exlow","exhigh")],
		paste0(upPlace,"/lifeTables/dataOut/e0ciState.rds"))
saveRDS(lt.county[x==0,c("GEOID","sex","race7","year","ex","exlow","exhigh")],
		paste0(upPlace,"/lifeTables/dataOut/e0ciCounty.rds"))
if (whichDeaths %in% c("real","fake")) { 
	saveRDS(lt.mssa[x==0,c("comID","sex","race7","year","ex","exlow","exhigh")]
			,paste0(upPlace,"/lifeTables/dataOut/e0ciMSSA.rds"))
}

## 7.2	Review
mx.state[sex=="TOTAL" & race7=="TOTAL",.(Nx=sum(Nx),Dx=sum(Dx)),by=c("GEOID","sex","year","race7")] # state sum
lt.state[x==0 & sex=="TOTAL" & race7=="TOTAL",c("GEOID","sex","year","race7","ex","exlow","exhigh")] 
mx.county[sex=="TOTAL" & race7=="TOTAL",.(Nx=sum(Nx),Dx=sum(Dx)),by=c("sex","year","race7")] # state sum
lt.county[x==0 & sex=="TOTAL" & race7=="TOTAL" & year==2017,
			c("GEOID","sex","year","race7","ex","exlow","exhigh")] 
mx.mssa[sex=="TOTAL" & race7=="TOTAL",.(Nx=sum(Nx),Dx=sum(Dx)),by=c("sex","year","race7")] # state sum
lt.mssa[x==0 & sex=="TOTAL" & race7=="TOTAL" & (year %in% c(2010,2017)),
			c("comID","sex","year","race7","ex","exlow","exhigh")] 

## 7.3	NOTES	----------------------------------------------------------

# Life tables for communities, counties and states are generated from age specific 
# mortality rates, which are the quotient of deaths during a calendar year to the 
# and exposure, approximated by the population of the same age at the midpoint of 
# the year (July 1). Age structured population data for tracts and communities are
# estimated using data from the American Community Survey, 5-year sample (table 
# B01001; multiple years). County and state age population by age are estimated by 
# the Demographic Research Unit, CA Department of Finance. Deaths data are based 
# on 100% extracts from the vital statistics reporting system, CA Department of 
# Public Health. Mortality and exposure data were combined for small groups:
# 5 years of combined population and mortality data for each annual community table, 
# as well as to county tables by race. 3 years of combined data for county tables
# without race detail. Life tables with fewer than 700 deaths of 10,000 PY were 
# censored. Intra-age mortality (nax) was calculated for ages below 5 using values
# from a similar population (CA life table for 2010-17 from USMDB) and by the 
# midpoint of the age interval for other age groups except the last (1/mx or a 
# value from USMDB if mx is zero or undefined). Standard errors were calculated 
# for age specific probabilities of death and used to calculate 95% confidence
# intervals for life expectancy (Chiang 1984; Eayres and Williams 2004).
#
# United States Mortality DataBase. University of California, Berkeley (USA). 
#			Available at usa.mortality.org. Downloaded 2020-02-27.
#
# Chiang, C.L. 1984. The Life Table and its Applications. Robert E Krieger Publ Co., pp. 153-168.
#
# Eayres D, and E.S.E. Williams. Evaluation of methodologies for small area life expectancy estimation.
# 	Journal of Epidemiology & Community Health 2004;58:243-249.
