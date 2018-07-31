# title: lifetable.make-ccb.r
# purpose: life tables for mssa/county/state
# notes:
# history: 
#  v02 2018.7.19 rewritten to use most recent population data and count back from there.
#				 this involves adding datum of population data; adding multiple years' pop data
#				 from 5yr ACS. rather than having to average deaths and take population at 
#				 midpoint, can sum deaths and PY during the interval, e.g.:
#					deaths to 10-14 year olds 2011-2015: XX
#					N of 10-14 year olds in 2011+2012+2013+2014+2015: YY. ASDR = XX/YY.
#				 the popular alternative is (1) modeling or (2) combining geographies
#  v01 2018.5.25 forked from lifetable.make.r to use CCB datasets
#				 approach taken is to fix denominator at 2010 Census, and average deaths
#				 around the central datum of 4/1/2010 until sufficient PY met, e.g.
#					deaths to 10-14 year olds during 2008+2009+...+2012: XX. 
#					N years elapsed between first death and last death included: NN.
#					N of 10-14 year olds in 2010 = YY. ASDR = (XX/NN)/YY.

## 1    SETUP		----------------------------------------------------------------------

## 1.1  packages
.pkg	<- c("data.table","tidyr") 
.inst   <- .pkg %in% installed.packages() 
if(length(.pkg[!.inst]) > 0) install.packages(.pkg[!.inst]) 
lapply(.pkg, library, character.only=TRUE)           

## 1.2  path and globals
.path   <- "d:/Users/fieshary/projects/mortality_ccb_lt/ccb_lifetables"		# !! update path to working dir
setwd(.path)

## 2	DATASETS	----------------------------------------------------------------------

## 2.1 	deaths (death.make.r)
## 		comID year sex (char) age17 dx 
dx.mssa	<-readRDS("dxMSSA-ccb.rds") 			# VR

## 2.2	population (pop.make.r)
## 		comID year sex (char) age23 estimate
#nx.mssa	<-readRDS("LTnxMSSA-ccb.rds") 		# DEC (datum 4/1/2010)
nx.mssa	<- readRDS("popMSSA.rds") 				# ACS 2010-15 (datum 7/1/2015)
setnames(nx.mssa,"estimate","nx")				# rename ACS variable for consistency
.datum <- max(nx.mssa$year) 					# most recent date in dataset

## 3	ANALYSIS (LIFE TABLE)	----------------------------------------------------------

## 3.1	how many years of data required to get a stable LT 
##		this code is simplified for ACS. for DEC, have to account for April 1 date instead of July 1.
.threshold <- 15000														# set the minimum N of PY exposure
.window <- nx.mssa[,.(nx=sum(nx)),by=.(comID,year,sex)][				# sum nx by MSSA/sex
						nx<.threshold,.(comID,sex, 						# filter out those with <minimum PY, 
							flag=ceiling((.threshold/nx)/2))][			# calculate N years needed, rounded, by sex
							,.(flag=max(flag)),by=comID][flag>1]		# report if need more than 1 year 
.window[, startyr:=pmax(.datum-flag,2009)]								# 5-yr ACS series starts in 2009, so nothing before (affects 4plusAlpine)
.window

## 3.2	update all counties with start years needed for denominators for MSSA level life tables
##		this code is simplified for ACS. for DEC, have to account for April 1 date instead of July 1.
.comID <- data.table(unique(dx.mssa$comID), key='V1')			# list of unique MSSA ids
names(.comID)[names(.comID) == "V1"] = "comID"                  # rename variable to fips (to merge with labels)
.window <- .window[.comID, on = "comID"]						# add counties that will not need additional data
.window[is.na(flag), flag := 1]									# default N years (1 year)
.window[is.na(startyr) , startyr := .datum]						# default start date (same CY as ACS)
.window[, startdate:=sprintf("%d-%02d", startyr, 1)]
.window[, endyr := .datum]										# default end date (most recent ACS)

## 3.3	extract N deaths during the specifed window for one area
doDx <- function(mssa) {
	dx.mssa[comID==mssa & 
		    date>=.window[comID==mssa,startdate]]
}

## 3.4	extract PY exposure during the specifed window for one area
doNx <- function(mssa) {
	nx.mssa[comID==mssa & 
			year>=.window[comID==mssa,startyr]]
}

## 3.5.1 calculate ASDR, part 1: summarize deaths 
lifetable.Dx<-rbindlist(lapply(.comID[,comID],doDx))			# loop to read MSSAs (test with id[1:3,id])
lifetable.Dx<-lifetable.Dx[,.(dx=sum(dx)),
						by=c("comID","sex","agell")]			# summarize by age group (drop year detail)
setkeyv(lifetable.Dx,c("comID","sex","agell"))					# sort

## 3.5.2 calculate ASDR, part 2: summarize exposure
lifetable.Nx<-rbindlist(lapply(.comID[,comID],doNx))			# loop to get pop for all MSSAs (test with id[1:3,id])
lifetable.Nx<-lifetable.Nx[,.(nx=sum(nx)),
						by=c("comID","sex","agell")]			# summarize by age group (drop year detail)
setkeyv(lifetable.Nx,c("comID","sex","agell"))					# sort

## 4.1	harmonize death, pop data and merge
mx.mssa <-merge(lifetable.Nx, lifetable.Dx, 
				by=c("comID","sex","agell"), all=TRUE)			# merge pop, death data

## 4.2 	rectangularize and collapse by new age groups
mx.mssa<-setDT(complete(mx.mssa,comID,sex,agell))		# (tidyr) rectangularize and key as DT
.maxage=85
mx.mssa[agell<.maxage, ageul:=agell+4]	
mx.mssa[agell>=.maxage, ':=' (agell=.maxage, ageul=199)]	
mx.mssa[is.na(nx), nx:=0]
mx.mssa[is.na(dx), dx:=0]
mx.mssa[, i:=.GRP, by=c("comID","sex")] 				# create an ID variable for each LT
setkeyv(mx.mssa,c("i","agell"))	

## 4.1	generic function to produce a life table from minimum inputs
## 		x is a vector of age groups, nx is the corresponding vector of pop, dx of deaths
##		sex is M or MALE or F or FEMALE (used to calc ax); ax is an optional vector of ax values
doLT <- function(x, Nx, Dx, sex, ax=NULL) {
	m <- length(x)							# get final age group by length of age vector
  	mx  <- Dx/Nx 							# mortality rate
	n <- c(diff(x), NA)						# n years between age groups
	if(is.null(ax)){
		ax <- rep(0,m)
    	if(x[1]!=0 | x[2]!=1){
      		ax <- n/2						# rule of thumb: 1/2 age interval
      		ax[m] <- 1 / mx[m]				# rule of thumb: inverse of mx in final age interval
    	}
    	else{    
      		if(grepl("F",sex[1])){ 			# ax values for women from Coale (Preston et al 2001)
        		if(mx[1]>=0.107) {
          			ax[1] <- 0.350
        		}
        		else{
          			ax[1] <- 0.053 + 2.800*mx[1]
        		}
      		}
      		if(!grepl("F",sex[1])){ 		# ax values for men
        		if(mx[1]>=0.107) {
          			ax[1] <- 0.330
        		}
        		else{
          			ax[1] <- 0.045 + 2.684*mx[1]
        		}
      		}
      		ax[-1] <- n[-1]/2
      		ax[m] <- 1 / mx[m]
    	}
	}
  	qx  <- n*mx / (1 + (n - ax) * mx)		# probablity of death (from mortality rate)
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
	return(data.table(x, n, Nx, Dx, mx, ax, qx, px, lx, dx, Lx, Tx, ex))
}

# 4.2	call to LT function
lt.mssa<-data.table()										# init empty dt
#for (j in 1:2) {											# loop over each LT in dataset, sending params as vectors
for (j in 1:mx.mssa[agell==0,.N]) {						
	x<-mx.mssa[i==j,agell]
	nx<-mx.mssa[i==j,nx]
	dx<-mx.mssa[i==j,dx]
	sex<-mx.mssa[i==j,sex]
	i<-mx.mssa[i==j,i]
	lt.mssa <- rbindlist(list(lt.mssa,                      # fast rbind result to lt.mssa
					cbind(doLT(x,nx,dx,sex),j)))			# attach ID to life table
}
names(lt.mssa)[names(lt.mssa) == "j"] = "i"					# rename j column to i (ID of mx file)
setkeyv(lt.mssa,c("i","x"))

# 4.3 	function to produce a life table from qx values only (used in simulation for CI)
doQxLT<- function(x, qx, sex, ax=NULL, last.ax=5.5) {
	m <- length(x)
	n <- c(diff(x), NA)
	qx[is.na(qx)] <- 0
	if(is.null(ax)){
		ax <- rep(0,m)
		if(x[1]!=0 | x[2]!=1){
			ax <- n/2
			ax[m] <- last.ax
		}
		else{    
			if(grepl("F",sex[1])){
				if(qx[1]>=0.1){
					ax[1] <- 0.350
				}
				else{
					ax[1] <- 0.05 + 3*qx[1]
				}
			}
			if(!grepl("F",sex[1])){
				if(qx[1]>=0.1){
					ax[1] <- 0.33
				}
				else{
					ax[1] <- 0.0425 + 2.875*qx[1]
				}
			}
			ax[-1] <- n[-1]/2
			ax[m] <- last.ax
		}
	}
	px  <- 1-qx
	lx  <- cumprod(c(1,px))*100000
	dx  <- -diff(lx)
	Lx  <- n*lx[-1] + ax*dx
	lx 	<- lx[-(m+1)]
	Lx[m] <- lx[m]*last.ax
	Lx[is.na(Lx)|is.infinite(Lx)] <- 0 			# fix NA or Inf values
	Tx  <- rev(cumsum(rev(Lx)))
	ex  <- Tx/lx
	return(data.table(x, n, ax, qx, px, lx, dx, Lx, Tx, ex))
}

# 4.4	function to calculate confidence interval around life expectancy
#		E. Andreev and V. Shkolnikov. 2010. "Spreadsheet for calculation of confidence limits 
#			for any life table or healthy-life table quantity". MPIDR Technical Report 2010-005.
doLTCI <- function(LT=NULL, 									# LT matrix created by doLT
					which.x=0,  								# CI of ex at which age?
					ns=1000, 									# N simulations
					level=0.95) { 								# desired CI
	setDT(LT)													# (redundant if already DT)
	m  <- LT[,.N]												# N age groups == n rows
	x  <- LT[,x] 												# ages
	qx <- LT[,qx] 												# qx
	Dx <- LT[,Dx] 												# Dx
	.trials <- round(Dx/qx) 									# trials for binomial, rounded
	.lastax <- LT$ax[m] 										# ax in open-ended age group
	Y <- suppressWarnings(matrix(rbinom(m*ns,.trials,qx),       # simulated death counts (binomial)
	                             		m,ns))
	QX <- Y/.trials												# simulated qx
	wh <- which(x==which.x) 									# row number which contains age group of interest
	fun.ex <- function(qx) {									# subroutine to compute ex for simulated qx
	  return(doQxLT(x=x, qx, sex=sex, last.ax=.lastax)$ex[wh]) 	# runs doQxLT and saves one value of ex
	}
	exsim <- apply(QX, 2, fun.ex) 								# call subroutine
	CI <- quantile(exsim, 										# generate CI
	               probs = c((1-level)/2,
	                         1 - (1-level)/2))
	return(list(ex=LT$ex[wh], 									# result is list of: ex= life table ex
	            meanex=mean(exsim),  	                        # meanex: mean of simulated ex
	            ciex=CI, 										# ciex: ci around ex
	            exsim=exsim, 									# every simulated ex result
	            which.x=which.x)) 							    # the age
}

# 4.4	call to CI function
ltci.mssa<-data.table() 										# initialize an empty DT
#for (j in 1:2) {
.counter<-lt.mssa[x==0,.N]/10
.pb <- txtProgressBar(min = 0, max = .counter, style = 3)		# show a text progress bar for loop
for (j in 1:lt.mssa[x==0,.N]) {						
	ltci.mssa<-rbindlist(list(ltci.mssa, 						# fast rbind result to ltci.mssa (2 items)
			  				cbind(data.table( 					# format results of exsim as DT
			  				t(unlist(doLTCI(lt.mssa[i==j],      # run specific LT
			  				which.x=0,ns=500,level=.95)[		# pass parameters for simulation
			  				c(1,2,3,5)])))						# save just desired fields from exsim
			  			  ,j)									# attach ID to simulation results; 
			  )
	)
	setTxtProgressBar(.pb,j)												
}
close(.pb)
names(ltci.mssa)[names(ltci.mssa) == "j"] = "i"					# rename j column to i (ID of mx file)
names(ltci.mssa)[names(ltci.mssa) == "which.x"] = "x"			# rename to agell
setkeyv(ltci.mssa,c("i","x"))

# 4.5 	diagnostic plots
#		these are of a de novo simulation, not the output dataset
#		use to check any single LT for typical results.
doExHist <- function(i) {
	while (!is.null(dev.list())) dev.off()
	par(mfrow=c(1,1))
	tmp<-doLTCI(LT=lt.mssa[i==2],0,500,.95)
	hist(tmp$exsim)
	abline(v=tmp$ex,col=2)
	abline(v=tmp$meanex,col=4)
	abline(v=tmp$ciex,col=4)
}

# 4.6 	update mortality dataset with some parameters from results
#		this involves: renaming i, x columns in ltci.mssa to merge with lt.mssa
#		theoretically could run CI for all ages in lt.mssa (n lifetables * n age groups per LT)
#		but for now, just link metadata to to both lt.mssa and ltci.mssa: (GEOID|comID), sex, year(s)
names(mx.mssa)[names(mx.mssa) == "agell"] = "x" 						# rename agell to x for consistency
lt.mssa<-lt.mssa[mx.mssa[,c("i","x","sex","comID")],nomatch=0] 			# add MSSA comID and sex by ID
ltci.mssa<-ltci.mssa[mx.mssa[x==0,c("i","x","sex","comID")],nomatch=0]	# add MSSA comID and sex by ID

# 5		export data

saveRDS(ltci.mssa, file="LTciMSSA.rds")		# comID sex (char) x (age0) ex meanex ciex.low ciex.high
