# title: lifetable.make-ccb.r
# purpose: life tables for mssa/county/state

## 1    SETUP		----------------------------------------------------------------------

## 1.1  packages
.pkg	<- c("data.table","tidyr","readr") 
.inst   <- .pkg %in% installed.packages() 
if(length(.pkg[!.inst]) > 0) install.packages(.pkg[!.inst]) 
lapply(.pkg, library, character.only=TRUE)           

## 1.2  dataset flag
whichData     <- "fake"   # "real" or "fake" death data

## 1.3  paths
myDrive <- getwd()
myPlace <- paste0(myDrive,"/myCBD") 
upPlace <- paste0(myDrive,"/myUpstream") 
LTplace <- paste0(upPlace,"/lifeTables/dataOut/")

## 1.4  links
.cbdlink	<- paste0(myPlace,"/myInfo/Tract to Community Linkage.csv") # map tract level GEOID to comID
.countylink <- paste0(myPlace,"/myInfo/County Codes to County Names Linkage.xlsx") # map county names to codes
.ckey	    <- read_file(paste0(upPlace,"/upstreamInfo/census.api.key.txt")) # census API key
.clabels    <- paste0(myPlace,"/myInfo/B01001_labels.csv") # labels for fields in B01001 table.
.dofurl		<- "https://data.ca.gov/sites/default/files/dof_dru_pop_1970_2050_csya_wide.csv"
.nxtract	<- paste0(upPlace,"/lifeTables/dataOut/nxTract.rds") # output deaths by tract
.nxmssa		<- paste0(upPlace,"/lifeTables/dataOut/nxMSSA.rds") # output deaths by mssa
.nxcounty	<- paste0(upPlace,"/lifeTables/dataOut/nxCounty.rds") # output deaths by county
.nxstate	<- paste0(upPlace,"/lifeTables/dataOut/nxState.rds") # output deaths by state
.dxtract	<- paste0(upPlace,"/lifeTables/dataOut/dxTract.rds") # output deaths by tract
.dxmssa		<- paste0(upPlace,"/lifeTables/dataOut/dxMSSA.rds") # output deaths by mssa
.dxcounty	<- paste0(upPlace,"/lifeTables/dataOut/dxCounty.rds") # output deaths by county
.dxstate	<- paste0(upPlace,"/lifeTables/dataOut/dxState.rds") # output deaths by state

## 1.5  setwd
setwd(myDrive)

## 2	DATASETS	----------------------------------------------------------------------

## 2.1  load datasets
.dxtract	<- readRDS(.dxtract)	
.dxmssa		<- readRDS(.dxmssa)	
.dxcounty	<- readRDS(.dxcounty)
.dxstate	<- readRDS(.dxstate)	
.nxtract	<- readRDS(.nxtract)	
.nxmssa 	<- readRDS(.nxmssa) 	
.nxcounty	<- readRDS(.nxcounty)
.nxstate	<- readRDS(.nxstate)	

## 2.2  temp rename comID to GEOID for consistency with functions using GEOID
.dxmssa[,GEOID:=comID] 
.nxmssa[,GEOID:=comID]

## 2.3  for fake data, reduce exposure (censor geographies, but more accurate ex)
.dxstate[year>=2015,(dx=sum(dx)),by=.(GEOID,year)] # looks like approx 45%??
if (whichData=="fake") .nxmssa[,nx:=nx*.45] # inflate deaths to compensate for sample size
if (whichData=="fake") .nxcounty[,nx:=nx*.45] # inflate deaths to compensate for sample size
if (whichData=="fake") .nxstate[,nx:=nx*.45] # inflate deaths to compensate for sample size

## 3	ANALYSIS (LIFE TABLE)	----------------------------------------------------------
## - there are 9 years of exposure (population) data, so that is a limit of ACS.
## - rules of thumb are 10,000 or 15k PY of exposure for a stable LT in a high-e0 population.
## - earlier versions return a table of start/end years needed, working back from 2017.
## - temporarily, we are using a 5 years window for all (2013-2017 5 year ACS samples).
## - ACS population estimates include uncertainty, not accounted for here.
## - for this version, no spatiotemporal smoothing.

## 3.1	function: calculate how many years of exposure data required for stable LT
doWindow <- function(d=NULL,t=10000) { 								# d=population dataset, t=critical PY exposure
	return(d[,.(nx=sum(nx)),by=.(GEOID,year,sex)][					# sum nx by geo/sex
						nx<t,.(GEOID,sex, 							# filter out those with <minimum PY, 
							flag=ceiling((t/nx)/2))][				# calculate N years needed, rounded, by sex
							,.(flag=max(flag)),by=GEOID][flag>1])	# report if need more than 1 year 
}

## 3.2  list of areas that will not be processed (to plug in to map as blanks)
.zeroTract <-doWindow(.nxtract)[flag>5] 		# tracts that require>N years data. (874/9170)
.zeroMSSA  <-doWindow(.nxmssa)[flag>5]		  # MSSAs that require>N years data. (27/542)
.zeroCounty<-doWindow(.nxcounty)[flag>5]		# counties that require>N years data. (1/58)

## 3.3  summarize deaths 
ltdx.mssa	<-.dxmssa[year>=2013 & year<=2017        # extract years overlapping ACS 
                & (comID %in% .zeroMSSA$GEOID)==FALSE][, # extract areas that meet exposure threshold
					      .(dx=sum(dx)),
					      by=c("comID","sex","agell","ageul")]		
ltdx.county	<-.dxcounty[year>=2013 & year<=2017    # extract years overlapping ACS 
                & (GEOID %in% .zeroCounty$GEOID)==FALSE][, # extract areas that meet exposure threshold
					      .(dx=sum(dx)),
					      by=c("GEOID","sex","agell","ageul")]		 
ltdx.state	<-.dxstate[year>=2013 & year<=2017][,  # extract years overlapping ACS 
					      .(dx=sum(dx)),
					      by=c("GEOID","sex","agell","ageul")]		

## 3.4	summarize exposures
ltnx.mssa	<-.nxmssa[year>=2013 & year<=2017        # extract years overlapping ACS 
                & (comID %in% .zeroMSSA$GEOID)==FALSE][, # extract areas that meet exposure threshold
               .(nx=sum(nx)),
               by=c("comID","sex","agell","ageul")]		
ltnx.county	<-.nxcounty[year>=2013 & year<=2017    # extract years overlapping ACS 
                & (GEOID %in% .zeroCounty$GEOID)==FALSE][, # extract areas that meet exposure threshold
               .(nx=sum(nx)),
               by=c("GEOID","sex","agell","ageul")]		 
ltnx.state	<-.nxstate[year>=2013 & year<=2017][,  # extract years overlapping ACS 
               .(nx=sum(nx)),
               by=c("GEOID","sex","agell","ageul")]		

## 3.5 	merge deaths and exposures
##	mssa
setkeyv(ltnx.mssa,c("comID","sex","agell","ageul"))	
setkeyv(ltdx.mssa,c("comID","sex","agell","ageul"))	
mx.mssa <-merge(ltnx.mssa, ltdx.mssa, 
				by=c("comID","sex","agell","ageul"), all=TRUE)	# merge pop, death data
##	county
setkeyv(ltnx.county,c("GEOID","sex","agell","ageul"))	
setkeyv(ltdx.county,c("GEOID","sex","agell","ageul"))	
mx.county <-merge(ltnx.county, ltdx.county, 
				by=c("GEOID","sex","agell","ageul"), all=TRUE)	# merge pop, death data
##	state
setkeyv(ltnx.state,c("GEOID","sex","agell","ageul"))	
setkeyv(ltdx.state,c("GEOID","sex","agell","ageul"))	
mx.state <-merge(ltnx.state, ltdx.state, 
				by=c("GEOID","sex","agell","ageul"), all=TRUE)	# merge pop, death data

## 3.6 	rectangularize and collapse by new age groups
##	mssa
mx.mssa<-setDT(complete(mx.mssa,comID,sex,agell))				# (tidyr) rectangularize and key as DT
mx.mssa[is.na(nx), nx:=0]
mx.mssa[is.na(dx), dx:=0]
mx.mssa[, i:=.GRP, by=c("comID","sex")] 				# create an ID variable for each LT
setkeyv(mx.mssa,c("i","agell"))	
##	county
mx.county<-setDT(complete(mx.county,GEOID,sex,agell))				# (tidyr) rectangularize and key as DT
mx.county[is.na(nx), nx:=0]
mx.county[is.na(dx), dx:=0]
mx.county[, i:=.GRP, by=c("GEOID","sex")] 				# create an ID variable for each LT
setkeyv(mx.county,c("i","agell"))	
##	state
mx.state<-setDT(complete(mx.state,GEOID,sex,agell))				# (tidyr) rectangularize and key as DT
mx.state[is.na(nx), nx:=0]
mx.state[is.na(dx), dx:=0]
mx.state[, i:=.GRP, by=c("GEOID","sex")] 				# create an ID variable for each LT
setkeyv(mx.state,c("i","agell"))	

## 3.7	generic function to produce a life table from minimum inputs
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

## 3.8 call to LT function
##	mssa
lt.mssa<-data.table()										# init empty dt
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
##	county
lt.county<-data.table()										# init empty dt
for (j in 1:mx.county[agell==0,.N]) {						
	x<-mx.county[i==j,agell]
	nx<-mx.county[i==j,nx]
	dx<-mx.county[i==j,dx]
	sex<-mx.county[i==j,sex]
	i<-mx.county[i==j,i]
	lt.county <- rbindlist(list(lt.county,                  # fast rbind result to lt.county
					cbind(doLT(x,nx,dx,sex),j)))			# attach ID to life table
}
names(lt.county)[names(lt.county) == "j"] = "i"				# rename j column to i (ID of mx file)
setkeyv(lt.county,c("i","x"))
##	state
lt.state<-data.table()										# init empty dt
for (j in 1:mx.state[agell==0,.N]) {						
	x<-mx.state[i==j,agell]
	nx<-mx.state[i==j,nx]
	dx<-mx.state[i==j,dx]
	sex<-mx.state[i==j,sex]
	i<-mx.state[i==j,i]
	lt.state <- rbindlist(list(lt.state,                      # fast rbind result to lt.state
					cbind(doLT(x,nx,dx,sex),j)))			# attach ID to life table
}
names(lt.state)[names(lt.state) == "j"] = "i"					# rename j column to i (ID of mx file)
setkeyv(lt.state,c("i","x"))

## 3.9  function to produce a life table from qx values only (used in simulation for CI)
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

## 3.10   function to calculate confidence interval around life expectancy
## - E. Andreev and V. Shkolnikov. 2010. "Spreadsheet for calculation of confidence limits 
##   for any life table or healthy-life table quantity". MPIDR Technical Report 2010-005.
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

## 3.11   call to CI function
##	mssa
ltci.mssa<-data.table() 										# initialize an empty DT
.counter<-lt.mssa[x==0,.N]
.pb <- txtProgressBar(min = 0, max = .counter, style = 3)		# show a text progress bar for loop
for (j in 1:lt.mssa[x==0,.N]) {									# or "for (j in 1:2) {" for a quick test
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
##	county
ltci.county<-data.table() 										# initialize an empty DT
.counter<-lt.county[x==0,.N]
.pb <- txtProgressBar(min = 0, max = .counter, style = 3)		# show a text progress bar for loop
for (j in 1:lt.county[x==0,.N]) {								# or "for (j in 1:2) {" for a quick test
	ltci.county<-rbindlist(list(ltci.county, 					# fast rbind result to ltci.county (2 items)
			  				cbind(data.table( 					# format results of exsim as DT
			  				t(unlist(doLTCI(lt.county[i==j],    # run specific LT
			  				which.x=0,ns=500,level=.95)[		# pass parameters for simulation
			  				c(1,2,3,5)])))						# save just desired fields from exsim
			  			  ,j)									# attach ID to simulation results; 
			  )
	)
	setTxtProgressBar(.pb,j)												
}
names(ltci.county)[names(ltci.county) == "j"] = "i"					# rename j column to i (ID of mx file)
names(ltci.county)[names(ltci.county) == "which.x"] = "x"			# rename to agell
setkeyv(ltci.county,c("i","x"))
##	state
ltci.state<-data.table() 										# initialize an empty DT
for (j in 1:lt.state[x==0,.N]) {								# or "for (j in 1:2) {" for a quick test
	ltci.state<-rbindlist(list(ltci.state, 						# fast rbind result to ltci.state (2 items)
			  				cbind(data.table( 					# format results of exsim as DT
			  				t(unlist(doLTCI(lt.state[i==j],     # run specific LT
			  				which.x=0,ns=500,level=.95)[		# pass parameters for simulation
			  				c(1,2,3,5)])))						# save just desired fields from exsim
			  			  ,j)									# attach ID to simulation results; 
			  )
	)
}
names(ltci.state)[names(ltci.state) == "j"] = "i"				# rename j column to i (ID of mx file)
names(ltci.state)[names(ltci.state) == "which.x"] = "x"			# rename to agell
setkeyv(ltci.state,c("i","x"))

## 4	DIAGNOSTICS	----------------------------------------------------------

## 4.1 	diagnostic plots
## - these are of a de novo simulation, not the output dataset
## - use to check any single LT for typical results, by its idx value
doExHist <- function(dat=NULL,idx=NULL,age=0,reps=500,ci=.95) {
	while (!is.null(dev.list())) dev.off()
	par(mfrow=c(1,1))
	tmp<-doLTCI(LT=dat[i==idx],age,reps,ci)
	hist(tmp$exsim)
	abline(v=tmp$ex,col=2)
	abline(v=tmp$meanex,col=4)
	abline(v=tmp$ciex,col=4)
}
doExHist(dat=lt.mssa,idx=1,age=0,reps=500,ci=.9)
doExHist(dat=lt.county,idx=1,age=0,reps=500,ci=.9)
doExHist(dat=lt.state,idx=1,age=0,reps=500,ci=.9)

## 5	EXPORT DATA	----------------------------------------------------------

## 5.1 	update mortality dataset with some parameters from results
## mssa
names(mx.mssa)[names(mx.mssa) == "agell"] = "x" 						# rename agell to x for consistency
lt.mssa<-lt.mssa[mx.mssa[,c("i","x","sex","comID")],nomatch=0] 			# add MSSA comID and sex by ID
ltci.mssa<-ltci.mssa[mx.mssa[x==0,c("i","x","sex","comID")],nomatch=0]	# add MSSA comID and sex by ID
## county
names(mx.county)[names(mx.county) == "agell"] = "x" 							# rename agell to x for consistency
lt.county<-lt.county[mx.county[,c("i","x","sex","GEOID")],nomatch=0] 			# add COUNTY comID and sex by ID
ltci.county<-ltci.county[mx.county[x==0,c("i","x","sex","GEOID")],nomatch=0]	# add COUNTY comID and sex by ID
## state
names(mx.state)[names(mx.state) == "agell"] = "x" 							# rename agell to x for consistency
lt.state<-lt.state[mx.state[,c("i","x","sex","GEOID")],nomatch=0] 			# add STATE comID and sex by ID
ltci.state<-ltci.state[mx.state[x==0,c("i","x","sex","GEOID")],nomatch=0]	# add STATE comID and sex by ID

## 5.2  export datasets
saveRDS(ltci.mssa,   file=paste0(LTplace,"LTciMSSA.rds"))			# comID sex (char) x (age0) ex meanex ciex.low ciex.high
saveRDS(ltci.county, file=paste0(LTplace,"LTciCounty.rds"))		# GEOID sex (char) x (age0) ex meanex ciex.low ciex.high
saveRDS(ltci.state,  file=paste0(LTplace,"LTciState.rds"))		# GEOID sex (char) x (age0) ex meanex ciex.low ciex.high

# end
