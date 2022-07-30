# title: ltmake.r
# purpose: produce lifetables for CA burden project
# author: ethan sharygin (github:sharygin)
# notes:
# - intention is for analyst to be able to generate life tables by using default population + deaths,
#		or inputting their own population + deaths.
# - ACS 5-yr datasets populations are weighted by age/sex to sum to CB PEP estimates from middle year.
# - ACS tract population tables: from B01001 = age/sex by tract; subtables by race/ethnicity.
# - combine years to get higher exposures for better tables: 
#		geo		years (total)	years (by race)	      agegroups 			  by-characteristics
#		state	    1,3		                1,3				0,1-4,5(5)85,199	GEOID,sex,race
#		county	  1,3		                  5				0,1-4,5(5)85,199	GEOID,sex,race
#		mssa	    5		                   NA  			0(5)85,199			  GEOID,sex
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
.pkg	<- c("data.table","readr","readstata13","stringr","tidyr", "dplyr", "readxl") 
.inst   <- .pkg %in% installed.packages() 
if(length(.pkg[!.inst]) > 0) install.packages(.pkg[!.inst]) 
lapply(.pkg, require, character.only=TRUE)           

## 1.2  options
myYear <- 2021
controlPop  <-  F  # whether to control ACS to DOF pop totals
whichDeaths <- "real" # source of deaths data (real,fake,dof)
whichPop    <- "pep"  # source of population data (dof,pep)
critNx      <- 10000
critDx      <-   700

# critNx1 <- 1000
# critDx1 <- 70

## 1.3 	paths 
#setwd("C:/Users/fieshary/projects/CACommunityBurden")
myDrive   <- getwd()
myPlace   <- paste0(myDrive,"/myCCB") 
ccbUpstream   <- paste0(myDrive,"/myUpstream") 

server <- F
if (server) {
  source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
} else {
  source("G:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
}

# dofSecure <- "d:/users/fieshary/projects/vry-lt/dx"
# mySecure  <- "d:/0.Secure.Data/myData"


## 1.4 	links
#.ckey	  <- read_file(paste0(ccbUpstream,"/upstreamInfo/census.api.key.txt")) # census API key

# 1.5 - Read in Standard Files

source(paste0(standardsPlace, "ageChop.R"))

# 1.6 - year ranges (used in Chiang's function)
#yearLink <- readxl::read_xlsx(paste0(ccbInfo, "Year to Year-Group Linkage.xlsx"), sheet = "main")

range_1year <- 2000:myYear
range_3year <- 2001:(myYear - 1)
range_5year <- 2002:(myYear - 2)


## 2	GEOGRAPHY	----------------------------------------------------------------------

## 2.1	load tract to MSSA maps
trt00mssa <- read.dta13(paste0(ccbUpstream,"/lifeTables/dataIn/trt00mssa13.dta")) # 2009 TIGER/LINE census tracts to 2013 MSSAs
trt10mssa <- read.dta13(paste0(ccbUpstream,"/lifeTables/dataIn/trt10mssa13.dta")) # 2010 TIGER/LINE census tracts to 2013 MSSAs
mssacfips <- read.dta13(paste0(ccbUpstream,"/lifeTables/dataIn/mssa13cfips.dta")) # 2013 MSSA to county

## 2.2 	load county name to county FIPS code maps
countycfips <- read.dta13(paste0(ccbUpstream,"/lifeTables/dataIn/countycfips.dta")) # county name to county FIPS in GEOID format
# Use our standard FIPS JASPO



## 3	POPULATION	----------------------------------------------------------------------

## 3.1 	load 2000-09 intercensal + 2010-18 postcensal county + state population
# if (whichPop == "dof") nxCounty <- read.dta13(paste0(ccbUpstream,"/lifeTables/dataIn/dof_ic10pc19.dta"))
# if (whichPop == "pep") nxCounty <- read.dta13(paste0(ccbUpstream,"/lifeTables/dataIn/pep_ic10pc18_special.dta")) %>%
#   rename(Ethan = race7) %>%
#   left_join(select(raceLink, raceCode, Ethan), by = "Ethan") %>%
#   select(-Ethan)
# 
# # Filter state GEOID in case it exists
# nxCounty <- nxCounty %>%
#   filter(GEOID != "06000000000")
# 
# # Grab total race
# nxCounty_totalRace <- nxCounty %>%
#   group_by(GEOID, sex, year, agell, ageul) %>%
#   summarise(Nx = sum(Nx)) %>%
#   mutate(raceCode = "Total")
# 
# nxCounty <- bind_rows(nxCounty, nxCounty_totalRace)
# 
# # Grab total sex
# nxCounty_totalSex <- nxCounty %>%
#   group_by(GEOID, raceCode, year, agell, ageul) %>%
#   summarise(Nx = sum(Nx)) %>%
#   mutate(sex = "TOTAL")
# 
# nxCounty <- bind_rows(nxCounty, nxCounty_totalSex)
# 
# # Grab state as separate data frame
# nxState <- nxCounty %>%
#   group_by(sex, raceCode, year, agell, ageul) %>%
#   summarise(Nx = sum(Nx)) %>%
#   mutate(GEOID = "06000000000")

# 3.1 Load in nxCounty and nxState
nxCounty <- readRDS(paste0(ccbUpstream, "/lifeTables/dataIn/nxCounty.RDS"))
nxState <- readRDS(paste0(ccbUpstream, "/lifeTables/dataIn/nxState.RDS"))


## 3.3 	load ACS 2005-2015 five-year samples from NHGIS, rolled up to MSSA level
if (controlPop) {
  nxACS <- read.dta13(paste0(ccbUpstream,"/lifeTables/dataIn/acs5_mssa_adj.dta")) %>% # ACS tract pop, collapsed to MSSA and controlled to DOF county
    rename(Ethan = race7) %>%
    left_join(select(raceLink, raceCode, Ethan), by = "Ethan") %>%
    select(-Ethan)
} else {
  # nxACS <- read.dta13(paste0(ccbUpstream,"/lifeTables/dataIn/acs5_mssa.dta")) %>% # ACS tract pop collapsed to MSSA
  #   rename(Ethan = race7) %>%
  #   left_join(select(raceLink, raceCode, Ethan), by = "Ethan") %>%
  #   select(-Ethan) 
  
  nxACS <- readRDS(paste0(ccbUpstream,"/lifeTables/dataIn/nxMSSA.RDS")) %>%
    mutate(sex = str_to_title(sex)) %>%
    rename(Nx = nx) 
}

# Grab total sex - nxACS already has total Race
nxACS_totalSex <- nxACS %>%
  group_by(comID, year, agell, ageul, raceCode) %>%
  summarise(Nx = sum(Nx)) %>%
  mutate(sex = "Total")

nxACS <- bind_rows(nxACS, nxACS_totalSex)


## 4	DEATHS ---------------------------------------------------------------------------

## 4.1	load selected deaths master file
if (whichDeaths=="fake") cbdDeaths		<- load(paste0(ccbUpstream,"/upData/cbdDat0SAMP.R")) 

if (whichDeaths=="real") {
  
  cbdDeaths		<- readRDS(paste0(securePlace,"myData/ccb_processed_deaths.RDS")) %>%
    filter(year <= myYear) %>%
    left_join(select(raceLink, raceCode, CHSI), by = "CHSI") %>%
    select(-CHSI)
  
} 

if (whichDeaths=="dof")  cbdDeaths 		<- read.dta13(paste0(dofSecure,"/dof_deaths_mi.dta")) 

if (whichDeaths %in% c("real","fake")) {
  ## MSSA
  dxMssa <- cbdDeaths %>%
    filter(sex %in% c("M", "F"), !is.na(age), !is.na(year), as.numeric(substring(GEOID,1,5)) %in% 6001:6115) %>%
    mutate(agell = 5*floor(age/5), # create lower ageGroup by cutting into 5 years
           agell = ifelse(agell > 85, 85, agell), # set lower ageGroup > 85 to 85
           ageul = ifelse(age < 85, agell + 4, 199), # create upper ageGroup; if age > 85, then set to 199
           sex = ifelse(sex == "F", "Female", "Male")) %>%
    left_join(trt10mssa, by = "GEOID") %>%
    group_by(year,comID,sex, agell,ageul) %>%
    summarise(Dx = n()) %>%
    mutate(raceCode = "Total")

  # Grab total sex
  dxMssa_totalSex <- dxMssa %>%
    group_by(year,comID,raceCode,agell,ageul) %>%
    summarise(Dx = sum(Dx)) %>%
    mutate(sex = "Total")
    
  dxMssa <- bind_rows(dxMssa, dxMssa_totalSex)
  

  
  ## county - age Groups: 0, 1-4, 5-84 (5), 85-199
  dxCounty <- cbdDeaths %>%
    filter(sex %in% c("M", "F"), !is.na(age), !is.na(year), !is.na(county)) %>%
    mutate(agell = ageChop(INAGE = age,
                           myCuts = TRUE,
                           my_lAge = c(0, 1, seq(5, 85, by = 5)),
                           my_uAge = c(0, seq(4, 84, by = 5), 199),
                           my_ageName = c(0, 1, seq(5, 85, by = 5)), 
                           ourServer = server), 
           ageul = ageChop(INAGE = age,
                           myCuts = TRUE,
                           my_lAge = c(0, 1, seq(5, 85, by = 5)),
                           my_uAge = c(0, seq(4, 84, by = 5), 199),
                           my_ageName = c(0, seq(4, 84, by = 5), 199), 
                           ourServer = server),
           sex = ifelse(sex == "F", "Female", "Male")) %>%
    left_join(countycfips, by = "county") %>%
    mutate(GEOID = sprintf("%05d000000",cfips)) %>%
    group_by(year,GEOID,sex, raceCode, agell,ageul) %>%
    summarise(Dx = n())
  
  # Get total sex
  dxCounty_totalSex <- dxCounty %>%
    group_by(year,GEOID, raceCode, agell,ageul) %>%
    summarise(Dx = sum(Dx)) %>%
    mutate(sex = "Total")
  
  dxCounty <- bind_rows(dxCounty, dxCounty_totalSex)
  
  # Get total race
  dxCounty_totalRace <- dxCounty %>%
    group_by(year,GEOID, sex, agell,ageul) %>%
    summarise(Dx = sum(Dx)) %>%
    mutate(raceCode = "Total")
  
  dxCounty <- bind_rows(dxCounty, dxCounty_totalRace)
  
  ## State
  dxState <- dxCounty %>%
    group_by(year, sex, raceCode, agell,ageul) %>%
    summarise(Dx = sum(Dx)) %>%
    mutate(GEOID = "06000000000")
  
  
  
  
  # dxCounty <- cbdDeaths %>%
  #   filter(sex %in% c("M", "F"), !is.na(age), !is.na(year), !is.na(county)) %>%
  #   mutate(agell = ifelse(age == 0, 0, age),
  #          agell = ifelse(age %in% 1:4, 1, agell),
  #          agell = ifelse(age > 5, 5*floor(age/5), agell), # create lower ageGroup by cutting into 5 years
  #          agell = ifelse(agell > 85, 85, agell), # set lower ageGroup > 85 to 85
  #          ageul = ifelse(agell == 0, 0, age), 
  #          ageul = ifelse(agell == 1, 4, ageul), 
  #          ageul = ifelse(agell %in% 5:80, agell + 4, agell), # create upper ageGroup; if age > 85, then set to 199
  #          ageul = ifelse(agell == 85, 199, ageul),
  #          sex = ifelse(sex == "F", "FEMALE", "MALE")) %>%
  #   left_join(countycfips, by = "county") %>%
  #   mutate(GEOID = sprintf("%05d000000",cfips)) %>%
  #   group_by(year,GEOID,sex, CHSI, agell,ageul) %>%
  #   summarise(Dx = n()) 
  
}

## 5.1	function to generate an extract of years by geo and merge pop + deaths
##		syntax: dx=deaths data, nx=pop data, nyrs=N neighborings years to combine, y=target year, level=geography
doExtract <- function(dx=NULL, nx=NULL, nyrs=NA, y=NA, level=NA) {
  
  if (level=="mssa") {
    dx <- mutate(dx, GEOID = comID)
    nx <- mutate(nx, GEOID = comID)
  }
  
  if (length(unique(nx$year[nx$year>=y-nyrs & nx$year<=y+nyrs]))<(2*nyrs+1)) { stop("Exposure data are missing for one or more years") }
  if (length(unique(dx$year[dx$year>=y-nyrs & dx$year<=y+nyrs]))<(2*nyrs+1)) { stop("Incidence data are missing for one or more years") }
  
  yearsRange <- (y - nyrs):(y + nyrs)
  
  tmp <- dx %>%
    filter(year %in% yearsRange) %>%
    full_join(filter(nx, year %in% yearsRange), by = c('GEOID','sex','year','agell','ageul','raceCode'))
  
  # Ethan's 'join'
  # tmp <- merge(nx[nx$year>=y-nyrs & nx$year<=y+nyrs, ], dx[dx$year>=y-nyrs & dx$year<=y+nyrs, ], 
  #               on=c('GEOID','sex','year','agell','ageul','CHSI'),
  #               all.x=TRUE,all.y=TRUE)
  
  tmp <- setDT(tmp)
  
  tmp<-tmp[,.(Nx=sum(Nx),Dx=sum(Dx)),by=c('GEOID','sex','agell','ageul','raceCode')] # collapse

  tmp <- as.data.frame(complete(tmp, GEOID,sex,raceCode,agell)) %>%
    replace_na(list(Dx = 0)) %>%
    mutate(year = y) %>%
    ungroup()

  if (level == "mssa") {
    tmp <- tmp %>%
      mutate(comID = GEOID) %>%
      select(-GEOID)
    
    dx <- select(dx, -GEOID)
    nx <- select(nx, -GEOID)
  }
  
  return(tmp)
}


# For some reason, the range is 2009-2016 for MSSA
if (whichDeaths %in% c("real","fake")) { 
  range<-2009:2014 # or later if available. 'fake' has nx 2009-2018 and dx 2007-2014
  range<-2009:(myYear - 2) # or later if available. 'fake' has nx 2009-2018 and dx 2007-2014
  
  mxMssa_list <- lapply(range,doExtract,dx=dxMssa,nx=nxACS,nyrs=2,level="mssa")
  
  mxMssa <- bind_rows(mxMssa_list) %>%
    mutate(nyrs = 5)
  

}

## county

# 1 year with TOTAL race
# mxCounty_1year_list <- lapply(range_1year,doExtract,dx=dxCounty,nx=nxCounty,nyrs=0,level="county")
# mxCounty_1year <- bind_rows(mxCounty_1year_list) %>%
#   filter(raceCode == "Total") %>%
#   mutate(nyrs = 1)

# 1 year with race
mxCounty_1year_list <- lapply(range_1year,doExtract,dx=dxCounty,nx=nxCounty,nyrs=0,level="county")
mxCounty_1year <- bind_rows(mxCounty_1year_list) %>%
  mutate(nyrs = 1)

# 3 year with race
mxCounty_3year_list <- lapply(range_3year,doExtract,dx=dxCounty,nx=nxCounty,nyrs=1,level="county")
mxCounty_3year <- bind_rows(mxCounty_3year_list) %>%
  # filter(raceCode == "Total") %>%
  mutate(nyrs = 3)

# 5 year with race
mxCounty_5year_list <- lapply(range_5year,doExtract,dx=dxCounty,nx=nxCounty,nyrs=2,level="county")
mxCounty_5year <- bind_rows(mxCounty_5year_list) %>%
  mutate(nyrs = 5)

# Final county
mxCounty <- bind_rows(mxCounty_1year, mxCounty_3year, mxCounty_5year) %>%
  filter(!raceCode %in% c("Other", "Unknown"))



## state 

# 1 year with race
mxState_1year_list <- lapply(range_1year,doExtract,dx=dxState,nx=nxState,nyrs=0,level="state")
mxState_1year <- bind_rows(mxState_1year_list) %>%
  mutate(nyrs = 1) 

# 3 year with race
mxState_3year_list <- lapply(range_3year,doExtract,dx=dxState,nx=nxState,nyrs=1,level="state")
mxState_3year <- bind_rows(mxState_3year_list) %>%
  mutate(nyrs = 3) 

# 5 year with race
mxState_5year_list <- lapply(range_5year,doExtract,dx=dxState,nx=nxState,nyrs=2,level="state")
mxState_5year <- bind_rows(mxState_5year_list) %>%
  mutate(nyrs = 5) 

# Final state
mxState <- bind_rows(mxState_1year, mxState_3year, mxState_5year) %>%
  filter(!raceCode %in% c("Other", "Unknown"))


#XXX testing
#mxState$pDead = 100*mxState$Dx / mxState$Nx # quality Check - deaths / population

# By this point, 2 dfs for county (+ 1 more for county), 1df for state (+1 more for state), and mssa dfs are generated JASPO


## 6	LIFE TABLES ----------------------------------------------------------------------

## 6.1	generic function to produce a life table 
## 		x is a vector of age groups, nx is the corresponding vector of pop, dx of deaths
##		sex is M or MALE or F or FEMALE (used to calc ax); ax is an optional vector of ax values
##		previously estimated ax values are available from the UN WPP, USMDB, NCHS, including by race. 
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
  # return(data.frame(x = x, n = n, Nx = Nx, Dx = Dx, mx = mx, ax = ax, qx = qx, px = px,
  #             lx = lx, dx = dx, Lx = Lx, Tx = Tx, ex = ex, sp2 = sp2, wsp2 = wsp2,
  #             Twsp2 = Twsp2, se2 = se2, exlow = exlow, exhigh = exhigh))
}



## 6.2 add index to mx tables, and calculate sum pop and deaths per each index
mxState1 <- mxState %>%
  group_by(agell, ageul) %>%
  mutate(i = row_number()) %>%
  group_by(i) %>%
  mutate(sumNx = sum(Nx, na.rm = T), 
         sumDx = sum(Dx, na.rm = T)) %>%
  ungroup() %>%
  filter(sumNx >= critNx & sumDx >= critDx)

mxCounty1 <- mxCounty %>%
  group_by(agell, ageul) %>%
  mutate(i = row_number()) %>%
  group_by(i) %>%
  mutate(sumNx = sum(Nx, na.rm = T), 
         sumDx = sum(Dx, na.rm = T)) %>%
  ungroup() %>%
  filter(sumNx >= critNx & sumDx >= critDx)

mxMssa1 <- mxMssa %>%
  group_by(agell, ageul) %>%
  mutate(i = row_number()) %>%
  group_by(i) %>%
  mutate(sumNx = sum(Nx, na.rm = T), 
         sumDx = sum(Dx, na.rm = T)) %>%
  ungroup() %>%
  filter(sumNx >= critNx & sumDx >= critDx)


## 6.4	Call LT routine by geography
## state
# ltState <- mxState %>%
#   group_by(id, nyrs, GEOID, sex, raceCode, year) %>%
#   bind_cols(doLTChiangCI(x=agell,Nx=Nx,Dx=Dx,sex=sex))

ltState <- setDT(mxState1)[, doLTChiangCI(x=agell,Nx=Nx,Dx=Dx,sex=sex), by=c("i","nyrs","GEOID","sex","raceCode","year")]
setkeyv(ltState,c("i","x"))

## county
ltCounty <- setDT(mxCounty1)[, doLTChiangCI(x=agell,Nx=Nx,Dx=Dx,sex=sex), by=c("i","nyrs","GEOID","sex","raceCode","year")]
setkeyv(ltCounty,c("i","x"))

## MSSA
if (whichDeaths %in% c("real","fake")) { 
  ltMssa <- setDT(mxMssa1)[, doLTChiangCI(x=agell,Nx=Nx,Dx=Dx,sex=sex), by=c("i","nyrs","comID","sex","raceCode","year")]
  setkeyv(ltMssa,c("i","x"))
}


## 7	REVIEW/EXPORT ----------------------------------------------------------------------

# checkState <- readRDS(paste0(ccbUpstream,"/lifeTables/dataOut/LTciState.rds"))
# checkCounty <- readRDS(paste0(ccbUpstream,"/lifeTables/dataOut/LTciCounty.rds"))
# checkMssa <- readRDS(paste0(ccbUpstream,"/lifeTables/dataOut/LTciMSSA.rds"))

## 7.1	EXPORT
## full LT
saveRDS(ltState, paste0(ccbUpstream, "/lifeTables/dataOut/LTciState.RDS"))
saveRDS(ltCounty, paste0(ccbUpstream, "/lifeTables/dataOut/LTciCounty.RDS"))
saveRDS(ltMssa, paste0(ccbUpstream, "/lifeTables/dataOut/LTciMSSA.RDS"))

## e0 only - save upStream
saveRDS(ltState[x==0,c("nyrs","GEOID","sex","raceCode","year","ex","exlow","exhigh")],
        paste0(ccbUpstream,"/lifeTables/dataOut/e0ciState.RDS"))
saveRDS(ltCounty[x==0,c("nyrs","GEOID","sex","raceCode","year","ex","exlow","exhigh")],
        paste0(ccbUpstream,"/lifeTables/dataOut/e0ciCounty.RDS"))
if (whichDeaths %in% c("real","fake")) { 
  saveRDS(ltMssa[x==0,c("nyrs","comID","sex","raceCode","year","ex","exlow","exhigh")]
          ,paste0(ccbUpstream,"/lifeTables/dataOut/e0ciMSSA.RDS"))
}

# e0 only - save myCBD/data
saveRDS(ltState[x==0,c("nyrs","GEOID","sex","raceCode","year","ex","exlow","exhigh")],
        paste0(ccbData,"e0ciState.RDS"))
saveRDS(ltCounty[x==0,c("nyrs","GEOID","sex","raceCode","year","ex","exlow","exhigh")],
        paste0(ccbData,"e0ciCounty.RDS"))
if (whichDeaths %in% c("real","fake")) { 
  saveRDS(ltMssa[x==0,c("nyrs","comID","sex","raceCode","year","ex","exlow","exhigh")]
          ,paste0(ccbData,"e0ciMSSA.RDS"))
}

## 7.2	Review
mx.state[sex=="TOTAL" & CHSI=="TOTAL",.(Nx=sum(Nx),Dx=sum(Dx)),by=c("GEOID","sex","year","CHSI")] # state sum
lt.state[x==0 & sex=="TOTAL" & CHSI=="TOTAL",c("GEOID","sex","year","CHSI","ex","exlow","exhigh")] 
mx.county[sex=="TOTAL" & CHSI=="TOTAL",.(Nx=sum(Nx),Dx=sum(Dx)),by=c("nyrs","sex","year","CHSI")] # state sum
lt.county[x==0 & sex=="TOTAL" & CHSI=="TOTAL" & year==2017,
          c("nyrs","GEOID","sex","year","CHSI","ex","exlow","exhigh")] 
mx.mssa[sex=="TOTAL" & CHSI=="TOTAL",.(Nx=sum(Nx),Dx=sum(Dx)),by=c("sex","year","CHSI")] # state sum
lt.mssa[x==0 & sex=="TOTAL" & CHSI=="TOTAL" & (year %in% c(2010,2017)),
        c("comID","sex","year","CHSI","ex","exlow","exhigh")] 





# Jaspo MSSA Pop Exploration - DELETE SOON
# source("/mnt/projects/FusionData/SDOH/pullACS_function_V2.R")
# 
# 
# sdohVars <- read_xlsx("/mnt/projects/FusionData/SDOH/linkageSDOH.xlsx", sheet = "Age") %>%
#   slice(1:2)
# 
# mssaPop <- getACS(State=06, # Set to NULL if pulling ZCTA
#                   Geography="tract", # Options: county, zcta, tract.. for acs1, puma
#                   MSSA = F, # if you want MSSA output, set T and geograph = Tract
#                   Survey="acs5",
#                   Year=2018,
#                   moeLevel=95,
#                   asList = T, # Want as a list, or as one data frame?
#                   df = sdohVars)


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

