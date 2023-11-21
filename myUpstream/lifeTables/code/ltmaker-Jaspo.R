# 0 Script Documentation ------------------------------------------------------------------------------------------------------------------------------

# Authors: Originally created by Ethan Sharygin (Github: sharygin), revised by Michael Samuel, DrPH and Jaspreet Kang (Fusion Center/Office of Policy and Planning, CDPH)
# Contact information for any questions: ccb@cdph.ca.gov

# About this script:
# This script produces life tables for the California Community Burden of Disease Engine (CCB), State Health Assessment Core Module, and for various analytical purposes.
# Life tables in this script are produced at multiple geographic levels, by various demographic characteristics, and for several year groupings. See the table below for more information on the outputs produced:

#		geo        years      agegroups                 by-characteristics
#		state      1,3,5    0,1-4,5(5)85,199      GEOID,sex(incl. Total),race(incl. Total)
#		county     1,3,5    0,1-4,5(5)85,199      GEOID,sex(incl. Total),race(incl. Total)
#		mssa         5        0(5)85,199                GEOID,sex(incl. Total)

# Pre-requisites:
# Must run 'createPopData_for_ltmaker.R' first to generate up to date state, county, and MSSA level population datasets

# Script dependencies:
# R packages:
  # - pacman
  # - readr
  # - readstata13
  # - stringr
  # - tidyr
  # - dplyr
  # - readxl
  # - purrr

# Inputs:
# FusionStandards.R - loads in standard paths and objects
# ageChop.R - loads in standard custom function to cut age into age groups
# trt10mssa13.dta - 2010 TIGER/LINE census tracts to 2013 MSSAs linkage file
# countycfips.dta - county name to county FIPS linkage file
# nxCounty.RDS - county-level population data created from 'createPopData_for_ltmaker.R' - Contains every year(2000-)-county-sex(including total)-race(including total)-ageGroup combination
# nxState.RDS - state-level population data created from 'createPopData_for_ltmaker.R' - Contains every year(2000-)-state-sex(including total)-race(including total)-ageGroup combination
# nxMSSA.RDS - MSSA-level population data created from 'createPopData_for_ltmaker.R' - Contains every year(2007-)-MSSA-sex-race(including total)-ageGroup combination
# ccb_processed_deaths.RDS - semi-processed, record-level death data created further upstream; Contains death data from 2000-


# Outputs:
# 1. LTciState.RDS - full life tables at state level
# 2. LTciCounty.RDS - full life tables at county level
# 3. LTciMSSA.RDS - full life tables at MSSA level
# 4. e0ciState.RDS - life expectancies at birth at state level
# 5. e0ciCounty.RDS - life expectancies at birth at county level
# 6. e0ciMSSA.RDS - life expectancies at birth at MSSA level



# Script structure:
# 1. Setup - Load required R packages, set global constants, set directory paths, source standard and geography linkage files
# 2. Read in and prepare population data
# 3. Read in and prepare death data
# 4. Merge population and death data
# 5. Calculate life expectancy estimates
# 6. Save data 
# 7. Data quality checks



# Ethan's Notes:
# - 'nx' and 'dx' are standard notations used in LE literature, and refers to population and death respectively
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



# 1 Setup -------------------------------------------------------------------------------------------------------------------------------------------------------

## 1.1 Load R packages -------------------------------------------------

# install.packages("pacman") # Uncomment line if pacman is not installed on your system
pacman::p_load("readr", "readstata13", "stringr", "tidyr", "dplyr", "readxl", "purrr") # Used to install (if needed) and load in R packages. 

## 1.2 Set Global Options -------------------------------------------------

myYear      <- 2022   # Most recent year of death data available
controlPop  <-  F     # whether to control ACS to DOF pop totals
whichDeaths <- "real" # source of deaths data (real,fake,dof) - REMOVE
critNx      <- 10000 # ADD COMMENT
critDx      <-   700 # ADD COMMENT

### 1.2.1 Global Variables Based on 'myYear' Object - Do not adjust -------
range_1year <- 2000:myYear       # Used for annual estimates
range_3year <- 2001:(myYear - 1) # Used for 3-year estimates
range_5year <- 2002:(myYear - 2) # Used for 5-year estimates

## 1.3 Set Paths ----------------------------------------------------------

CCB         <- TRUE
server      <- FALSE
myDrive     <- getwd()
myPlace     <- paste0(myDrive,"/myCCB/") 

## 1.4 Source standard files ----------------------------------------------

source(paste0(myPlace,"/Standards/FusionStandards.R")) # Loads in standard paths and objects
source(paste0(standardsPlace, "ageChop.R")) # Loads in function for cutting age into age groups

## 1.5 Read in geography linkages files -----------------------------------

## 2.1	load tract to MSSA maps

trt10mssa <- read_csv(paste0(standardsPlace, "Tract to Community Linkage.csv")) %>% 
  select(GEOID, comID) %>% 
  add_row(GEOID = "06037930401", comID = "78.2xx")

## 2.2 	load county name to county FIPS code maps

fipsCounty <- readxl::read_xlsx(paste0(standardsPlace, "countyLink.xlsx")) %>%
  select(county = countyName, FIPSCounty) %>% 
  mutate(FIPSCounty = as.numeric( paste0("6", FIPSCounty) ))

# 2 Read in and Prepare Population Data -----------------------------------------------------------------------------------------------------------------------

## 2.1 State and County Population -----------------------------------------

nxCounty <- readRDS(paste0(ccbUpstream, "/lifeTables/dataIn/nxCounty.RDS")) # Contains every year(2000-)-county-sex(including total)-race(including total)-ageGroup combination
nxState <-  readRDS(paste0(ccbUpstream, "/lifeTables/dataIn/nxState.RDS")) # Contains every year(2000-)-state-sex(including total)-race(including total)-ageGroup combination

## 2.2 MSSA Population Data ------------------------------------------------

if (controlPop) { 
  
  # load ACS 2005-2015 five-year samples from NHGIS, rolled up to MSSA level
  nxMSSA <- read.dta13(paste0(ccbUpstream,"/lifeTables/dataIn/acs5_mssa_adj.dta")) %>% # ACS tract pop, collapsed to MSSA and controlled to DOF county
    rename(Ethan = race7) %>%
    left_join(select(raceLink, raceCode, Ethan), by = "Ethan") %>%
    select(-Ethan)
  
} else {

  # Read in MSSA population data (pulled from ACS 5-year; rolled up to MSSA from Tracts; no control adjustments)
  nxMSSA <- readRDS(paste0(ccbUpstream,"/lifeTables/dataIn/nxMSSA.RDS")) %>%
    mutate(sex = str_to_title(sex)) %>%
    rename(Nx = nx, GEOID = comID) 
}

# Grab total sex - popMSSA already has total Race
nxMSSA_totalSex <- nxMSSA %>%
  group_by(GEOID, year, agell, ageul, raceCode) %>%
  summarise(Nx = sum(Nx)) %>%
  mutate(sex = "Total")

# Add total sex to popMSSA
nxMSSA <- bind_rows(nxMSSA, nxMSSA_totalSex) # Contains every year(2007-)-MSSA-sex(including total)-race(including total)-ageGroup combination


# 3 Read in and Prepare Death Data -------------------------------------------------------------------------------------------------------------------------------

## 3.1	Read in semi-processed, record-level death data ----------------------

# Sampled/fake death data
if (whichDeaths=="fake") dx	<- load(paste0(ccbUpstream,"/upData/cbdDat0SAMP.R")) 

# Real death data
if (whichDeaths=="real") {
  
  dx <- readRDS(paste0(securePlace,"myData/ccb_processed_deaths.RDS")) %>%
    filter(year <= myYear) %>%
    left_join(select(raceLink, raceCode, CHSI), by = "CHSI") %>%
    mutate(sex = case_when(sex == "F" ~ "Female", 
                           sex == "M" ~ "Male",
                           TRUE ~ sex)) %>% 
    select(-CHSI)
  
} 

# if (whichDeaths=="dof")  cbdDeaths 		<- read.dta13(paste0(dofSecure,"/dof_deaths_mi.dta")) # DO WE STILL NEED THIS IN THE SCRIPT? IF WE DO, PUT THE IF STATEMENT BACK IN BELOW


## 3.2 Prepare Death Data -----------------------------------------------------

### 3.2.1 MSSA-level -------------

dxMSSA <- dx %>% 
  filter(!is.na(year), !is.na(age), GEOID != "", !is.na(GEOID)) %>% # Remove missing year, age, and GEOID
  mutate(agell = 5*floor(age/5), # create age lower limit
         agell = ifelse(agell > 85, 85, agell), # if lower limit > 85, set to 85 since the oldest age group is capped to 85+
         ageul = ifelse(age < 85, agell + 4, 199)) %>%  # create age upper limit from the lower limit
  left_join(trt10mssa, by = "GEOID") %>% # Link tract GEOID to MSSA
  bind_rows(mutate(., sex = "Total")) %>% # Add total sex to data frame
  group_by(year,GEOID=comID,sex, agell,ageul) %>% # Renaming comID to GEOID for convenience and consistency throughout the code
  summarise(Dx = n()) %>%
  ungroup() %>% 
  filter(sex %in% c("Female", "Male", "Total")) %>% # Removes Unknown/missing sex
  mutate(raceCode = "Total")


### 3.2.2 County-level ---------

# Create age group lower and upper limits to pass through standard, custom ageChop function
ageLowerLimit <- c(0, 1, seq(5, 85, by = 5))
ageUpperLimit <- c(0, seq(4, 84, by = 5), 199)

dxCounty <- dx %>%
  filter(!is.na(age), !is.na(year)) %>% # Remove missing age, year
  mutate(agell = ageChop(INAGE = age, # Using standard age chop function to create age lower limits
                         myCuts = TRUE,
                         my_lAge = ageLowerLimit,
                         my_uAge = ageUpperLimit,
                         my_ageName = ageLowerLimit, 
                         ourServer = server), 
         ageul = ageChop(INAGE = age, # Using standard age chop function to create age upper limits
                         myCuts = TRUE,
                         my_lAge = ageLowerLimit,
                         my_uAge = ageUpperLimit,
                         my_ageName = ageUpperLimit, 
                         ourServer = server)) %>% 
  left_join(fipsCounty, by = "county") %>% # Bring in county fips code
  mutate(GEOID = sprintf("%05d000000", FIPSCounty)) %>% # Overwrite GEOID column to only include county fips
  bind_rows(mutate(., sex = "Total")) %>% # Add total sex to data frame
  bind_rows(mutate(., raceCode = "Total")) %>% # Add total race to data frame
  group_by(year,GEOID,sex, raceCode, agell,ageul) %>%
  summarise(Dx = n()) %>% 
  ungroup() %>% 
  filter(sex %in% c("Female", "Male", "Total"), !raceCode %in% c("Other", "Unknown")) # Remove Unknown/missing sex, unknown/other/missing race

### 3.2.3 State-level --------------

dxState <- dxCounty %>%
  group_by(year, sex, raceCode, agell,ageul) %>%
  summarise(Dx = sum(Dx)) %>%
  mutate(GEOID = "06000000000")

dxCounty <- dxCounty %>% filter(!is.na(GEOID), !grepl("NA", GEOID)) # Remove missing counties from county-level death data frame


# 4 Merge Population and Death data frames -------------------------------------------------------------------------------------------------------------------------------------------

## 4.1 Function to generate an extract of years by geo, and merge pop + deaths --------------------------------------------------------

doExtract <- function(dx=NULL, nx=NULL, nyrs=NA, y=NA) { # syntax: dx=deaths data, nx=pop data, nyrs=N neighborings years to combine, y=target year

  yearsRange <- (y - nyrs):(y + nyrs)
  
  dx <- dx %>% filter(year %in% yearsRange)
  nx <- nx %>% filter(year %in% yearsRange)
  
  
  if (length(unique(nx$year)) < (2*nyrs+1)) { stop("Exposure data are missing for one or more years") }
  if (length(unique(dx$year)) < (2*nyrs+1)) { stop("Incidence data are missing for one or more years") }
  
  
  tmp <- suppressMessages( 
    dx %>% 
      full_join(nx, by = c('GEOID','sex','year','agell','ageul','raceCode')) %>% 
      group_by(GEOID, sex, agell, ageul, raceCode) %>%
      summarise(Nx = sum(Nx, na.rm = TRUE), 
                Dx = sum(Dx, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(year = y)
  )
  
  
    # group_by(year) %>% 
    # complete(GEOID, sex, raceCode, agell, fill = list(Dx = 0, Nx = 0)) %>% # Is complete even needed here? I dont think so
    # ungroup()

  
  # Data quality check - Checking if every combination exist
  everyComb <- all( c(table(tmp$sex, tmp$raceCode, tmp$agell, useNA = "ifany")) == length(unique(nx$GEOID)) )
  print(paste0("Data quality check: For year(s) ", min(yearsRange), "-", max(yearsRange), ", every combination exists: ", everyComb))
  
  
  return(tmp)
}


## 4.2 Merge MSSA population and death ----------------------------------
range <- 2009:(myYear - 2)

mxMSSA <- lapply(range,doExtract,dx=dxMSSA,nx=nxMSSA,nyrs=2) %>% 
  bind_rows() %>% 
  mutate(nyrs = 5)

## 4.3 Merge County population and death --------------------------------

### 4.3.1 County-level 1-year (race included) ----------

mxCounty_1year <- lapply(range_1year,doExtract,dx=dxCounty,nx=nxCounty,nyrs=0) %>% 
  bind_rows() %>%
  mutate(nyrs = 1)

### 4.3.2 County-level 3-year (race included) ----------

mxCounty_3year <- lapply(range_3year,doExtract,dx=dxCounty,nx=nxCounty,nyrs=1) %>% 
  bind_rows() %>% 
  mutate(nyrs = 3)

### 4.3.3 County-level 5-year (race included) ----------

mxCounty_5year <- lapply(range_5year,doExtract,dx=dxCounty,nx=nxCounty,nyrs=2) %>% 
  bind_rows() %>% 
  mutate(nyrs = 5)

### 4.3.4 Bind county-level data frame above ----------

mxCounty <- bind_rows(mxCounty_1year, mxCounty_3year, mxCounty_5year) 



## 4.4 Merge State population and death --------------------------------

### 4.4.1 State-level 1-year (race included) ----------

mxState_1year <- lapply(range_1year,doExtract,dx=dxState,nx=nxState,nyrs=0) %>% 
  bind_rows() %>% 
  mutate(nyrs = 1) 

### 4.4.2 State-level 3-year (race included) ----------

mxState_3year <- lapply(range_3year,doExtract,dx=dxState,nx=nxState,nyrs=1) %>% 
  bind_rows() %>% 
  mutate(nyrs = 3)

### 4.4.3 State-level 5-year (race included) ----------

mxState_5year <- lapply(range_5year,doExtract,dx=dxState,nx=nxState,nyrs=2) %>% 
  bind_rows() %>% 
  mutate(nyrs = 5) 

### 4.4.4 Bind state-level data frame above ----------

mxState <- bind_rows(mxState_1year, mxState_3year, mxState_5year) 


# Quality checks
dqCheck <- mxState %>% 
  filter(nyrs == 1, sex == "Total", raceCode == "Total") %>% 
  group_by(year) %>% 
  summarise(Dx = sum(Dx), Nx = sum(Nx)) %>% 
  mutate(rate = 100000*Dx/Nx) %>% 
  full_join(
    dx %>% 
      filter(!is.na(age)) %>% 
      count(year, name = "Dx1") %>% 
      full_join (
        nxCounty %>% 
          filter(sex == "Total", raceCode == "Total") %>% 
          group_by(year) %>% 
          summarise(Nx1 = sum(Nx))
      ) %>% 
      mutate(rate1 = 1000000*Dx1/Nx1)
  ) %>% 
  mutate(eqDx = Dx == Dx1, 
         eqNx = Nx == Nx1, 
         eqRate = rate == rate1)

print(paste0("Data Quality Check: 2000-2022 Statewide number of deaths from 'mxState' are quivalent to the corresponding numbers in the raw death file ('dx'): ", all(c(dqCheck$eqDx))))
print(paste0("Data Quality Check: 2000-2022 Statewide populations from 'mxState' are equivalent to the corresponding numbers in 'nxState': ", all(c(dqCheck$eqNx))))
print(paste0("Data Quality Check: 2000-2022 Statewide crude death rates computed from 'mxState' are equivalent to the corresponding rates calculated from the raw death file ('dx') and 'nxState': ", all(c(dqCheck$eqNx))))


mxState %>% 
  filter(sex == "Total", raceCode == "Total", nyrs == 1) %>% 
  mutate(rate = 100000 * Dx / Nx) %>% 
  ggplot(aes(x = year, y = rate)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2000:myYear, labels = 2000:myYear) +
  facet_grid(vars(agell), scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


# For testing out different critNx and critDx values
# saveRDS(mxMSSA, paste0(ccbUpstream, "/lifeTables/code/thresholds/mxMSSA.RDS"))
# saveRDS(mxCounty, paste0(ccbUpstream, "/lifeTables/code/thresholds/mxCounty.RDS"))
# saveRDS(mxState, paste0(ccbUpstream, "/lifeTables/code/thresholds/mxState.RDS"))

# 5	Calculating Life Expectancy ----------------------------------------------------------------------------------------------------------

## 5.1	Generic function to calculate a life table ---------------------------

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
  return(tibble(x = x, n = n, Nx = Nx, Dx = Dx, mx = mx, ax = ax, qx = qx, px = px,
              lx = lx, dx = dx, Lx = Lx, Tx = Tx, ex = ex, sp2 = sp2, wsp2 = wsp2,
              Twsp2 = Twsp2, se2 = se2, exlow = exlow, exhigh = exhigh))
}

## 5.2 Function that first removes stratas that dpn't meet criteria, then calls the function above ----------------
calcLT <- function(myData, groupByCols = c("GEOID", "year", "nyrs", "sex", "raceCode")) {
  
  myData %>% 
    group_by(across({{ groupByCols }})) %>% 
    mutate(sumNx = sum(Nx, na.rm = T), # Get total population per nyrs-year-GEOID-sex-race strata
           sumDx = sum(Dx, na.rm = T)) %>%  # Get total deaths per nyrs-year-GEOID-sex-race strata
    ungroup() %>% 
    filter(sumNx >= critNx & sumDx >= critDx) %>% # Removes stratas below deaths and population threshold (defined at the beginning)
    select(-sumNx, -sumDx) %>% 
    mutate(tSex = sex) %>%
    group_by(across({{ groupByCols }})) %>% 
    nest() %>% 
    mutate(
      data = map(data, ~ doLTChiangCI(x = .x$agell, Nx = .x$Nx, Dx = .$Dx, sex = .x$tSex)) # Call Chiang function defined above; Calculates life expectancy per nyrs-year-GEOID-sex-race strata
    ) %>% 
    unnest(data) %>% 
    ungroup()
}

## 5.3 Calculate life expectancy ----------------------------------------

### 5.3.1 State-level life table -----------------

ltState <- calcLT(myData = mxState) 

### 5.3.2 County-level life table ------------------

ltCounty <- calcLT(myData = mxCounty) 

### 5.3.3 MSSA-level life table -------------------

ltMSSA <- calcLT(myData = mxMSSA) %>% 
  rename(comID = GEOID)


# 6 Save data -------------------------------------------------------------------------------------------------------------------

## 6.1 Save full life tables ---------------------------------------------

saveRDS(ltState, paste0(ccbUpstream, "/lifeTables/dataOut/LTciState.RDS"))
saveRDS(ltCounty, paste0(ccbUpstream, "/lifeTables/dataOut/LTciCounty.RDS"))
saveRDS(ltMSSA, paste0(ccbUpstream, "/lifeTables/dataOut/LTciMSSA.RDS"))

## 6.2 Save filtered life tables - at birth -------------------------------

selectCols <- c("nyrs", "sex", "raceCode", "year", "ex", "exlow", "exhigh")

ltState_atBirth <- ltState %>% 
  filter(x == 0) %>% 
  select(GEOID, all_of(selectCols))

ltCounty_atBirth <- ltCounty %>% 
  filter(x == 0) %>% 
  select(GEOID, all_of(selectCols))

ltMSSA_atBirth <- ltMSSA %>% 
  filter(x == 0) %>% 
  select(comID, all_of(selectCols))

## save upStream
saveRDS(ltState_atBirth, paste0(ccbUpstream,"/lifeTables/dataOut/e0ciState.RDS"))
saveRDS(ltCounty_atBirth, paste0(ccbUpstream,"/lifeTables/dataOut/e0ciCounty.RDS"))

if (whichDeaths %in% c("real","fake")) { 
  saveRDS(ltMSSA_atBirth, paste0(ccbUpstream,"/lifeTables/dataOut/e0ciMSSA.RDS"))
}

# save myCCB/myData
saveRDS(ltState_atBirth, paste0(ccbData,"e0ciState.RDS"))
saveRDS(ltCounty_atBirth, paste0(ccbData,"e0ciCounty.RDS"))

if (whichDeaths %in% c("real","fake")) { 
  saveRDS(ltMSSA_atBirth, paste0(ccbData,"e0ciMSSA.RDS"))
}


# 7 Data Quality Checks ----------------------------------------------------------------------------------------------------

# TO DO - Convert below to tidyverse syntax

# mx.state[sex=="TOTAL" & CHSI=="TOTAL",.(Nx=sum(Nx),Dx=sum(Dx)),by=c("GEOID","sex","year","CHSI")] # state sum
# lt.state[x==0 & sex=="TOTAL" & CHSI=="TOTAL",c("GEOID","sex","year","CHSI","ex","exlow","exhigh")] 
# mx.county[sex=="TOTAL" & CHSI=="TOTAL",.(Nx=sum(Nx),Dx=sum(Dx)),by=c("nyrs","sex","year","CHSI")] # state sum
# lt.county[x==0 & sex=="TOTAL" & CHSI=="TOTAL" & year==2017,
#           c("nyrs","GEOID","sex","year","CHSI","ex","exlow","exhigh")] 
# mx.mssa[sex=="TOTAL" & CHSI=="TOTAL",.(Nx=sum(Nx),Dx=sum(Dx)),by=c("sex","year","CHSI")] # state sum
# lt.mssa[x==0 & sex=="TOTAL" & CHSI=="TOTAL" & (year %in% c(2010,2017)),
#         c("comID","sex","year","CHSI","ex","exlow","exhigh")] 




# 8	KEY NOTES	----------------------------------------------------------

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

