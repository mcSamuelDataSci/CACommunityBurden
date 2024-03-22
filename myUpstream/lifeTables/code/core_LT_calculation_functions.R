if(1==2) {
server <- F
if (!server) source("g:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
if (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
  

stateDat <- readRDS(paste0(ccbUpstream,"lifeTables/dataMid/mxState.RDS"))
mssaDat  <- readRDS(paste0(ccbUpstream,"lifeTables/dataMid/mxMSSA.RDS"))

testDat  <- stateDat %>% filter(sex == "Total", raceCode == "Total", year == 2020, nyrs == 1)
testDat2 <- stateDat %>% filter(                raceCode == "Total", year == 2020, nyrs == 1)
testDat3 <- stateDat %>% filter(                raceCode == "Total", year == 2020)


tractTest <- readRDS(paste0(ccbData,"real/datTract.RDS")) %>% filter(causeCode == "0", yearG5 == "2018-2022", sex == "Total")




}





if(1==2){
x     <- testDat$agell
Nx    <- testDat$Nx
Dx    <- testDat$Dx
sex   <- testDat$sex
ax    <- NULL
level <- 0.95
}


# 5	Calculating Life Expectancy ----------------------------------------------------------------------------------------------------------

## 5.1	Generic function to calculate a life table ---------------------------

## 		x is a vector of age groups, nx is the corresponding vector of pop, dx of deaths
##		sex is M or MALE or F or FEMALE (used to calc ax); ax is an optional vector of ax values
##		previously estimated ax values are available from the UN WPP, USMDB, NCHS, including by race. 
##		values used here are from USMDB CA 1x10 or 5x10 (2010-17) by sex.
##		also exports LTCI from Chiang's method with adjusted final age group
## - D. Eayres and E.S. Williams. 2004. "Evaluation of methodologies for small area life 
##   expectancy estimation". J Epi Com Health 58(3). http://dx.doi.org/10.1136/jech.2003.009654.


doLTChiangCI <- function(x, Nx, Dx, sex, ax=NULL, level=0.95) {   #======================================

  m   <- length(x)			# get position of final age group by length of age vector
  mx  <- Dx/Nx				  # mortality rate
  n   <- c(diff(x), NA)	# n years between age groups

  # calculate ax ----------------------------------------------------------------------------------------
 
  if(is.null(ax)) {					# if no ax values provided, use hardcoded CA 2010-17 by sex.
   
    ax <- n/2						    # rule of thumb: 1/2 age interval
    
    # infant ages  					# from USMDB CA 5x10 life tables.
    if (n[1]==1) ax[1] <- 0.06 
    if (n[1]==5) ax[1] <- 0.44 
    if (n[2]==4) ax[2] <- 1.64
    
    # final age interval
    ax[m] <- 1 / mx[m]				# rule of thumb: inverse of mx in final age interval
  
      if (is.na(ax[m])) {				# if cannot calculate, e.g. because mx==0
      if(sex[1] == "F") {	 						# female
        if (x[m]==85) ax[m]  <- 7.58
        if (x[m]==90) ax[m]  <- 5.22
        if (x[m]==100) ax[m] <- 2.47
      }
      if(sex[1] == "M") {	            # male
        if (x[m]==85) ax[m]  <- 6.54
        if (x[m]==90) ax[m]  <- 4.50
        if (x[m]==100) ax[m] <- 2.22
      }
      if(sex[1] == "T") {							# total
        if (x[m]==85) ax[m]  <- 7.19
        if (x[m]==90) ax[m]  <- 4.97
        if (x[m]==100) ax[m] <- 2.42
      }
    }
  }
  # end calculate ax ----------------------------------------------------------------------------------------

  # Chiang standard elements
  qx    <- n*mx / (1+(n-ax)*mx)				# probablity of death (from mortality rate)
  qx[m] <- 1							          	# 100% at oldest age group
  px    <- 1-qx							        	# pr(survival)
  lx    <- cumprod(c(1,px))*100000		# 100,000 for radix
  dx    <- -diff(lx)						      # deaths each age interval
  Lx    <- n*lx[-1] + ax*dx 				  # PY lived in this age group
  lx 	  <- lx[-(m+1)] 						    # survivors
  Lx[m] <- lx[m]/mx[m] 					      # PY lived in final age group
  Lx[is.na(Lx)|is.infinite(Lx)] <- 0	# in case of NA or Inf values from poorly formed LTs
  Tx    <- rev(cumsum(rev(Lx)))				# cumulative PY lived at this age and above
  ex    <- Tx/lx 							        # life expectancy at this age
  # Chiang CI elements
  zcrit            <- 1-((1-level)/2) 					# CI from normal distribution
  sp2              <- ((qx^2)*(1-qx))/Dx				# variance of survival probability
  sp2[is.na(sp2)]  <- 0 										    # fix zero deaths case
  sp2[m]           <- 4/Dx[m]/mx[m]^2						# adjustment final age interval
  wsp2             <- lx^2*((1-(ax/n))*n+c(tail(ex,-1),NA))^2*sp2		# weighted SP2 
  wsp2[m]          <- (lx[m]/2)^2*sp2[m]				# adjustment final age interval
  Twsp2            <- rev(cumsum(rev(wsp2)))		# sum of weighted sp2 rows below (like Tx)
  se2              <- Twsp2/lx^2 								# sample variance of e0
  exlow            <- ex-qnorm(zcrit)*sqrt(se2)	# CI low
  exhigh           <- ex+qnorm(zcrit)*sqrt(se2)	# CI high
  
  return(tibble(x = x, n = n, Nx = Nx, Dx = Dx, mx = mx, ax = ax, qx = qx, px = px,
              lx = lx, dx = dx, Lx = Lx, Tx = Tx, ex = ex, sp2 = sp2, wsp2 = wsp2,
              Twsp2 = Twsp2, se2 = se2, exlow = exlow, exhigh = exhigh))
  
} # end doLTChiangCI ===========================================================================================



## 5.2 Function that first removes stratas that dpn't meet criteria, then calls the function above ----------------

calcLT <- function(myData, groupByCols = c("GEOID", "year", "nyrs", "sex", "raceCode") ) {
  
  myData %>% 
    group_by(across({{ groupByCols }})) %>% 
    mutate(sumNx = sum(Nx, na.rm = T),      # Get total population per nyrs-year-GEOID-sex-race strata
           sumDx = sum(Dx, na.rm = T)) %>%  # Get total deaths per nyrs-year-GEOID-sex-race strata
    ungroup() %>% 
    filter(sumNx >= critNx & sumDx >= critDx) %>% # Removes stratas below deaths and population threshold (defined at the beginning)
    select(-sumNx, -sumDx) %>% 
    group_by(across({{ groupByCols }})) %>% 
    mutate(doLTChiangCI(agell, Nx, Dx, Sex)) %>%  # Call Chiang function defined above; Calculates life expectancy per nyrs-year-GEOID-sex-race strata
    ungroup()
}


critNx      <- 10000 # ADD COMMENT
critDx      <-   700 # ADD COMMENT


groupByCols = c("GEOID", "year", "nyrs", "sex", "raceCode")
groupByCols = c("sex","nyrs")
test3LT <- calcLT(testDat3, groupByCols = c("sex","nyrs"))



mssaDat0 <- mssaDat %>% filter(year == 2020)

ltMSSA <- calcLT(myData = mssaDat0) %>%
                   filter(x == 0) %>% 
                   select(GEOID, c("nyrs", "sex", "raceCode", "year", "ex", "exlow", "exhigh"))





## 5.3 Calculate life expectancy ----------------------------------------

ltState  <- calcLT(myData = stateDat) 
ltCounty <- calcLT(myData = mxCounty) 
ltMSSA   <- calcLT(myData = mxMSSA) %>%   rename(comID = GEOID)


# 6 Save data -------------------------------------------------------------------------------------------------------------------

## 6.1 Save full life tables ---------------------------------------------

saveRDS(ltState, paste0(ccbUpstream, "/lifeTables/dataOut/LTciState.RDS"))
saveRDS(ltCounty, paste0(ccbUpstream, "/lifeTables/dataOut/LTciCounty.RDS"))
saveRDS(ltMSSA, paste0(ccbUpstream, "/lifeTables/dataOut/LTciMSSA.RDS"))

## 6.2 Save filtered life tables - at birth -------------------------------

selectCols <- c("nyrs", "sex", "raceCode", "year", "ex", "exlow", "exhigh")

ltState_atBirth  <- ltState  %>%  filter(x == 0) %>%  select(GEOID, all_of(selectCols))
ltCounty_atBirth <- ltCounty %>%  filter(x == 0) %>%  select(GEOID, all_of(selectCols))
ltMSSA_atBirth   <- ltMSSA   %>%  filter(x == 0) %>%  select(comID, all_of(selectCols))

## save upStream
saveRDS(ltState_atBirth, paste0(ccbUpstream,"/lifeTables/dataOut/e0ciState.RDS"))
saveRDS(ltCounty_atBirth, paste0(ccbUpstream,"/lifeTables/dataOut/e0ciCounty.RDS"))
saveRDS(ltMSSA_atBirth, paste0(ccbUpstream,"/lifeTables/dataOut/e0ciMSSA.RDS"))


# save myCCB/myData
saveRDS(ltState_atBirth, paste0(ccbData,"e0ciState.RDS"))
saveRDS(ltCounty_atBirth, paste0(ccbData,"e0ciCounty.RDS"))
saveRDS(ltMSSA_atBirth, paste0(ccbData,"e0ciMSSA.RDS"))


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

