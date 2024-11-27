if(1==2) {
server <- F
if (!server) source("g:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
if (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")

testDat <-   mxTract %>%     arrange(agell, .by_group = TRUE) %>%
filter(GEOID == "06001409700")  %>% mutate(junk = deaths/population)

testDat <-   mxTract %>%     arrange(agell, .by_group = TRUE) %>%
  filter(agell==75)  %>% mutate(junk = deaths/population)

x     <- testDat$agell
Nx    <- testDat$population
Dx    <- testDat$deaths
sex   <- testDat$sex
ax    <- NULL
level <- 0.95

####################
# IF mx is large (> 0.2 ?) qx can become > 1.0 and cascade of "errors" can arise, resulting in se being negative
# partly related to large width of age intervals with tract-level work (i.e. 10 years, rather than more typcial 5)
####################


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
      if(sex[1] == "Female") {	 						# female
        if (x[m]==85) ax[m]  <- 7.58
        if (x[m]==90) ax[m]  <- 5.22
        if (x[m]==100) ax[m] <- 2.47
      }
      if(sex[1] == "Male") {	            # male
        if (x[m]==85) ax[m]  <- 6.54
        if (x[m]==90) ax[m]  <- 4.50
        if (x[m]==100) ax[m] <- 2.22
      }
      if(sex[1] == "Total") {							# total
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
 # exlow <- "test"
 # exhigh <- "test"
  
  return(tibble(x = x, n = n, Nx = Nx, Dx = Dx, mx = mx, ax = ax, qx = qx, px = px,
              lx = lx, dx = dx, Lx = Lx, Tx = Tx, ex = ex, sp2 = sp2, wsp2 = wsp2,
              Twsp2 = Twsp2, se2 = se2, exlow = exlow, exhigh = exhigh))
  
 
} # end doLTChiangCI ===========================================================================================



## 5.2 Function that first removes stratas that dpn't meet criteria, then calls the function above ----------------

calcLT <- function(myData, groupByCols = c("GEOID", "year", "nyrs", "sex", "raceCode"),
                   crit = F, critPop = 10000, critDeaths = 700) {
  
  critPop <- ifelse(crit, critPop, 0)
  critDeaths <- ifelse(crit, critDeaths, 0)
  
   myData %>%
    group_by(across({{ groupByCols }})) %>% 
    arrange(agell, .by_group = TRUE) %>%
    mutate(sumPop    = sum(population, na.rm = T),      # Get total population per nyrs-year-GEOID-sex-race strata
           sumDeaths = sum(deaths, na.rm = T)) %>%  # Get total deaths per nyrs-year-GEOID-sex-race strata
    ungroup()   %>% 
    filter(sumPop >= critPop & sumDeaths >= critDeaths) %>% # Removes stratas below deaths and population threshold (defined at the beginning)
    select(-sumPop, -sumDeaths)   %>% 
    group_by(across({{ groupByCols }}))   %>% 
    mutate(doLTChiangCI(agell, Nx=population, Dx=deaths, sex)) %>%  # Call Chiang function defined above; Calculates life expectancy per nyrs-year-GEOID-sex-race strata
    ungroup()
}

