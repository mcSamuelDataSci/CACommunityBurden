calculateLE  <- function(inDat) {
   
    attach(inDat)
    
    level=0.95
  
    m    <- nrow(inDat)    
    mx   <- Dx/Nx                 # mortality rate   
    n    <- lead(agell) - agell   # n years between age groups
 
 
    # calculate ax
    # if(is.null(ax)) {						# if no ax values provided, use hardcoded CA 2010-17 by sex.
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
          
          # CHANGED x to agell below
          
          if (agell[m]==85) ax[m]<-7.58
          if (agell[m]==90) ax[m]<-5.22
          if (agell[m]==100) ax[m]<-2.47
        }
        if(!grepl("F",sex[1]) & !grepl("T",sex[1])) {	# male
          if (agell[m]==85) ax[m]<-6.54
          if (agell[m]==90) ax[m]<-4.50
          if (agell[m]==100) ax[m]<-2.22
        }
        if(grepl("T",sex[1])) {							# total
          if (agell[m]==85) ax[m]<-7.19
          if (agell[m]==90) ax[m]<-4.97
          if (agell[m]==100) ax[m]<-2.42
        }
      }
 #   }   
    
  # Chiang standard elements
  qx                            <- n*mx / (1+(n-ax)*mx)		  # probablity of death (from mortality rate)
  qx[m]                         <- 1	               				# 100% at oldest age group
  px                            <- 1-qx							      	# pr(survival)
  lx                            <- cumprod(c(1,px))*100000	# 100,000 for radix
  dx                            <- -diff(lx)						    # deaths each age interval
  Lx                            <- n*lx[-1] + ax*dx 				# PY lived in this age group
  lx                          	<- lx[-(m+1)] 						  # survivors
  Lx[m]                         <- lx[m]/mx[m] 					    # PY lived in final age group
  Lx[is.na(Lx)|is.infinite(Lx)] <- 0		                    # in case of NA or Inf values from poorly formed LTs
  Tx                            <- rev(cumsum(rev(Lx)))			# cumulative PY lived at this age and above
  ex                            <- Tx/lx 						       	# life expectancy at this age

  
    # Chiang CI elements
  zcrit            <- 1-((1-level)/2) 								             	# CI from normal distribution
  sp2              <- ((qx^2)*(1-qx))/Dx								          	# variance of survival probability
  sp2[is.na(sp2)]  <- 0 									                         	# fix zero deaths case
  sp2[m]           <- 4/Dx[m]/mx[m]^2								               	# adjustment final age interval
  wsp2             <- lx^2*((1-(ax/n))*n+c(tail(ex,-1),NA))^2*sp2		# weighted SP2 
  wsp2[m]          <- (lx[m]/2)^2*sp2[m]								            # adjustment final age interval
  Twsp2            <- rev(cumsum(rev(wsp2)))					          		# sum of weighted sp2 rows below (like Tx)
  se2              <- Twsp2/lx^2 									                	# sample variance of e0
  exlow            <- ex-qnorm(zcrit)*sqrt(se2)					          	# CI low
  exhigh           <- ex+qnorm(zcrit)*sqrt(se2)						          # CI high 
  
  LE0 <- bind_cols(mx = mx, 
                   ax = ax, 
                   ex = ex, 
                   exlow = exlow, 
                   exhigh = exhigh)
  
  bind_cols(inDat,LE0)
  
}
  
  
  
  