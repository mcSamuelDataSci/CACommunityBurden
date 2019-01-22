rankCauseTab  <- function(myLHJ="Alameda",myYear=2015,mySex="Total") {
 
  if(1==2){
    myYear <- 2017
    myLHJ  <- "Amador"
    temp <- full_join(datCounty,fullCauseList,by=c("CAUSE"="LABEL"))
    temp <- filter(temp,is.na(causeList))
  }
  
  # study this to keep selected values...
  # https://dev.to/awwsmm/reactive-datatables-in-r-with-persistent-filters-l26
  
  
  mySex <- "Total"
  inDat <- datCounty
  
    dat.1 <- filter(inDat,county==myLHJ,year==myYear,sex==mySex,CAUSE !=0)

  dat.1$causeList <- fullCauseList[match(dat.1$CAUSE,fullCauseList[,"LABEL"]),"causeList"]
  
  dat.1 <- dat.1[,c("causeList","Ndeaths","cDeathRate","aRate","YLL","YLLper","YLL.adj.rate","SMR")]
  
  dat.1[,3:7] <- round(dat.1[3:7],1)
  
  
  # NEW!!!!
  #dat.1$Ndeaths[dat.1$Ndeaths==0] <- NA
  #dat.1$Ndeaths[is.na(dat.1$Ndeaths)] <- "*"
  
   # mtext(paste("Measures by Cause,",myYear,myLHJ),outer = TRUE,cex=1.3,line=1)

  names(dat.1) <- c("Condition","Number of deaths","Crude Death Rate","Age-Adjusted Death Rate","Years of Life Lost (YLL)","YLL per 100,000 population","Age-Adjusted YLL Rate","Standard Mortality Ratio")
  
  dat.1
  
}

