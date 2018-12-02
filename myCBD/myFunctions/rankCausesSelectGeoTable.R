rankCauseTab  <- function(myLHJ="Alameda",myYear=2015,mySex="Total") {
 
  if(1==2){
    myYear <- 2017
    myLHJ  <- "CALIFORNIA"
    temp <- full_join(datCounty,fullCauseList,by=c("CAUSE"="LABEL"))
    temp <- filter(temp,is.na(causeList))
  }
  
  
  
  mySex <- "Total"
  inDat <- datCounty
  
  
  dat.1 <- filter(inDat,county==myLHJ,year==myYear,sex==mySex,CAUSE !=0)

  dat.1$causeList <- fullCauseList[match(dat.1$CAUSE,fullCauseList[,"LABEL"]),"causeList"]
  
  dat.1 <- dat.1[,c("causeList","Ndeaths","cDeathRate","aRate","YLL","YLLper","YLL.adj.rate","SMR")]
  
  dat.1[,3:7] <- round(dat.1[3:7],1)
  
 # mtext(paste("Measures by Cause,",myYear,myLHJ),outer = TRUE,cex=1.3,line=1)

  names(dat.1) <- c("Condition","Number of deaths","Crude Death Rate","Age-Adjusted Death Rate","Years of Life Lost (YLL)","YLL per 100,000 population","Age-Adjusted YLL Rate","Standard Mortality Ratio")
  
  dat.1
  
}

