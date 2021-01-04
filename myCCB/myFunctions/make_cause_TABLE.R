rankCauseTab  <- function(myLHJ="Alameda",myYear=2015,mySex="Total") {
 
  if(1==2){
    myYear <- 2017
    myLHJ  <- "Amador"
    temp <- full_join(datCounty,fullCauseList,by="causeCode")
    temp <- filter(temp,is.na(causeList))
  }
  
  # study this to keep selected values...
  # https://dev.to/awwsmm/reactive-datatables-in-r-with-persistent-filters-l26
  
  
  mySex <- "Total"
  inDat <- datCounty
  
    dat.1 <- filter(inDat,county==myLHJ,year==myYear,sex==mySex,causeCode !=0)

  dat.1$causeList <- fullCauseList[match(dat.1$causeCode,fullCauseList[,"causeCode"]),"causeList"]
  
  dat.1 <- dat.1[,c("causeList","Ndeaths","cDeathRate","aRate","aLCI","aUCI","YLL","YLLper","YLL.adj.rate","SMR")]
  
  dat.1[,3:7] <- round(dat.1[3:7],1)
  
   names(dat.1) <- c("Condition","Number of deaths","Crude Death Rate","Age-Adjusted Death Rate (AADR)","Lower 95% CI AADR","Upper 95% CI AADR","Years of Life Lost (YLL)","YLL per 100,000 population","Age-Adjusted YLL Rate","Standard Mortality Ratio")
  
  dat.1
  
}

