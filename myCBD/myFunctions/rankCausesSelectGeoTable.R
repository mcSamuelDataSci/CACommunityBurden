rankCauseTab  <- function(myLHJ="Alameda",myYear=2015,mySex="Total") {

  inDat <- datCounty
  dat.1 <- filter(inDat,county==myLHJ,year==myYear,sex==mySex,CAUSE !=0)

  dat.1$causeList <- causeList36[match(dat.1$CAUSE,causeList36[,"LABEL"]),"causeList"]
  
  dat.1 <- dat.1[,c("causeList","Ndeaths","cDeathRate","aRate","YLL","YLLper","YLL.adj.rate","SMR")]
  
  dat.1[,3:6] <- round(dat.1[3:6],1)
  
 # mtext(paste("Measures by Cause,",myYear,myLHJ),outer = TRUE,cex=1.3,line=1)

  dat.1
}

