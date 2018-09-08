rankCauseTab  <- function(myLHJ="Alameda",myYear=2015) {

  inDat <- datCounty
  dat.1 <- filter(inDat,county==myLHJ,year==myYear,Level == "gbd36",CAUSE !=0)

  dat.1$causeName <- gbdMap0[match(dat.1$CAUSE,gbdMap0[,1]),"nameOnly"]
  
  dat.1 <- dat.1[,c("causeName","Ndeaths","YLLper","YLL.adj.rate","cDeathRate","aRate")]
  
  dat.1[,3:6] <- round(dat.1[3:6],1)
  
 # mtext(paste("Measures by Cause,",myYear,myLHJ),outer = TRUE,cex=1.3,line=1)

  dat.1
}

