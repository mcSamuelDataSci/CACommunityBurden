trend <- function(myLHJ="zz California",myCause=61,myMeasure = "YLL",mySex="Total") {

  minYear <- 2002
  maxYear <- 2015
  
  myCex <- 1.6
  myCol <- "blue"            #mycol <- rep("blue",nrow(dat.1))

  inDat <- datCounty
  dat.1 <- filter(inDat,county==myLHJ,(year >= minYear & year <= maxYear ),CAUSE == myCause,sex==mySex)
  
  if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those; could be some other error")
  
  plot(dat.1$year,eval(parse(text=paste0("dat.1$",myMeasure))),
       type="l",lwd=3,col="blue",
       xlab="Year",ylab=names(lMeasures[lMeasures==myMeasure]),
       main=paste("Trend in",names(lMeasures[lMeasures==myMeasure]),"of",causeList36[causeList36[,1]==myCause,2],"in",myLHJ)) 
  
  
}