rankGeo <- function(myLHJ, myCause=61, myMeasure = "YLL", myYear=2015,mySex="Total", cZoom=FALSE, myCI=TRUE) {

    temp <- paste0("dat.1$",myMeasure)
  
    if (cZoom & myMeasure == "SMR") stop("I appologize dear, but SMR is not calcualted for now below the county level")
    
    
    
    
    if (cZoom) {dat.1    <- filter(datComm,county==myLHJ,yearG==yG,sex==mySex,CAUSE==myCause, comID != "Unknown") 
             dat.1    <- dat.1[order(eval(parse(text=temp))),]
                      dat.1$lab <- wrap.labels(dat.1$comName,30)
              tit <- paste("Community Ranking of",causeList36[causeList36[,1]==myCause,2],"in",myLHJ,"in",yG)  }
  
    if (!cZoom) {dat.1    <- filter(datCounty,year==myYear,sex==mySex,CAUSE==myCause,county != "zz California")  
    dat.1    <- dat.1[order(eval(parse(text=temp))),]
              dat.1$lab <- dat.1$county
              tit <- paste("County Ranking of",causeList36[causeList36[,1]==myCause,2],"in",myYear) }
  
    #rankdat <- rankdat[(nrow(rankdat)-mytop):nrow(rankdat),]
    
    
    if (myMeasure == "aRate")  {dat.1 <- dat.1[dat.1$county != "CALIFORNIA",]}
    
    
    
    par(mar=par()$mar+c(2,12,0,0))
    t.plot <- barplot(eval(parse(text=temp)),col="gray",cex.names=.8,horiz=TRUE,space=.3,xlab=names(lMeasures[lMeasures==myMeasure]))
    axis(side=2,at=t.plot,labels=dat.1$lab,las=2,cex.axis=.8)
  
    
    if (myCI & myMeasure=="cDeathRate") {arrows(y0=t.plot,x0=dat.1$rateLCI,x1=dat.1$rateUCI,col="blue",length=.05,angle=90,code=3)}
    
    title(tit)
    
  }
