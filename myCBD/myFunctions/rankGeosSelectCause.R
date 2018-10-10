rankGeo <- function(myLHJ, myCause=61, myMeasure = "YLL", myYear=2015,mySex="Total", cZoom=FALSE, myCI=TRUE) {

    temp <- paste0("dat.1$",myMeasure)
  
    if (cZoom & myMeasure == "SMR") stop("I appologize dear, but SMR is not calcualted for now below the county level")
    
    causeLab <- causeList36[causeList36[,"LABEL"]==myCause,"nameOnly"]
    sexLab   <- ""
                if (mySex != "Total") sexLab <- paste0(", among ",mySex,"s")
    
     
    if (cZoom)  {dat.1     <- filter(datComm,county==myLHJ,yearG==yearGrp,sex==mySex,CAUSE==myCause, comID != "Unknown") 
                 dat.1     <- dat.1[order(eval(parse(text=temp))),]
                 dat.1$lab <- wrap.labels(dat.1$comName,30)
                 tit       <- paste0("Community Ranking of ",causeLab," in ",myLHJ," in ",yearGrp,sexLab)  }
  
    if (!cZoom) {dat.1     <- filter(datCounty,year==myYear,sex==mySex,CAUSE==myCause,county != "zz California")  
                 dat.1     <- dat.1[order(eval(parse(text=temp))),]
                 dat.1$lab <- dat.1$county
                 tit       <- paste0("County Ranking of ",causeLab," in ",myYear,sexLab) }
  
    #rankdat <- rankdat[(nrow(rankdat)-mytop):nrow(rankdat),]
  
    if (myMeasure == "aRate")  {dat.1 <- dat.1[dat.1$county != "CALIFORNIA",]}
    
    par(mar=par()$mar+c(2,12,-4,0))
    t.plot <- barplot(eval(parse(text=temp)),col="gray",cex.names=.8,horiz=TRUE,space=.3,xlab=names(lMeasures[lMeasures==myMeasure]))
    axis(side=2,at=t.plot,labels=dat.1$lab,las=2,cex.axis=1)
  
    
    if (myCI & myMeasure=="cDeathRate") {arrows(y0=t.plot,x0=dat.1$rateLCI,x1=dat.1$rateUCI,col="blue",length=.05,angle=90,code=3)}
    if (myCI & myMeasure=="YLLper")     {arrows(y0=t.plot,x0=dat.1$YLLrateLCI,x1=dat.1$YLLrateUCI,col="blue",length=.05,angle=90,code=3)}
    if (myCI & myMeasure=="aRate")      {arrows(y0=t.plot,x0=dat.1$aLCI,x1=dat.1$aUCI,col="blue",length=.05,angle=90,code=3)}
    
    
    
    mtext(tit,cex=1.6,line=-2,font=2)
    
    
  }
