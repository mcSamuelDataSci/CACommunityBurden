rankGeo <- function(myLHJ, myCause="A", myMeasure = "YLL", myYear=2015,mySex="Total", myCI=TRUE,myRefLine=FALSE) {

  
    if (1==2){
      myCause="A"
      myMeasure = "Ndeaths"
      myLHJ = STATE
      mySex = "Total"
        }
  
  

    if (myLHJ != STATE) {        cZoom <- TRUE
    } else { cZoom <-FALSE}
    
    
    if (cZoom & myMeasure == "SMR") stop("I appologize dear, but SMR is not calcualted for now below the county level")
    
    causeLab <- fullCauseList[fullCauseList[,"LABEL"]==myCause,"nameOnly"]
    sexLab   <- ""
                if (mySex != "Total") sexLab <- paste0(", among ",mySex,"s")
    
     
    datCounty$plotter <- datCounty[,myMeasure]
    datCounty         <- datCounty %>% filter(year==myYear,sex==mySex,CAUSE==myCause) 
    
    
 
    if (cZoom) { datComm$plotter <- datComm[,myMeasure]
                 dat.1     <- datComm  %>% filter(county==myLHJ,yearG==yearGrp,sex==mySex,CAUSE==myCause, comID != "Unknown")  %>%
                 arrange(!is.na(plotter),plotter) %>%
                 mutate(lab =wrap.labels(comName,30))
    tit       <- paste0("Community Ranking of ",lMeasuresC[lMeasures==myMeasure]," for ",causeLab," in ",myLHJ," in ",yearGrp,sexLab) 
    sMeasure  <- datCounty$plotter[datCounty$county==myLHJ]
    }
    
    
    
    if (!cZoom) { dat.1     <- datCounty  %>%   arrange(!is.na(plotter),plotter) %>%
                                 mutate(lab =county)
                 tit       <- paste0("County Ranking of ",lMeasuresC[lMeasures==myMeasure]," for ",causeLab," in ",myYear,sexLab)
                 sMeasure  <- dat.1$plotter[dat.1$county==STATE]
                 }
  

    tit <-  wrap.labels(tit,80)
    
    
  par(mar=par()$mar+c(2,12,4,0))
  #  par(mar=c(2,12,2,0),oma=c(0,0,0,0))
    
    
  if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those OR all data are suppressed because of small numbers")
  

   
    t.plot <- barplot(dat.1$plotter,col="gray",cex.names=.8,horiz=TRUE,
                      border="black",
                      offset=0,
                      xlab=names(lMeasures[lMeasures==myMeasure]))
    
    grid(nx=NULL,ny=NA)
    # t.plot <- barplot(dat.1$plotter,col="gray",cex.names=.8,horiz=TRUE,add=TRUE,
    #                   border="black",
    #                   offset=0,
    #                   xlab=names(lMeasures[lMeasures==myMeasure]))
    # 
    
    axis(side=2,at=t.plot,labels=dat.1$lab,las=2,cex.axis=1)
   
   
   
    if (myRefLine)  segments(x0=sMeasure,y0=t.plot[1],y1=t.plot[length(t.plot)],lwd=1.5,lty=2)
   
   
    if (myCI & myMeasure=="cDeathRate") {arrows(y0=t.plot,x0=dat.1$rateLCI,x1=dat.1$rateUCI,col="blue",length=.05,angle=90,code=3)}
    if (myCI & myMeasure=="aRate")      {arrows(y0=t.plot,x0=dat.1$aLCI,x1=dat.1$aUCI,col="blue",length=.05,angle=90,code=3)}
   
    axis(side=3)
   
   mtext(tit,cex=1.6,line=3,font=2)
   
    
  }
