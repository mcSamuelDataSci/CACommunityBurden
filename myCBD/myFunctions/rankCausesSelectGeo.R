rankCause  <- function(myLHJ="Alameda", myMeasure = "YLL",myYear=2015,myN=10) {

  myCex <- 1.6
  myCol <- "blue"            #mycol <- rep("blue",nrow(dat.1))

  
  inDat <- datCounty
  dat.1 <- filter(inDat,county==myLHJ,year==myYear,Level == "gbd36",CAUSE !=0)
  
  #  if (myMeasure == "YLL")        dat.1 <- dat.1[order(dat.1$YLL),]
  #  if (myMeasure == "m.YLL")      dat.1 <- dat.1[order(dat.1$m.YLL),]
  #  if (myMeasure == "Ndeaths")    dat.1 <- dat.1[order(dat.1$Ndeaths),]
  #  if (myMeasure == "excessRisk") dat.1 <- dat.1[order(abs(dat.1$excessRisk),na.last=FALSE),]
  #  if (myMeasure == "cDeathRate") dat.1 <- dat.1[order(dat.1$cDeathRate),]
  #  if (myMeasure == "med.age")    dat.1 <- dat.1[order(dat.1$med.age),]
    
  
   dat.1 <- dat.1[order( eval(parse(text=paste0("dat.1$",myMeasure)))),]
 
  nR    <- nrow(dat.1)
  myNX  <- min(nR,myN) 
  dat.1 <- dat.1[((nR-myNX):nR),]
  
  
  layout(matrix(c(1,1,2,3,4),1,5,byrow=TRUE))
  
  bLwd <- 2
  
  par(mar=c(5,13,0,0),oma = c(0, 0, 3, 0))
  
   t.plot <- barplot((dat.1$YLLper),xlab="YLL per 100K pop",col=myCol,horiz=TRUE,space=.3,cex.lab=myCex,xlim=c(0,1.04*max(dat.1$YLLper))); box(lwd=bLwd)
 
   t.label <- gbdMap0[match(dat.1$CAUSE,gbdMap0[,1]),"nameOnly"] 
   wr.lap <- wrap.labels(t.label ,18)
   
   axis(side=2,at=t.plot,labels=wr.lap,las=2,cex.axis=1.6)
   box(lwd=2)
     
 par(mar=c(5,0,0,0))
   
   t.plot <- barplot((dat.1$m.YLL), xlab="Mean YLL",        col=myCol,horiz=TRUE,space=.3,cex.lab=myCex,xlim=c(0,1.04*max(dat.1$m.YLL)));   box(lwd=bLwd)
   t.plot <- barplot((dat.1$Ndeaths),xlab="Deaths (n)",     col=myCol,horiz=TRUE,space=.3,cex.lab=myCex,xlim=c(0,1.04*max(dat.1$Ndeaths))); box(lwd=bLwd)
 
  if (myLHJ != "CALIFORNIA STATE") {
   t.plot <- barplot((dat.1$SMR),xlab="SMR",                col=myCol,horiz=TRUE,space=.3,cex.lab=myCex,xlim=c(0,1.04*max(dat.1$SMR)));     box(lwd=bLwd)
   abline(v=0.8,col="green"); abline(v=1,col="gray"); abline(v=1.2,col="red")
   text(1,0,"state rate",srt=90,col="black",cex=1.2,adj=c(0,.5))
  
 
  }
 
  mtext(paste("Measures by Cause,",myYear,myLHJ),outer = TRUE,cex=1.3,line=1)

}




# ADD TO OUR R FUNCTION LIST na.omit
#  junk <-  na.omit(gbdMap0$GBDA[gbdMap0$linkL1==myL1List] )


