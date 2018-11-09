rankCause  <- function(myLHJ="CALIFORNIA",myMeasure = "mean.age",myYear=2017,mySex="Total",myLev="lev1",myN=10) {

  if(1==2){
  myLHJ="CALIFORNIA"
  myMeasure = "mean.age"
  myYear=2017
  mySex="Total"
  myLev="lev2"
  myN=3
  }
  
  
  #high resolution for images?
  #svg("filename.svg")  #,width=14,height=7
  #plot....
  #dev.off()
  
  
  myCex <- 1.6
  myCol <- "blue"            #mycol <- rep("blue",nrow(dat.1))

  
 # levelVec <- c("lev1")
  
  
  inDat <- datCounty
  dat.1 <- filter(inDat,county==myLHJ,year==myYear,sex==mySex,Level %in% myLev,CAUSE !=0)
 
  dat.1 <- dat.1[order( dat.1[,myMeasure]),]
  #dat.1 <- dat.1[order( eval(parse(text=paste0("dat.1$",myMeasure)))),]

   if (myMeasure=="mean.age"){
     dat.1 <- dat.1[order( dat.1[,myMeasure],decreasing=TRUE),]}
   
  nR    <- nrow(dat.1)
  myNX  <- min(nR,myN) 
  dat.1 <- dat.1[((nR-myNX+1):nR),]
  
  layout(matrix(c(1,1,2,3,4,5),1,6,byrow=TRUE))
  
  bLwd <- 2
  
  par(mar=c(5,13,0,0),oma = c(0, 0, 3, 0))
   t.plot <- barplot((dat.1$Ndeaths),xlab="Deaths (n)",  
                     col=myCol,horiz=TRUE,space=.3,cex.lab=myCex,
                     xlim=c(0,1.04*max(dat.1$Ndeaths))); box(lwd=bLwd)
 
   t.label <- causeList36[match(dat.1$CAUSE,causeList36[,"LABEL"]),"nameOnly"]
 
   wr.lap <- wrap.labels(t.label ,18)
   
   axis(side=2,at=t.plot,labels=wr.lap,las=2,cex.axis=1.6)
   box(lwd=2)
     
 par(mar=c(5,0,0,0))
   t.plot <- barplot((dat.1$YLLper),xlab="YLL per 100K pop",col=myCol,
                     horiz=TRUE,space=.3,cex.lab=myCex,xlim=c(0,1.04*max(dat.1$YLLper))); box(lwd=bLwd)
   t.plot <- barplot((dat.1$aRate),xlab="Age-Adjusted Rate",col=myCol,horiz=TRUE,space=.3,cex.lab=myCex,xlim=c(0,1.04*max(dat.1$aRate))); box(lwd=bLwd)
   
   t.plot <- barplot((dat.1$mean.age), xlab="Mean Age",        col=myCol,horiz=TRUE,space=.3,cex.lab=myCex,xlim=c(0,1.04*max(dat.1$mean.age)));   box(lwd=bLwd)
 
  if (myLHJ != "CALIFORNIA") {
   t.plot <- barplot((dat.1$SMR),xlab="Stnd. Mortaility Ratio",                col=myCol,horiz=TRUE,space=.3,cex.lab=myCex,xlim=c(0,1.04*max(dat.1$SMR)));     box(lwd=bLwd)

   abline(v=0.8,col="green"); abline(v=1,col="gray"); abline(v=1.2,col="red")
   text(1,.1,"state rate",srt=90,col="black",cex=1.3,adj=c(0,.5))
  
 
  }
 
 sexLab <- ""
 if (mySex != "Total") sexLab <- paste0(", among ",mySex,"s")
   
   
  mtext(paste0("Measures by Cause in ",myYear," in ",myLHJ,sexLab),outer = TRUE,cex=1.6,line=1,font=2)

}




# ADD TO OUR R FUNCTION LIST na.omit
#  junk <-  na.omit(gbdMap0$GBDA[gbdMap0$linkL1==myL1List] )


