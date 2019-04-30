rankCause  <- function(myLHJ="Amador",myMeasure = "aRate",myYear=2017,mySex="Total",myLev="lev1",myN=10) {

 myCex <- 1.6
 myCol <- "blue"            #mycol <- rep("blue",nrow(filtered.df))
 bLwd <- 2
  
 
 if(myLev=="lev3") myLev <- c("lev2","lev3")
 
 filtered.df <- filter(datCounty,county==myLHJ,year==myYear,sex==mySex,Level %in% myLev,CAUSE !=0)
 filtered.df <- filtered.df[order( filtered.df[,myMeasure],na.last = FALSE),]

 if (myMeasure=="mean.age"){
  filtered.df <- filtered.df[order( filtered.df[,myMeasure],na.last = NA,decreasing=TRUE),]
  }
   
 Nrows.df          <- nrow(filtered.df)
 Nrows.to.display  <- min(Nrows.df,myN) 
 filtered.df       <- filtered.df[((Nrows.df-Nrows.to.display+1):Nrows.df),]
  
 layout(matrix(c(1,1,1,2,3,4,5),1,7,byrow=TRUE))
 par(mar=c(5,25,0,0),oma = c(3, 0, 3, 0))
 
 t.plot <- barplot( filtered.df$Ndeaths,
                    xlab = "Deaths (n)",  
                    col  = myCol, horiz = TRUE, space = .3, cex.lab = myCex,
                    xlim = c(0,1.04*max(filtered.df$Ndeaths,na.rm=TRUE)))
 # grid(nx=NULL,ny=NA,lty=1)
 # t.plot <- barplot( filtered.df$Ndeaths,add=TRUE,
 #                    xlab = "Deaths (n)",  
 #                    col  = myCol, horiz = TRUE, space = .3, cex.lab = myCex,
 #                    xlim = c(0,1.04*max(filtered.df$Ndeaths,na.rm=TRUE)))

 
 
  box(lwd=bLwd)
 
 t.label <- fullCauseList[match(filtered.df$CAUSE,fullCauseList[,"LABEL"]),"nameOnly"]
 wr.lap  <- wrap.labels(t.label ,30)
 axis(side=2,at=t.plot,labels=wr.lap,las=2,cex.axis=1.8)
 
    
 
 
  
 par(mar=c(5,0,0,0))
 
 
 barplot(filtered.df$aRate,
         xlab="Age-Adjusted Rate",
         col=myCol,horiz=TRUE,space=.3,cex.lab=myCex,
         xlim=c(0,1.04*max(filtered.df$aRate,na.rm=T)))
 # grid(nx=NULL,ny=NA,lty=1)
 # barplot(filtered.df$aRate,add=TRUE,
 #         xlab="Age-Adjusted Rate",
 #         col=myCol,horiz=TRUE,space=.3,cex.lab=myCex,
 #         xlim=c(0,1.04*max(filtered.df$aRate,na.rm=T)))
 box(lwd=bLwd)
 
 
 barplot(filtered.df$YLLper,
         xlab="YLL per 100K pop",
         col=myCol,horiz=TRUE,space=.3,cex.lab=myCex,
         xlim=c(0,1.04*max(filtered.df$YLLper,na.rm=T)))
 # grid(nx=NULL,ny=NA,lty=1)
 # barplot(filtered.df$YLLper,add=TRUE,
 #         xlab="YLL per 100K pop",
 #         col=myCol,horiz=TRUE,space=.3,cex.lab=myCex,
 #         xlim=c(0,1.04*max(filtered.df$YLLper,na.rm=T)))
 box(lwd=bLwd)

 
 
   
 barplot(filtered.df$mean.age, 
         xlab="Mean Age",
         col=myCol,horiz=TRUE,space=.3,cex.lab=myCex,
         xlim=c(0,1.04*max(filtered.df$mean.age,na.rm=T)))
 # grid(nx=NULL,ny=NA,lty=1)
 # barplot(filtered.df$mean.age, add=TRUE,
 #         xlab="Mean Age",
 #         col=myCol,horiz=TRUE,space=.3,cex.lab=myCex,
 #         xlim=c(0,1.04*max(filtered.df$mean.age,na.rm=T)))
 box(lwd=bLwd)
 
 if (myLHJ != "CALIFORNIA") {
  t.plot <- barplot((filtered.df$SMR),
                     xlab="Stnd. Mortaility Ratio",
                     col=myCol,horiz=TRUE,space=.3,cex.lab=myCex,
                     xlim=c(0,1.04*max(filtered.df$SMR,na.rm=T)))
  # grid(nx=NULL,ny=NA,lty=1)
  # t.plot <- barplot((filtered.df$SMR),add=TRUE,
  #                   xlab="Stnd. Mortaility Ratio",
  #                   col=myCol,horiz=TRUE,space=.3,cex.lab=myCex,
  #                   xlim=c(0,1.04*max(filtered.df$SMR,na.rm=T)))
  box(lwd=bLwd)
  abline(v=0.8,col="green")
  abline(v=1,col="gray")
  abline(v=1.2,col="red")
  text(1,.1,"state rate",srt=90,col="black",cex=1.3,adj=c(0,.5))
 }
 
 sexLab <- ""
 if (mySex != "Total") sexLab <- paste0(", among ",mySex,"s")
   
 mtext(paste0("Measures by Cause in ",myYear," in ",myLHJ,sexLab),outer = TRUE,cex=1.6,line=1,font=2)
 mtext(figureAttribution,side=1,outer = TRUE,line=2)
 
 
}


if(1==2){
  myLHJ="Amador"
  myMeasure = "aRate"
  myYear=2017
  mySex="Total"
  myLev="lev1"
  myN=10
}