
if(1==2){
  myLHJ="CALIFORNIA" 
  myCause="D04b"
  myMeasure = "Ndeaths"
  myMeasure = "cDeathRate"
  mySex   = "Total"
  myLogTrans=FALSE
}


trendAge <- function(myLHJ="CALIFORNIA",myCause="A",myLogTrans=FALSE,myMeasure = "cDeathRate") {

  

minYear <- 2000
maxYear <- 2017

dat.1 <- filter(datCounty_AGE_3year,county == myLHJ,CAUSE == myCause, sex=="Total") 

if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those or all data are supressed because of SMALL NUMBERS")

myTit <- paste0("Trend in ",deathMeasuresNames[deathMeasures == myMeasure]," of ",fullCauseList[fullCauseList[,"LABEL"]== myCause,"nameOnly"]," in ",myLHJ," by AGE GROUP, ",minYear," to ",maxYear)
myTit <-  wrap.labels(myTit,myWrapNumber)


yRange     <- chartYearMap$yearGroup3
yMid       <- chartYearMap$midYear3

dat.1$year <- yMid[match(dat.1$yearG3,yRange)]

myTrans    <- ifelse(myLogTrans,'log2','identity')
myMin      <- ifelse(myLogTrans,NA,0)


dat.1 <- mutate(dat.1,ageG = ifelse(ageG == "85 - 999","85+",ageG))

 ggplot(data=dat.1, aes(x=year, y=eval(parse(text=paste0(myMeasure))), group=ageG, color=ageG)) +
    geom_line(size=myLineSize)  + geom_point(shape=myPointShape,size=myPointSize) +
    scale_x_continuous(minor_breaks=yMid,breaks=yMid,expand=c(0,3),labels=yRange) +
      scale_y_continuous(limits = c(myMin, NA),trans=myTrans) + 
    scale_colour_discrete(guide = 'none') +   # removed legend 
    labs(y = myMeasure)  + 
    geom_dl(aes(label = ageG), method = list(dl.trans(x = x + myLineLabelSpace), "last.bumpup", cex=myCex1, font="bold")) +
    geom_dl(aes(label = ageG), method = list(dl.trans(x = x - myLineLabelSpace), "first.bumpup",cex=myCex1, font="bold"))  +
    labs(title =myTit) +
    labs(y = deathMeasuresNames[deathMeasures == myMeasure]) +
     theme_bw() +
    theme(axis.text=element_text(size=myAxisSize),
          axis.title=element_text(size=myAxisSize,face="bold"),
          plot.title=element_text(family='', face='bold', colour='black', size=myTitleSize, color=myTitleColor),
          axis.text.x = element_text(angle = 0,vjust = 0.5, hjust=.5),
          plot.caption = element_text(hjust = 0, face = "italic",size=14)) 
  
 
 
 #theme(axis.text.x = element_text(angle = 60, hjust = 1))
 
 
 }
