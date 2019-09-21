
if(1==2){
  myLHJ="CALIFORNIA" 
  myCause="0"
  myMeasure = "aRate"
  mySex   = "Total"
  myLogTrans=FALSE
  myMultiRace = FALSE
}

trendRace <- function(myLHJ="CALIFORNIA",myCause="A",myMeasure = "YLL",myLogTrans=FALSE, myMultiRace = FALSE) {

minYear <- 2000
maxYear <- 2017

dat.1 <- filter(datCounty_RE,county == myLHJ,CAUSE == myCause, sex=="Total") %>%
         mutate(raceName = raceNameFull[match(raceCode,raceCodeFull)] )

if (!myMultiRace) dat.1 <- filter(dat.1,raceCode != "Multi-NH")

if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those or all data are supressed because of SMALL NUMBERS")

myTit <- paste0("Trend in ",deathMeasuresNames[deathMeasures == myMeasure]," of ",fullCauseList[fullCauseList[,"LABEL"]== myCause,"nameOnly"]," in ",myLHJ," by RACE/ETHNIC Group*, ",minYear," to ",maxYear)
myTit <-  wrap.labels(myTit,myWrapNumber)


yRange     <- chartYearMap$yearGroup3
yMid       <- chartYearMap$midYear3

dat.1$year <- yMid[match(dat.1$yearG3,yRange)]

myTrans    <- ifelse(myLogTrans,'log2','identity')
myMin      <- ifelse(myLogTrans,NA,0)

 ggplot(data=dat.1, aes(x=year, y=eval(parse(text=paste0(myMeasure))), group=raceCode, color=raceCode)) +
    geom_line(size=myLineSize)  + geom_point(shape=myPointShape, size=myPointSize) +
    scale_x_continuous(minor_breaks=yMid,breaks=yMid,expand=c(0,4),labels=yRange) +
       # scale_x_continuous(minor_breaks=yMid,breaks=yMid,labels=yRange) +
    #   expand_limits(x = c(0, .2)) +
    scale_y_continuous(limits = c(myMin, NA),trans=myTrans) + 
   #,,limits = c(1, NA) trans=myTrans
    scale_colour_discrete(guide = 'none') +   # removed legend 
    labs(y = myMeasure)  + 
    geom_dl(aes(label = raceName), method = list(dl.trans(x = x + 0.3), "last.bumpup", cex=myCex1, font="bold")) +
    geom_dl(aes(label = raceName), method = list(dl.trans(x = x - 0.2), "first.bumpup",cex=myCex1, font="bold"))  +
    labs(title =myTit,size=myTitleSize) +
    labs(y = deathMeasuresNames[deathMeasures == myMeasure]) +
   labs(caption = raceNote) +
   theme_bw() +
    theme(axis.text=element_text(size=myAxisSize),
          axis.title=element_text(size=myAxisSize,face="bold"),
          plot.title=element_text(family='', face='bold', colour='black', size=myTitleSize, color=myTitleColor),
          axis.text.x = element_text(angle = 0,vjust = 0.5, hjust=.5),
          plot.caption = element_text(hjust = 0, face = "italic",size=14)) 
  
 
 
 #theme(axis.text.x = element_text(angle = 60, hjust = 1))
 
 
 }
