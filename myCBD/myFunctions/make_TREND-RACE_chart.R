
if(1==2){
  myLHJ="CALIFORNIA" 
  myCause="0"
  myMeasure = "aRate"
  mySex   = "Total"
}

trendRace <- function(myLHJ="CALIFORNIA",myCause="A",myMeasure = "YLL",myLogTrans=FALSE) {

minYear <- 2000
maxYear <- 2017

myCex <- 1.6
myCol <- "blue"            #mycol <- rep("blue",nrow(dat.1))

dat.1 <- filter(datCounty.RE,county == myLHJ,CAUSE == myCause, sex=="Total") %>%
         mutate(raceName = raceNameFull[match(raceCode,raceCodeFull)] )

#,raceCode != "Multi-NH"

if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those or all data are supressed because of SMALL NUMBERS")

myTit <- paste0("Trend in ",lMeasuresC[lMeasures==myMeasure]," of ",fullCauseList[fullCauseList[,"LABEL"]== myCause,"nameOnly"]," in ",myLHJ," by RACE/ETHNIC Group*, ",minYear," to ",maxYear)
myTit <-  wrap.labels(myTit,80)

mySize1 <- 18
mySize2 <- 20
myCex1  <- 1.5

yRange     <- c("2000-2002","2003-2005","2006-2008","2009-2011","2012-2014","2015-2017")
yMid       <- c(2001,2004,2007,2010,2013,2016)
dat.1$year <- yMid[match(dat.1$yearG3,yRange)]

myTrans    <- ifelse(myLogTrans,'log2','identity')
myMin      <- ifelse(myLogTrans,NA,0)

 ggplot(data=dat.1, aes(x=year, y=eval(parse(text=paste0(myMeasure))), group=raceCode, color=raceCode)) +
    geom_line(size=2)  + geom_point() +
    scale_x_continuous(minor_breaks=yMid,breaks=yMid,expand=c(0,3),labels=yRange) +
    # scale_x_continuous(minor_breaks=yMid,breaks=yMid,labels=yRange) +
    #   expand_limits(x = c(0, .2)) +
    scale_y_continuous(limits = c(myMin, NA),trans=myTrans) + 
   #,,limits = c(1, NA) trans=myTrans
    scale_colour_discrete(guide = 'none') +   # removed legend 
    labs(y = myMeasure)  + 
    geom_dl(aes(label = raceName), method = list(dl.trans(x = x + 0.2), "last.points", cex=myCex1, font="bold")) +
    geom_dl(aes(label = raceName), method = list(dl.trans(x = x - 0.2), "first.points",cex=myCex1, font="bold"))  +
    labs(title =myTit,size=mySize2) +
    labs(y = lMeasuresC[lMeasures==myMeasure]) +
   labs(caption = raceNote) +
   theme_bw() +
    theme(axis.text=element_text(size=mySize1),
          axis.title=element_text(size=mySize1,face="bold"),
          plot.title=element_text(family='', face='bold', colour='black', size=mySize2),
          axis.text.x = element_text(angle = 0,vjust = 0.5, hjust=.5),
          plot.caption = element_text(hjust = 0, face = "italic",size=14)) 
  
 
 
 #theme(axis.text.x = element_text(angle = 60, hjust = 1))
 
 
 }
