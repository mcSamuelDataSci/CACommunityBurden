if(1==2){
  myLHJ="Amador" 
  myCause="A01"
  myMeasure = "YLL"
  mySex   = "Total"
}

# https://stackoverflow.com/questions/29357612/plot-labels-at-ends-of-lines


trend <- function(myLHJ="CALIFORNIA",myCause="A",myMeasure = "YLL",myYearGrouping="One") {

minYear <- 2000
maxYear <- 2017

myCex <- 1.6
myCol <- "blue"            #mycol <- rep("blue",nrow(dat.1))




if (myYearGrouping == "One") {
  inDat <- datCounty  
  myBreaks <- 2000:2017
  myLabels <- myBreaks
}


if (myYearGrouping == "Three") {
  inDat <- datCounty_3year 
  myLabels   <- c("2000-2002","2003-2005","2006-2008","2009-2011","2012-2014","2015-2017")
  myBreaks   <- c(2001,2004,2007,2010,2013,2016)
  inDat$year <- myBreaks[match(inDat$yearG3,myLabels)]
}
  

dat.1 <- filter(inDat,county == myLHJ,CAUSE == myCause)

if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those or all data are supressed because of SMALL NUMBERS")

myTit <- paste0("Trend in ",deathMeasuresNames[deathMeasures == myMeasure]," of ",fullCauseList[fullCauseList[,"LABEL"]== myCause,"nameOnly"]," in ",myLHJ,", ",minYear," to ",maxYear)

myTit <-  wrap.labels(myTit,80)



mySize1 <- 18
mySize2 <- 20
myCex1  <- 1.8


tplot<-  ggplot(data=dat.1, aes(x=year, y=eval(parse(text=paste0(myMeasure))), group=sex, color=sex)) +
    geom_line(size=2)  +
    scale_x_continuous(minor_breaks=myBreaks,breaks=myBreaks,expand=c(0,3),labels=myLabels) +
    scale_y_continuous(limits = c(0, NA)) +
    scale_colour_discrete(guide = 'none') +   # removed legend
    labs(y = myMeasure)  + 
    geom_dl(aes(label = sex), method = list(dl.trans(x = x + 0.2), "last.points", cex=myCex1, font="bold")) +
    geom_dl(aes(label = sex), method = list(dl.trans(x = x - 0.2), "first.points",cex=myCex1, font="bold"))  +
    labs(title =myTit,size=mySize2) +
    labs(y = deathMeasuresNames[deathMeasures == myMeasure]) +
   theme_bw() +
    theme(axis.text=element_text(size=mySize1),
          axis.title=element_text(size=mySize1,face="bold"),
          plot.title=element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize),
          axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1)) 
  
 tplot
 # ggplotly(tplot)


}
