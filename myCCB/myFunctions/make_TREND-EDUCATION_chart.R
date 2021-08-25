
if(1==2){
  myLHJ="Contra Costa" 
  myLHJ="CALIFORNIA"
  myCause="0"
  myMeasure = "cDeathRate"
  myMeasure = "aRate"
  mySex   = "Total"
  myLogTrans=FALSE
}


trendEducation <- function(myLHJ="CALIFORNIA",myCause="A",mySex,myMeasure = "cDeathRate",myLogTrans=FALSE) {

minYear <- 2012

dat.1 <- filter(datCounty_EDU,county == myLHJ,causeCode == myCause, sex==mySex) 


if (nrow(dat.1)==0) stop("Sorry friend, data are suppressed per the California Health and Human Services Agency Data De-Identification Guidelines, or there are no cases that meet this criteria.")

myTit <- paste0("Trend in ",deathMeasuresNames[deathMeasures == myMeasure]," of ",deathCauseLink$causeName[deathCauseLink$causeCode== myCause]," in ",myLHJ," by EDUCATION Group, ",minYear," to ",maxYear,", ",mySex,", >24 years-old only, (crude age-adjustement)")
myTit <-  wrap.labels(myTit,80)


yRange     <- minYear:maxYear
yMid       <- minYear:maxYear


myTrans    <- ifelse(myLogTrans,'log2','identity')
myMin      <- ifelse(myLogTrans,NA,0)

 myPlot <- ggplot(data=dat.1, aes(x=year, y=eval(parse(text=paste0(myMeasure))), group=eduName, color=eduName))  +
   geom_line(size=myLineSize)  +
   geom_point(shape = myPointShape,size=myPointSize)  +
       scale_x_continuous(minor_breaks=yMid,breaks=yMid,expand=c(0,3),labels=yRange) +
    # scale_x_continuous(minor_breaks=yMid,breaks=yMid,labels=yRange) +
    #   expand_limits(x = c(0, .2)) +
    scale_y_continuous(limits = c(myMin, NA),trans=myTrans) + 
   #,,limits = c(1, NA) trans=myTrans
    scale_colour_discrete(guide = 'none') +   # removed legend 
    labs(y = myMeasure)  + 
    geom_dl(aes(label = eduName), method = list(dl.trans(x = x + 0.2), "last.points", cex=myCex2, font="bold")) +
    geom_dl(aes(label = eduName), method = list(dl.trans(x = x - 0.2), "first.points",cex=myCex2, font="bold"))  +
    labs(title =myTit,size=myTitleSize) +
    labs(y = deathMeasuresNames[deathMeasures == myMeasure]) +
    theme_bw() +
    theme(axis.text=element_text(size=myAxisSize),
          axis.title=element_text(size=myAxisSize,face="bold"),
          plot.title=element_text(family='', face='bold', colour='black', size=myTitleSize, color=myTitleColor),
          axis.text.x = element_text(angle = 0,vjust = 0.5, hjust=.5),
          plot.caption = element_text(hjust = 0, face = "italic",size=14)) 
  
 #theme(axis.text.x = element_text(angle = 60, hjust = 1))
 
 # Download data
 varsIn <- c("year", "county", "sex", "eduName", "causeName", myMeasure)
 
 if(myMeasure == "cDeathRate") varsIn <- c(varsIn, "rateSE", "rateLCI", "rateUCI")
 if(myMeasure == "aRate") varsIn <- c(varsIn, "aSE", "aLCI", "aUCI")
 
 df <- dat.1 %>%
   left_join(select(deathCauseLink, causeCode, causeName), by = "causeCode") %>%
   select(varsIn) %>%
   arrange(year)
 
 list(plotL = myPlot, dataL = df)
 
 
 }

