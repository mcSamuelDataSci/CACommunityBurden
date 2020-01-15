if(1==2){
  myLHJ="CALIFORNIA" 
  myCause="D05"
  myMeasure = "aRate"
  mySex   = "Total"
  myYearGrouping ="One"
}

myCex <- 1.6
myCol <- "blue"  


trendGeneric <- function(myLHJ="CALIFORNIA",myCause="A",myMeasure = "YLL", myTab, myYearGrouping="One", myLogTrans=FALSE, myMultiRace=FALSE) {

  # ----- sexTrendTab ---------------------------------------------------------
  
  
  if (myTab %in% c("sexTrendTab", "educationTrendTab", "lifeExpectancyTab") )  {
    
    
    
    myVARIABLE <- "sex"
    
    if (myYearGrouping == "One") {
      
      inDat    <- datCounty  
      myBreaks <- 2000:maxYear
      myLabels <- myBreaks
    }
    
    if (myYearGrouping == "Three") {
      
      inDat <- datCounty_3year 
     
      chartYearMap  <-  chartYearMap  %>%
       select(yearGroup3,midYear3) %>%
       filter(!is.na(midYear3))    %>%
       unique()
      
      myLabels   <- chartYearMap$yearGroup3
      myBreaks   <- chartYearMap$midYear3
      inDat$year <- myBreaks[match(inDat$yearG3,myLabels)]
    }
    
    if (myYearGrouping == "Five") {
      
      inDat <- datCounty_5year 
      
      chartYearMap    <-  chartYearMap  %>%
       select(yearGroup5,midYear5) %>%
       filter(!is.na(midYear5))    %>%
       unique()
    
      myLabels   <- chartYearMap$yearGroup5
      myBreaks   <- chartYearMap$midYear5
      inDat$year <- myBreaks[match(inDat$yearG5,myLabels)]
    }
    
    dat.1   <- filter(inDat,county == myLHJ,CAUSE == myCause)
    
    myTitle <- paste0("Trend in ",deathMeasuresNames[deathMeasures == myMeasure],
                      " of ",fullCauseList[fullCauseList[,"LABEL"]== myCause,"nameOnly"],
                      " in ",myLHJ,", 2000 to ",maxYear)
    
    myLineLabel <- myVARIABLE
    
    
    if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those or all data are supressed because of SMALL NUMBERS")
    
    
    varsOut <- c("Level","mean.age","pop","rateSE","rateLCI","rateUCI","aLCI","aUCI","aSE","SMRcrude","SMR")  
    varsIn  <- c("Ndeaths", "YLL",  "YLLper", "cDeathRate", "aRate", "YLL.adj.rate") 
    
    tabDat <- dat.1 %>% select(-varsOut)
    
     
    
    
    
  }
  
  
  # ----- ageTrendTab -----
  
  else if (myTab == "ageTrendTab") {
    
     myVARIABLE <- "ageG"
    
      
      if(myMeasure == "YLL.adj.rate")  myMeasure <- "YLLper"   ## ADD MESSAGE ABOUT THIS
      if(myMeasure == "aRate")         myMeasure <- "cDeathRate"
      
      minYear <- 2000
      maxYear <- 2017
      
      
      dat.1 <- filter(datCounty_AGE_3year,county == myLHJ,CAUSE == myCause, sex=="Total") 
      
      if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those or all data are supressed because of SMALL NUMBERS")
      
      myTitle <- paste0("Trend in ",deathMeasuresNames[deathMeasures == myMeasure],
                        " of ",fullCauseList[fullCauseList[,"LABEL"]== myCause,"nameOnly"],
                        " in ",myLHJ," by AGE GROUP, ",minYear," to ",maxYear)
      myTitle <-  wrap.labels(myTitle,myWrapNumber)
      
      yRange     <- chartYearMap$yearGroup3
      yMid       <- chartYearMap$midYear3
      myLabels   <- yRange
      myBreaks   <- yMid
      dat.1$year <- yMid[match(dat.1$yearG3,yRange)]
      
      myLineLabel <- myVARIABLE
      
      myTrans    <- ifelse(myLogTrans,'log2','identity')
      myMin      <- ifelse(myLogTrans,NA,0)
      
      dat.1 <- mutate(dat.1,ageG = ifelse(ageG == "85 - 999","85+",ageG))
      
      tabDat <- dat.1
      
  }
  
  # ----- raceTrendTab -----
  
  else if (myTab == "raceTrendTab") {
    
    myVARIABLE <- "raceCode"
    
    
    minYear <- 2000
    maxYear <- 2017
    
    dat.1 <- filter(datCounty_RE,county == myLHJ,CAUSE == myCause, sex=="Total") %>%
      mutate(raceName = raceNameFull[match(raceCode,raceCodeFull)] )
    
    if (!myMultiRace) dat.1 <- filter(dat.1,raceCode != "Multi-NH")
    
    if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those or all data are supressed because of SMALL NUMBERS")
    
    myTitle <- paste0("Trend in ",deathMeasuresNames[deathMeasures == myMeasure],
                      " of ",fullCauseList[fullCauseList[,"LABEL"]== myCause,"nameOnly"],
                      " in ",myLHJ," by RACE/ETHNIC Group*, ",minYear," to ",maxYear)
    myTitle <-  wrap.labels(myTitle,myWrapNumber)
    
    
    yRange     <- chartYearMap$yearGroup3
    yMid       <- chartYearMap$midYear3
    myLabels   <- yRange
    myBreaks   <- yMid
    dat.1$year <- yMid[match(dat.1$yearG3,yRange)]
    
    myLineLabel <- "raceName"
    
    myTrans    <- ifelse(myLogTrans,'log2','identity')
    myMin      <- ifelse(myLogTrans,NA,0)    
    
    tabDat <- dat.1
    
    
    
  }
  
  # ----- educationTrendTab -----
  else if (myTab == "educationTrendTab") {
    
  }
  

#### Generic Part Here =====================================================================================

myTitle <-  wrap.labels(myTitle,myWrapNumber)

tplot <-  ggplot(data=dat.1, 
                  aes(x=year, y=eval(parse(text=paste0(myMeasure))), 
                  group=get(myVARIABLE), 
                  color=get(myVARIABLE))) +
          
          geom_line(size=myLineSize)  +
          geom_point(shape = myPointShape,size=myPointSize)  +
  
          scale_x_continuous(minor_breaks=myBreaks,breaks=myBreaks,expand=c(0,3),labels=myLabels) +
          scale_y_continuous(limits = c(0, NA)) +
            
          scale_colour_discrete(guide = 'none') +   # removed legend
          geom_dl(aes(label = get(myLineLabel)), method = list(dl.trans(x = x + 0.2), "last.points", cex=myCex1, 'last.bumpup',font="bold")) +
          geom_dl(aes(label = get(myLineLabel)), method = list(dl.trans(x = x - 0.2), "first.points",cex=myCex1,'first.bumpup' ,font="bold"))  +
  
          labs(title = myTitle,
                   y = deathMeasuresNames[deathMeasures == myMeasure]
               ) +
          theme_bw(
            base_size   = myAxisSize) +
          theme(
            plot.title  = element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize),
            axis.title  = element_text(face="bold"),
            axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1)
            ) 

 list(plot=tplot,data= tabDat)

}
