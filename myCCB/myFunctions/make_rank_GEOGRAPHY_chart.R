# rankGeo <- function(myLHJ, myCause="A", myMeasure = "YLL", myYear=2015,mySex="Total", myCI=TRUE,myRefLine=FALSE) {
# 
#   
#     if (1==2){
#       myCause="A"
#       myMeasure = "Ndeaths"
#       myLHJ = STATE
#       mySex = "Total"
#         }
#   
#   
# 
#     if (myLHJ != STATE) {        cZoom <- TRUE
#     } else { cZoom <-FALSE}
#     
#     
#     if (cZoom & myMeasure == "SMR") stop("I appologize dear, but SMR is not calcualted for now below the county level")
#     
#     causeLab <- fullCauseList[fullCauseList[,"LABEL"]==myCause,"nameOnly"]
#     sexLab   <- ""
#                 if (mySex != "Total") sexLab <- paste0(", among ",mySex,"s")
#     
#      
#     datCounty$plotter <- datCounty[,myMeasure]
#     datCounty         <- datCounty %>% filter(year==myYear,sex==mySex,CAUSE==myCause) 
#     
#     
#  
#     if (cZoom) { datComm$plotter <- datComm[,myMeasure]
#                  dat.1     <- datComm  %>% filter(county==myLHJ,yearG5==yearGrp,sex==mySex,CAUSE==myCause, comID != "Unknown")  %>%
#                  arrange(!is.na(plotter),plotter) %>%
#                  mutate(lab =wrap.labels(comName,30))
#     tit       <- paste0("Community Ranking of ",deathMeasuresNames[deathMeasures == myMeasure]," for ",causeLab," in ",myLHJ," in ",yearGrp,sexLab) 
#     sMeasure  <- datCounty$plotter[datCounty$county==myLHJ]
#     }
#     
#     
#     
#     if (!cZoom) { dat.1     <- datCounty  %>%   arrange(!is.na(plotter),plotter) %>%
#                                  mutate(lab =county)
#                  tit       <- paste0("County Ranking of ",deathMeasuresNames[deathMeasures == myMeasure]," for ",causeLab," in ",myYear,sexLab)
#                  sMeasure  <- dat.1$plotter[dat.1$county==STATE]
#                  }
#   
# 
#     tit <-  wrap.labels(tit,80)
#     
#     
#     
#     
#     
#    geoPlot <- function(){  
#     
#     
#   par(mar=par()$mar+c(2,16,4,0))
#   #  par(mar=c(2,12,2,0),oma=c(0,0,0,0))
#     
#     
#   if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those OR all data are suppressed because of small numbers")
#   
# 
#    
#     t.plot <- barplot(dat.1$plotter,col="gray",cex.names=5,horiz=TRUE,
#                       border="black",
#                       offset=0,
#                       xlab=names(deathMeasuresNames[deathMeasures == myMeasure]))
#     
#     grid(nx=NULL,ny=NA)
#     # t.plot <- barplot(dat.1$plotter,col="gray",cex.names=.8,horiz=TRUE,add=TRUE,
#     #                   border="black",
#     #                   offset=0,
#     #                   xlab=names(deathMeasuresNames[deathMeasures == myMeasure]))
#     # 
#     
#     axis(side=2,at=t.plot,labels=dat.1$lab,las=2,cex.axis=1.6)
#    
#    
#    
#     if (myRefLine)  segments(x0=sMeasure,y0=t.plot[1],y1=t.plot[length(t.plot)],lwd=3,lty=2)
#    
#    
#     if (myCI & myMeasure=="cDeathRate") {arrows(y0=t.plot,x0=dat.1$rateLCI,x1=dat.1$rateUCI,col="blue",length=.05,angle=90,code=3,lwd=2)}
#     if (myCI & myMeasure=="aRate")      {arrows(y0=t.plot,x0=dat.1$aLCI,x1=dat.1$aUCI,col="blue",length=.05,angle=90,code=3,lwd=2)}
#    
#     axis(side=3)
#    
#    mtext(tit,cex=2,line=3,font=2,col = myTitleColor,at=0,adj=0)
#    
# 
#    
#    
#   }
# 
# 
# 
# 
#  plotL <- geoPlot()
#  list(plotL = plotL, dataL = dat.1)
# 
#  
# }



# -------------------------------------- My section -----------

# myLHJ = STATE
# myCause=0
# myMeasure = "cDeathRate"
# myYear=2018
# mySex="Total"
# myCI=TRUE
# myRefLine=TRUE


rankGeo <- function(myLHJ, myCause="A", myMeasure = "YLL", myYear=2015,mySex="Total", myCI=TRUE,myRefLine=FALSE) {


  if (1==2){
    myCause="A"
    myMeasure = "Ndeaths"
    myLHJ = STATE
    mySex = "Total"
  }



  if (myLHJ != STATE) {        cZoom <- TRUE
  } else { cZoom <-FALSE}


  if (cZoom & myMeasure == "SMR") stop("I appologize dear, but SMR is not calculated for now below the county level")

  causeLab <- deathCauseLink$causeName[deathCauseLink$causeCode == myCause]
  sexLab   <- ""
  if (mySex != "Total") sexLab <- paste0(", among ",mySex,"s")


  datCounty$plotter <- datCounty[,myMeasure]
  datCounty         <- datCounty %>% filter(year==myYear,sex==mySex,causeCode==myCause)



  if (cZoom) { datComm$plotter <- datComm[,myMeasure]
  dat.1     <- datComm  %>% filter(county==myLHJ,yearG5==yearGrp5,sex==mySex,causeCode==myCause, comID != "Unknown")  %>%
    arrange(!is.na(plotter),plotter) %>%
    mutate(lab =wrap.labels(comName,30))
  tit       <- paste0("Community Ranking of ",deathMeasuresNames[deathMeasures == myMeasure]," for ",causeLab," in ",myLHJ," in ",yearGrp5,sexLab)
  sMeasure  <- datCounty_5year %>% 
    filter(county == myLHJ, yearG5 == yearGrp5, sex == mySex, causeCode == myCause) %>% 
    pull ( {{ myMeasure }})
  }



  if (!cZoom) { dat.1     <- datCounty  %>%   arrange(!is.na(plotter),plotter) %>%
    mutate(lab =county)
  tit       <- paste0("County Ranking of ",deathMeasuresNames[deathMeasures == myMeasure]," for ",causeLab," in ",myYear,sexLab)
  sMeasure  <- dat.1$plotter[dat.1$county==STATE]
  }


  # tit <-  wrap.labels(tit,80)

  # -- roundUpNice function rounds up numbers to multiples of ten
  
  # roundUpNice <- function(x, nice=c(1:10)) {
  #   if(length(x) != 1) stop("'x' must be of length 1")
  #   10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
  # }
  # 
  # axis_numbers <- seq(0, roundUpNice(max(dat.1$plotter)),
  #                     by=roundUpNice(max(dat.1$plotter)/4))
      
  rank_geo_plot <- ggplot(dat.1, aes(x=reorder(lab, plotter), y=plotter)) +
    geom_bar(stat='identity', fill = 'gray', color = 'black') +
    coord_flip() +
    ggtitle(stringr::str_wrap(tit, 62)) +
    scale_y_continuous(# breaks=axis_numbers, 
                       sec.axis = dup_axis()) +
   # geom_hline(yintercept = c(axis_numbers[-1]), linetype=3) + # c(axis_numbers[-1])
    theme_bw() + # element_text(size = rel(#)) below fixes the text scaling issue when specifying width and height of plot
    theme(plot.title = element_text(size = rel(2.5), colour = "blue"), axis.title=element_blank(), axis.text = element_text(size = rel(1.5)))
  
  # Reference line
  if(!cZoom && myRefLine == TRUE) { 
    rank_geo_plot = rank_geo_plot + 
      geom_hline(yintercept=sMeasure, linetype="dotted", size = 1) + # geom_text below aligns text vertically while using hjust and vjust
      geom_text(aes(x= length(lab), label="State Reference Line", y=sMeasure), colour="black", angle=270, hjust = 0, vjust = 1.4, size=6)}
  
  if(cZoom && myRefLine == TRUE) { 
    rank_geo_plot = rank_geo_plot + 
      geom_hline(yintercept=sMeasure, linetype="dotted", size = 1) +
      geom_text(aes(x= length(lab), label="County Reference Line", y=sMeasure), colour="black", angle=270, hjust = 0, vjust = 1.4, size=6)}
  
  
  # Confidence Interval
  if(myCI && myMeasure=="cDeathRate") { rank_geo_plot = rank_geo_plot +
    geom_errorbar(aes(ymin = rateLCI, ymax = rateUCI), width = 0.5, color = "blue")}
  
  if(myCI && myMeasure=="aRate") { rank_geo_plot = rank_geo_plot +
    geom_errorbar(aes(ymin = aLCI, ymax = aUCI), width = 0.5, color = "blue")}
  
  # Clean up data frame for plotting
  
  # County Vars vs State Vars
  if(myLHJ == "CALIFORNIA") varsIn <- c("year", "county", "sex", "population", "causeName", myMeasure)
  if(myLHJ != "CALIFORNIA") varsIn <- c("yearG5", "county", "comName", "sex", "population", "causeName", myMeasure)
  
  # aRate and cRate have standard errors
  if(myMeasure == "cDeathRate") varsIn <- c(varsIn, "rateSE", "rateLCI", "rateUCI")
  if(myMeasure == "aRate") varsIn <- c(varsIn, "aSE", "aLCI", "aUCI")
  
  dat.1 <- dat.1 %>%
    left_join(select(deathCauseLink, causeCode, causeName), by = "causeCode") %>%
    select(varsIn) %>%
    arrange(desc(get(myMeasure)))
  
  list(plotL = rank_geo_plot, dataL = dat.1)


}

# df707 <- data.frame(x = c("Smallest", "Largest", "Middle"), y = c(1, 10, 5))
# sort(df707$y, decreasing = TRUE)[1]
# rankGeo(myLHJ = STATE,
#         myCause=0,
#         myMeasure = "aRate",
#         myYear=2018,
#         mySex="Total",
#         myCI=TRUE,
#         myRefLine=TRUE)
# myLHJ = STATE
# myCause=0
# myMeasure = "cDeathRate"
# myYear=2018
# mySex="Total"
# myCI=TRUE
# myRefLine=TRUE


# -------------------------------------------------------------------------

# 
#     par()$mar
# 
#     par(mar=par()$mar+c(2,16,4,0))
#     #  par(mar=c(2,12,2,0),oma=c(0,0,0,0))
# 
# 
#     if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those OR all data are suppressed because of small numbers")
# 
# 
# 
#     t.plot <- barplot(dat.1$plotter,col="gray",cex.names=5,horiz=TRUE,
#                       border="black",
#                       offset=0,
#                       xlab=names(deathMeasuresNames[deathMeasures == myMeasure]))
# 
#     grid(nx=NULL,ny=NA)
#     # t.plot <- barplot(dat.1$plotter,col="gray",cex.names=.8,horiz=TRUE,add=TRUE,
#     #                   border="black",
#     #                   offset=0,
#     #                   xlab=names(deathMeasuresNames[deathMeasures == myMeasure]))
#     #
# 
#     axis(side=2,at=t.plot,labels=dat.1$lab,las=2,cex.axis=1.6)
# 
# 
# 
#     if (myRefLine)  segments(x0=sMeasure,y0=t.plot[1],y1=t.plot[length(t.plot)],lwd=3,lty=2)
# 
# 
#     if (myCI & myMeasure=="cDeathRate") {arrows(y0=t.plot,x0=dat.1$rateLCI,x1=dat.1$rateUCI,col="blue",length=.05,angle=90,code=3,lwd=2)}
#     if (myCI & myMeasure=="aRate")      {arrows(y0=t.plot,x0=dat.1$aLCI,x1=dat.1$aUCI,col="blue",length=.05,angle=90,code=3,lwd=2)}
# 
#     axis(side=3)
# 
#     mtext(tit,cex=2,line=3,font=2,col = myTitleColor,at=0,adj=0)
# 
# 

# 
# 
# 
#   plotL <- geoPlot()
#   list(plotL = plotL, dataL = dat.1)
# 
# 
#   rankGeo(STATE)$plotL
