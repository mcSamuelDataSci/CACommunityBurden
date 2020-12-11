raceColors1        <- c("seashell4", "chocolate1", "firebrick", "royalBlue1", "darkblue", "navajowhite3", "red",   "chartreuse4")
raceNames1         <- c("AIAN_NH",   "ASIAN_NH",   "BLACK_NH",  "HISPANIC",   "MR_NH",    "NHPI_NH",      "TOTAL", "WHITE_NH")
names(raceColors1) <- raceNames1
totalColor         <- "red"

geoMap          <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/County Codes to County Names Linkage.xlsx"))) %>%
                     select(FIPSCounty,county=countyName)

lifeTableCounty <- readRDS(paste0(myPlace,"/myData/e0ciCounty.rds")) %>%
                     mutate(FIPSCounty=substr(GEOID,3,5))  %>%
                     left_join(geoMap,by="FIPSCounty") %>%
                     mutate(sex = str_to_title(sex))

lifeTableState  <- readRDS(paste0(myPlace,"/myData/e0ciState.rds")) %>%
                     mutate(county = "CALIFORNIA") %>%
                     mutate(sex = str_to_title(sex))

lifeTableSet0   <- bind_rows(lifeTableCounty, lifeTableState)


#lifeTableSet <- filter(lifeTableSet0,race7 == "TOTAL")
lifeTableSet <- filter(lifeTableSet0,race7 != "MR_NH")

# FIX MIN and MAX Year in global or other life tables function eventaully
  minYear_LT <- min(lifeTableSet$year)
  maxYear_LT <- max(lifeTableSet$year)

  
  
  
  
  
  
  
  
 LEtrend <- function(myLHJ="CALIFORNIA", mySexMult, myRace, myCI, CCB = T) {

   
 if (1==2){
   myLHJ="CALIFORNIA"
   mySexMult = "Total"
   myRace = "TOTAL"
 }
  
   # Put in standards 
 if(!CCB) {
   myWrapNumber <- 70
   myAxisSize  <- 22
   myTitleColor <- "darkblue"
   myTitleSize <- 24
   
   source(paste0(myPlace, "myFunctions/helperFunctions/wrapLabels.R"))
 }
   
   
 dat.1 <- lifeTableSet %>% filter(county==myLHJ, sex %in% mySexMult ,race7 %in% myRace)

 
 
 
 
 
 
 if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those or all data are supressed because of SMALL NUMBERS")

 myTitle <- paste0("Trend in Life Expectancy, ",myLHJ,", ",minYear_LT,"-",maxYear_LT)
 myTitle <-  wrap.labels(myTitle,myWrapNumber)

 myBreaks <- minYear_LT:maxYear_LT
 myLabels <- myBreaks

 
 # mySex <- c("Male","Female")
 # myRace <- "TOTAL"
 
 
 tplot<- ggplot(data=dat.1, aes(x=year, y=ex)) +
                 geom_line(size=1.6,aes(color=race7,linetype=sex)) +
               # geom_point(shape = myPointShape,size=myPointSize)  +
   # geom_line(data=dat.1,aes(x=year, y=exlow, color=race7,linetype=sex)) +
   # geom_line(data=dat.1,aes(x=year, y=exhigh,color=race7,linetype=sex)) +
                 ylim(62, 93) +
                 scale_x_continuous(minor_breaks=myBreaks,breaks=myBreaks,expand=c(0,2),labels=myLabels) +
                 scale_color_manual(values = raceColors1) +   
                 labs(title =myTitle, y = "life expectancy at birth")  +
                 theme_bw() +
                  theme(axis.text=element_text(size=myAxisSize),
                        axis.title=element_text(size=myAxisSize,face="bold"),
                        plot.title=element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize),
                        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1)
                       )
 
 
if (myCI) {
    tplot <- tplot +
        geom_line(data=dat.1,aes(x=year, y=exlow, color=race7,linetype=sex)) +
        geom_line(data=dat.1,aes(x=year, y=exhigh,color=race7,linetype=sex)) 
 }
     
 
# tplot <- tplot + ylim(62, 93)
 
 
 # if (!myLifeRace) {
 #   dat.1  <- filter(dat.1,race7 == myRace)
 #   
 #    tplot<- ggplot(data=dat.1, aes(x=year, y=ex)) +
 #     geom_line(size=myLineSize,aes(linetype=sex),color=totalColor,show.legend=FALSE) +
 #     ylim(66, 90) +
 #     geom_line(data=dat.1,aes(x=year, y=exlow, group=sex),color=totalColor) +
 #     geom_line(data=dat.1,aes(x=year, y=exhigh, group=sex),color=totalColor) +
 #     scale_x_continuous(minor_breaks=myBreaks,breaks=myBreaks,expand=c(0,2),labels=myLabels) +
 #     scale_colour_discrete(guide = 'none') +   # removed legend
 #     labs(title =myTitle, y = "life expectancy at birth")  +
 #     geom_dl(aes(label = sex), method = list(dl.trans(x = x + myLineLabelSpace), "last.points", cex=myCex1, font="bold")) +
 #     geom_dl(aes(label = sex), method = list(dl.trans(x = x - myLineLabelSpace), "first.points",cex=myCex1, font="bold"))  +
 #     theme_bw() +
 #     theme(axis.text=element_text(size=myAxisSize),
 #           axis.title=element_text(size=myAxisSize,face="bold"),
 #           plot.title=element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize),
 #           axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1)
 #     )
 #   }
 
 
 
  tplot

  
  
  
  
 }
 
 
 # -------------------------------------------------------------------------------
 
 
 
 if(1==2){
   myLHJ="Alameda" 
   myLHJ="CALIFORNIA"
   myLHJ="Alameda" 
   myLHJ="Butte"
   myLHJ="Marin"
   myCause="A01"
   myMeasure = "YLL"
   mySex   = "Total"
 }
 
 LTplace    <- paste0(myPlace,"/myData")   # "/myUpstream","/lifeTables/dataOut"
 
 