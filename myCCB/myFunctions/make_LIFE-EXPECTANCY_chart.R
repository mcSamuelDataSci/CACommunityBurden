library(dplyr)
library(ggplot2)
library(readxl)


if(1==2){
  myLHJ="Alameda" 
  myLHJ="CALIFORNIA"
  myLHJ="Alameda" 
  myLHJ="Butte"
  myLHJ="Marin"
  mySex <- c("Male","Female")
  myRace <- "TOTAL"
  myCause="A01"
  myMeasure = "YLL"
  mySex   = "Total"
  myLHJ="CALIFORNIA"
  mySexMult = c("Male","Female")
  # myRace = c("AIAN_NH",   "ASIAN_NH",   "BLACK_NH",  "HISPANIC",   "WHITE_NH")
  myRace = c("AIAN",   "Asian",   "Black",  "Hisp",   "White")
  }

# LTplace    <- paste0(myPlace,"/myData")   # "/myUpstream","/lifeTables/dataOut"


geoMap          <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/County Codes to County Names Linkage.xlsx"))) %>%
                     select(FIPSCounty,county=countyName)

lifeTableCounty <- readRDS(paste0(myPlace,"/myData/e0ciCounty.RDS")) %>%
                     mutate(FIPSCounty=substr(GEOID,3,5))  %>%
                     left_join(geoMap,by="FIPSCounty") %>%
                     mutate(sex = str_to_title(sex))

lifeTableState  <- readRDS(paste0(myPlace,"/myData/e0ciState.RDS")) %>%
                     mutate(county = "CALIFORNIA") %>%
                     mutate(sex = str_to_title(sex))

lifeTableSet0   <- bind_rows(lifeTableCounty, lifeTableState)



lifeTableSet <- filter(lifeTableSet0,raceCode != "Multi") # JASPO changed from race7 to raceCode

# FIX MIN and MAX Year in global or other life tables function eventaully
  minYear_LT <- min(lifeTableSet$year)
  maxYear_LT <- max(lifeTableSet$year)

  
  
  
 LEtrend <- function(myLHJ="CALIFORNIA", mySexMult, myRace, myCI) {
   
   raceColors1        <- c("seashell4", "chocolate1", "firebrick", "royalBlue1", "darkblue", "navajowhite3", "red",   "chartreuse4")
   # raceNames1         <- c("AIAN_NH",   "ASIAN_NH",   "BLACK_NH",  "HISPANIC",   "MR_NH",    "NHPI_NH",      "TOTAL", "WHITE_NH")
   raceNames1         <- c("AIAN",   "Asian",   "Black",  "Hisp",   "Multi",    "NHPI",      "Total", "White")
   names(raceColors1) <- raceNames1
   totalColor         <- "red"

 dat.1 <- lifeTableSet %>% filter(county==myLHJ, sex %in% mySexMult ,raceCode %in% myRace) %>% # originally race7, now raceCode JASPO
             mutate(lineLabel = paste(raceCode,"-",sex))


 if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those or all data are supressed because of SMALL NUMBERS")

 myTitle <- paste0("Trend in Life Expectancy, ",myLHJ,", ",minYear_LT,"-",maxYear_LT)
 myTitle <-  wrap.labels(myTitle,myWrapNumber)

 myBreaks <- minYear_LT:maxYear_LT
 myLabels <- myBreaks

 
 tplot_bar <- ggplot(data=filter(dat.1, year== 2019), aes(x=raceCode, y=ex, fill=sex)) + geom_bar(stat = "identity",position="dodge")  +
   scale_fill_manual(values = c("firebrick", "blue")) + labs(x = "Race/Ethnicity", y = "Life Expectancy at Birth")
   
 
 
 tplot<- ggplot(data=filter(dat.1, nyrs == 1), aes(x=year, y=ex)) +
                 geom_line(size=1.6,aes(color=raceCode,linetype=sex)) +
               # geom_point(shape = myPointShape,size=myPointSize)  +
                 ylim(62, 93) +
                 scale_x_continuous(minor_breaks=myBreaks,breaks=myBreaks,
                                    expand = expansion(mult = c(0, 0), add = c(1, 5)), # lower-limit: 2000 - (2018 - 2000) * 0 - 1... upper-limit: 2018 + (2018 - 2000) * 0 + 5
                                    #expand=c(0,5), # 
                                    labels=myLabels) +
                 scale_color_manual(values = raceColors1) +   
                 labs(title =myTitle, y = "life expectancy at birth")  +
                 theme_bw() +
                  theme(axis.text=element_text(size=myAxisSize),
                        axis.title=element_text(size=myAxisSize,face="bold"),
                        plot.title=element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize),
                        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1), 
                        legend.position = "none"
                       ) +    
                  geom_dl(aes(label = lineLabel,color=raceCode), method = list(dl.trans(x = x + 0.2), "last.points", 
                                                                            size = myLineLabelSize, # use this instead of cex
                                                                            #cex=myCex1*.3, 
                                                                            'last.bumpup',font="bold")) 
 
if (myCI) {
    tplot <- tplot +
        geom_line(data=dat.1,aes(x=year, y=exlow, color=raceCode,linetype=sex)) +
        geom_line(data=dat.1,aes(x=year, y=exhigh,color=raceCode,linetype=sex)) 
 }
     
 
# tplot <- tplot + ylim(62, 93)
 
 
  list(trend=tplot, bar=tplot_bar)

  
  
  
  
 }
 
 
 # -------------------------------------------------------------------------------
 
 