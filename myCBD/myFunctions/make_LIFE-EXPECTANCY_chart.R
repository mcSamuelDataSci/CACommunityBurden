

if(1==2){
  myLHJ="Siskiyou" 
  myLHJ="CALIFORNIA"
  myLHJ="Alameda" 
  myLHJ="Butte"
  myLHJ="Marin"
  myCause="A01"
  myMeasure = "YLL"
  mySex   = "Total"
}


# myDrive         <- getwd()
# myPlace <- paste0(myDrive,"/myCBD") 
# 
# library(readxl)
# library(dplyr)
# library(stringr)
# library(ggplot2)
# library(directlabels) 

geoMap     <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/County Codes to County Names Linkage.xlsx"))) %>%
               select(FIPSCounty,county=countyName)


# LTplace         <- paste0(myPlace,"/myUpstream","/lifeTables/dataOut")
 LTplace         <- paste0(myPlace,"/myData")


lifeTableCounty <- readRDS(paste0(LTplace,"/LTciCounty.rds")) %>%
                     mutate(FIPSCounty=substr(GEOID,3,5))  %>%
                     left_join(geoMap,by="FIPSCounty") %>%
                     mutate(sex = str_to_title(sex))


lifeTableState <- readRDS(paste0(LTplace,"/LTciState.rds")) %>%        
                     mutate(county = "CALIFORNIA") %>% 
                     mutate(sex = str_to_title(sex))

lifeTableSet <- bind_rows(lifeTableCounty, lifeTableState)



# FIX MIN and MAX Year in global or other life tables function eventaully

minYear <- min(lifeTableSet$year)
maxYear <- max(lifeTableSet$year)


LEtrend <- function(myLHJ="CALIFORNIA",myCI=FALSE) {


dat.1 <- lifeTableSet %>% filter(county==myLHJ)


if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those or all data are supressed because of SMALL NUMBERS")


myTit <- paste0("Trend in Life Expectancy, ",myLHJ,", ",minYear,"-",maxYear)
myTit <-  wrap.labels(myTit,myWrapNumber)

myBreaks <- minYear:maxYear
myLabels <- myBreaks

# USE meanex



tplot<-
 
  
   ggplot(data=dat.1, aes(x=year, y=meanex, group=sex,color=sex)) +
               geom_line(size=myLineSize) +
  geom_point(shape = myPointShape,size=myPointSize)  +
       geom_line(data=dat.1,aes(x=year, y=`ciex.97.5%`, group=sex,color=sex)) +
      geom_line(data=dat.1,aes(x=year, y=`ciex.2.5%`, group=sex,color=sex)) +
        scale_x_continuous(minor_breaks=myBreaks,breaks=myBreaks,expand=c(0,1),labels=myLabels) +
  #  scale_y_continuous(limits = c(0, NA)) +
    scale_colour_discrete(guide = 'none') +   # removed legend
    labs(y = "life expectancy at birth")  + 
    geom_dl(aes(label = sex), method = list(dl.trans(x = x + myLineLabelSpace), "last.points", cex=myCex1, font="bold")) +
    geom_dl(aes(label = sex), method = list(dl.trans(x = x - myLineLabelSpace), "first.points",cex=myCex1, font="bold"))  +
    labs(title =myTit,size=myTitleSize) +
    theme_bw() +
    theme(axis.text=element_text(size=myAxisSize),
          axis.title=element_text(size=myAxisSize,face="bold"),
          plot.title=element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize)
       #   axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1) 
  
       
         )
       
 tplot
 
 # ggplotly(tplot)


}

