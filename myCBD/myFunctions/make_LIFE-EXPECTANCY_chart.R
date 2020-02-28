

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

lifeTableSet <- bind_rows(lifeTableCounty, lifeTableState) %>%
                   filter(county != "Mono")      # Something seems to be wrong with Mono data

#### --- NEW ---------------------------------------------------------------------
#### -----------------------------------------------------------------------------





lifeTable0 <- readRDS(paste0(LTplace,"/lt_e0_all.rds")) %>%
mutate(FIPSCounty=substr(GEOID,3,5))  %>%
  left_join(geoMap,by="FIPSCounty") %>%
   mutate(sex = str_to_title(sex)) %>%
   mutate(county = ifelse(FIPSCounty=="000","CALIFORNIA",county) )

myLHJ="CALIFORNIA"; mySrc = "st1"
myLHJ="Alameda"   ; mySrc = "co5"
myLHJ="Butte"
myLHJ="Marin"

temp <- filter(lifeTable0,county==myLHJ,sex != "Total",src==mySrc)


,race != "tot"

ggplot(data=temp, aes(x=year, y=ex, color=race7,linetype=sex)) +
  geom_line(size=1.2)  +
  labs(title=myLHJ) +
  scale_x_continuous(minor_breaks=2000:2018,breaks=2000:2018,labels=2000:2018)
  
#### -----------------------------------------------------------------------------
#### -----------------------------------------------------------------------------


# FIX MIN and MAX Year in global or other life tables function eventaully

minYear_LT <- min(lifeTableSet$year)
maxYear_LT <- max(lifeTableSet$year)


LEtrend <- function(myLHJ="CALIFORNIA",myCI=FALSE) {


dat.1 <- lifeTableSet %>% filter(county==myLHJ)


if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those or all data are supressed because of SMALL NUMBERS")


myTit <- paste0("Trend in Life Expectancy, ",myLHJ,", ",minYear_LT,"-",maxYear_LT)
myTit <-  wrap.labels(myTit,myWrapNumber)

myBreaks <- minYear_LT:maxYear_LT
myLabels <- myBreaks

# USE meanex



tplot<-
 
  
   ggplot(data=dat.1, aes(x=year, y=ex, group=sex,color=sex)) +
               geom_line(size=myLineSize) +
  geom_point(shape = myPointShape,size=myPointSize)  +
       ylim(70, 90) +
       geom_line(data=dat.1,aes(x=year, y=`ciex_97.5`, group=sex,color=sex)) +
      geom_line(data=dat.1,aes(x=year, y=`ciex_2.5`, group=sex,color=sex)) +
        scale_x_continuous(minor_breaks=myBreaks,breaks=myBreaks,expand=c(0,2),labels=myLabels) +
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

