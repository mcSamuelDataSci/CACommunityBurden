
if(1==2){
  myLHJ="CALIFORNIA" 
  myCause="E"
  myMeasure = "aRate"
  mySex   = "Total"
  myLogTrans=FALSE
  myMultiRace = FALSE
  }

nCut      <- 20
myYearG3  <- "2016-2018"

# myJunk <- FALSE
# if (whichData == "fake") myJunk <- TRUE
# myJunk <- TRUE



lowColor  <- "palegreen"
midColor  <- "paleturquoise"
highColor <- "tomato"






# RACE --------------------------------------------------------------------------------------------------------------------------




raceTest <- datCounty_RE %>%  
              filter(raceCode != "Multi-NH") %>% 
              filter(Ndeaths > nCut ) %>%
              select(-YLL,-mean.age,-YLLper,-cDeathRate,-rateLCI,-rateUCI,-YLL.adj.rate,LCI=aLCI,UCI=aUCI)


raceTest2 <- raceTest %>% group_by(county,yearG3,sex,CAUSE) %>%
               mutate(bestRate = min(aRate),
                      bestSE   = aSE)  %>%
               filter(bestRate == aRate) %>%
               mutate(lowRace = raceCode) %>%
               select(-(Ndeaths:aSE),-raceCode)

   
raceTest <- left_join(raceTest,raceTest2,by=c("county","yearG3","sex","CAUSE")) %>%
                mutate(rateRatio = round(aRate/bestRate,1),
                       Ztest = (aRate - bestRate) / sqrt(aSE^2 + bestSE^2),
                       pValue = 1-pnorm(Ztest),
                       pMark = as.factor(ifelse(aRate==bestRate,"Lowest",ifelse(pValue < .01,"Sig. Higher (p<.01)","No Difference")))
                       ) 

# AGe ----------------------------------------------------------------------------------------------------------------------------



 ageTest <- datCounty_AGE_3year %>%
    filter(Ndeaths > nCut,!is.na(cDeathRate) ) %>%    # need !is.na becuase of tiny number missing age --> NA fix 
    select(-YLL,-mean.age,-YLLper,cDeathRate,LCI=rateLCI,UCI=rateUCI)


 ageTest2 <- ageTest %>% group_by(county,yearG3,sex,CAUSE) %>%
    mutate(bestRate = min(cDeathRate),
           bestSE   = rateSE) %>%
    filter(bestRate == cDeathRate)  %>%
    mutate(lowAge = ageG) %>%
    select(-(Ndeaths:UCI),-ageG)

ageTest <- left_join(ageTest,ageTest2,by=c("county","yearG3","sex","CAUSE")) %>%
    mutate(rateRatio = round(cDeathRate/bestRate,1),
           Ztest = (cDeathRate - bestRate) / sqrt(rateSE^2 + bestSE^2),
           pValue = 1-pnorm(Ztest),
           pMark = as.factor(ifelse(cDeathRate==bestRate,"Lowest",ifelse(pValue < .01,"Sig. Higher (p<.01)","No Difference")))
    )


# Sex -------------------------------------------------------------------------------------------------------------------------------


sexTest <- datCounty_3year %>%  
   filter(Ndeaths > nCut, !is.na(aRate),sex != "Total" ) %>%
   select(-YLL,-mean.age,-YLLper,-cDeathRate,-rateLCI,-rateUCI,-YLL.adj.rate,LCI=aLCI,UCI=aUCI)

sexTest2 <- sexTest %>% group_by(county,yearG3,CAUSE) %>%
   mutate(bestRate = min(aRate),
          bestSE   = aSE)  %>%
   filter(bestRate == aRate) %>%
   mutate(lowRace = sex) %>%
   select(-(Ndeaths:aSE),-sex)


sexTest <- left_join(sexTest,sexTest2,by=c("county","yearG3","CAUSE")) %>%
   mutate(rateRatio = round(aRate/bestRate,1),
          Ztest = (aRate - bestRate) / sqrt(aSE^2 + bestSE^2),
          pValue = 1-pnorm(Ztest),
          pMark = as.factor(ifelse(aRate==bestRate,"Lowest",ifelse(pValue < .01,"Sig. Higher (p<.01)","No Difference")))
   ) 


# ------------------------------------------------------------------------------------

disparity <- function(myLHJ="CALIFORNIA",myCause="A") {

myCex <- 1.6
myCol <- "blue"            #mycol <- rep("blue",nrow(dat.1))


#--RACE ------------------------------------------------------------------------------------------------------------------------

myMeasureRace <- "aRate"


# dat.1 <- filter(raceTest,county == myLHJ,CAUSE == myCause, yearG3==myYearG3) %>%
  dat.1 <- filter(raceTest,county == myLHJ,CAUSE == myCause, yearG3==myYearG3, sex == "Total") %>%
           mutate(raceName = raceNameFull[match(raceCode,raceCodeFull)] )

if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those or all data are supressed because of SMALL NUMBERS")

myTit <- "Race/Ethnicity (age-adjusted rate)"

racePlot <- ggplot(data=dat.1, aes(x=raceName, y=eval(parse(text=paste0(myMeasureRace))),fill=pMark)) +
   geom_bar(stat="identity") +
   theme_grey() +   #base_size = myBaseSize
  # facet_grid(rows = vars(sex)) +
     scale_fill_manual("legend", values = c("Lowest" = lowColor, "Sig. Higher (p<.01)" = highColor, "No Difference" = midColor)) +
  # guides(fill = guide_legend(reverse=TRUE)) +
   geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.1, size=1, position=position_dodge(.9), color="gray") + 
   labs(y = deathMeasuresNames[deathMeasures == myMeasureRace], x="Race/Ethnicity") +
      theme(legend.position="bottom",
            plot.title=element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize),
         axis.title.y = element_blank(),
         axis.title.x = element_blank(),
         axis.text.y = element_text(size = myAxisSize),
         axis.text.x = element_text(size = myAxisSize),
      legend.title = element_blank(),
         legend.text = element_text(size = myLegendSize),
         strip.text = element_text(size = myAxisSize)) +
      labs(title =myTit)   # ,size=mySize1
   #legend.text = element_text(size = myLegendSize) +
      
   
 
#--AGE ------------------------------------------------------------------------------------------------------------------------

ageMap     <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/Age Group Standard and US Standard 2000 Population.xlsx"),sheet = "data"))



ageMap$ageG          <- paste(ageMap$lAge,"-",ageMap$uAge)

ageMap$ageOrder      <- paste(ageMap$lAge,"-",ageMap$uAge)
ageMap$ageOrder[1:2] <- paste("",ageMap$ageOrder [1:2])
ageMap$ageOrder[10]  <- "85+"

myMeasure <- "cDeathRate"

# dat.1 <- filter(raceTest,county == myLHJ,CAUSE == myCause, yearG3==myYearG3) %>%
dat.1 <- filter(ageTest,county == myLHJ,CAUSE == myCause, yearG3==myYearG3, sex == "Total") %>%
           left_join(ageMap,by="ageG")  



if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those or all data are supressed because of SMALL NUMBERS")


myTit <-  "Age Groups (age-specific rate)"

agePlot <- ggplot(data=dat.1, aes(x=ageOrder, y=eval(parse(text=paste0(myMeasure))),fill=pMark)) +
   geom_bar(stat="identity") +
   theme_grey() +   #base_size = myBaseSize
 #  facet_grid(rows = vars(sex)) +
   scale_fill_manual("legend", values = c("Lowest" = lowColor, "Sig. Higher (p<.01)" = highColor, "No Difference" = midColor)) +
  # guides(fill = guide_legend(reverse=TRUE)) +
   geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.1, size=1, position=position_dodge(.9), color="gray") +
   labs(y = deathMeasuresNames[deathMeasures == myMeasure], x="Race/Ethnicity") +
   theme(legend.position="bottom",
         plot.title=element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize),
         axis.title.y = element_blank(),
         axis.title.x = element_blank(),
         axis.text.y = element_text(size = myAxisSize),
         axis.text.x = element_text(size = myAxisSize),
         legend.title = element_blank(),
         legend.text = element_text(size = myLegendSize),
         strip.text = element_text(size = myAxisSize)) +
   labs(title =myTit)   # ,size=mySize1
#legend.text = element_text(size = myLegendSize) +





#--SEX ------------------------------------------------------------------------------------------------------------------------

myMeasureRace <- "aRate"  # works...


dat.1 <- filter(sexTest,county == myLHJ,CAUSE == myCause, yearG3==myYearG3) 

if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those or all data are supressed because of SMALL NUMBERS")


myTit <-  "Sex (age-adjusted rate)"

sexPlot <- ggplot(data=dat.1, aes(x=sex, y=eval(parse(text=paste0(myMeasureRace))),fill=pMark)) +
   geom_bar(stat="identity") +
   theme_grey() +   #base_size = myBaseSize
   scale_fill_manual("legend", values = c("Sig. Higher (p<.01)" = highColor, "No Difference" = midColor,"Lowest" = lowColor)) +
  # guides(fill = guide_legend(reverse=TRUE)) +
   geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.1, size=1, position=position_dodge(.9), color="gray") + 
   labs(y = deathMeasuresNames[deathMeasures == myMeasureRace], x="Sex") +
   theme(legend.position="bottom",
         plot.title=element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize),
         axis.title.y = element_blank(),
         axis.title.x = element_blank(),
         axis.text.y = element_text(size = myAxisSize),
         axis.text.x = element_text(size = myAxisSize),
         legend.title = element_blank(),
         legend.text = element_text(size = myLegendSize),
         strip.text = element_text(size = myAxisSize)) +
   labs(title =myTit)   # ,size=mySize1
#legend.text = element_text(size = myLegendSize) +


#------------------------------------------------------------------------------------
library(cowplot)
# https://wilkelab.org/cowplot/articles/plot_grid.html
mainTitle <- ggdraw() + 
   draw_label(paste0("Disparities in Deaths Rates, ", fullCauseList[fullCauseList[,"LABEL"]== myCause,"nameOnly"]," in ",myLHJ,", ",myYearG3), colour=myTitleColor, size=myTitleSize,fontface = "bold")





# if(myJunk) {

dPlot <- cowplot::plot_grid(racePlot, agePlot, sexPlot,labels = c("A", "B","c"), nrow=3,
                            rel_widths = c(2, 2,1))
#}


topRow <- plot_grid(racePlot,sexPlot,rel_widths = c(5,2))
dPlot   <- plot_grid(mainTitle,topRow,agePlot,ncol=1,rel_heights = c(1,5,5))



myPlotly <- FALSE 
if (!myPlotly) dplot <- dPlot
if (myPlotly) dplot <- ggplotly(dPlot) 
                      

 
dPlot
 

 }
