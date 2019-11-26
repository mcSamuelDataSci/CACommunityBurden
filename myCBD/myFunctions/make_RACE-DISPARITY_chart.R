if(1==2){
  myLHJ="CALIFORNIA" 
  myCause="0"
  myMeasure = "aRate"
  mySex   = "Total"
  myLogTrans=FALSE
  myMultiRace = FALSE
  }

nCut      <- 20
myYearG3  <- "2016-2018"

lowColor  <- "palegreen"
midColor  <- "paleturquoise"
highColor <- "tomato"


# RACE --------------------------------------------------------------------------------------------------------------------------

raceTest <- datCounty_RE %>%  
              filter(raceCode != "Multi-NH") %>% 
              filter(Ndeaths > nCut ) %>%
              select(-YLL,-mean.age,-YLLper,-cDeathRate,-rateLCI,-rateUCI,-YLL.adj.rate,LCI=aLCI,UCI=aUCI)

#LOWEST ----------------------------------------

raceTest2 <- raceTest %>% group_by(county,yearG3,sex,CAUSE) %>%
               mutate(bestRate = min(aRate),  #MINIMUM RATE
                      bestSE   = aSE)  %>%
               filter(bestRate == aRate) %>%
               mutate(lowRace = raceCode) %>%
               select(-(Ndeaths:aSE),-raceCode)

raceTest_LOW <- left_join(raceTest,raceTest2,by=c("county","yearG3","sex","CAUSE")) %>%
                mutate(rateRatio = round(aRate/bestRate,1),
                       Ztest = (aRate - bestRate) / sqrt(aSE^2 + bestSE^2),
                       pValue = 1-pnorm(Ztest),
                       pMark = as.factor(ifelse(aRate==bestRate,"Lowest",ifelse(pValue < .01,"Sig. Higher (p<.01)","No Difference")))
                       ) 

#HIGHEST -------------------------------------------

raceTest2 <- raceTest %>% group_by(county,yearG3,sex,CAUSE) %>%
   mutate(bestRate = max(aRate),  # MAXIMUM RATE
          bestSE   = aSE)  %>%
   filter(bestRate == aRate) %>%
   mutate(lowRace = raceCode) %>%
   select(-(Ndeaths:aSE),-raceCode)


raceTest_HIGH <- left_join(raceTest,raceTest2,by=c("county","yearG3","sex","CAUSE")) %>%
   mutate(rateRatio = round(aRate/bestRate,1),
          Ztest = (aRate - bestRate) / sqrt(aSE^2 + bestSE^2),
        #  pValue = 1-pnorm(Ztest),
          pValue = pnorm(Ztest),
          pMark = as.factor(ifelse(aRate==bestRate,"Highest",ifelse(pValue < .01,"Sig. Lower (p<.01)","No Difference")))
   ) 


# Age ----------------------------------------------------------------------------------------------------------------------------

 ageTest <- datCounty_AGE_3year %>%
    filter(Ndeaths > nCut,!is.na(cDeathRate) ) %>%    # need !is.na becuase of tiny number missing age --> NA fix 
    select(-YLL,-mean.age,-YLLper,cDeathRate,LCI=rateLCI,UCI=rateUCI)


# LOWEST 

 ageTest2 <- ageTest %>% group_by(county,yearG3,sex,CAUSE) %>%
    mutate(bestRate = min(cDeathRate),
           bestSE   = rateSE) %>%
    filter(bestRate == cDeathRate)  %>%
    mutate(lowAge = ageG) %>%
    select(-(Ndeaths:UCI),-ageG)

 ageTest_LOW <- left_join(ageTest,ageTest2,by=c("county","yearG3","sex","CAUSE")) %>%
    mutate(rateRatio = round(cDeathRate/bestRate,1),
           Ztest = (cDeathRate - bestRate) / sqrt(rateSE^2 + bestSE^2),
           pValue = 1-pnorm(Ztest),
           pMark = as.factor(ifelse(cDeathRate==bestRate,"Lowest",ifelse(pValue < .01,"Sig. Higher (p<.01)","No Difference")))
    )

# HIGHEST

ageTest2 <- ageTest %>% group_by(county,yearG3,sex,CAUSE) %>%
   mutate(bestRate = max(cDeathRate),
          bestSE   = rateSE) %>%
   filter(bestRate == cDeathRate)  %>%
   mutate(lowAge = ageG) %>%
   select(-(Ndeaths:UCI),-ageG)

ageTest_HIGH <- left_join(ageTest,ageTest2,by=c("county","yearG3","sex","CAUSE")) %>%
   mutate(rateRatio = round(cDeathRate/bestRate,1),
          Ztest = (cDeathRate - bestRate) / sqrt(rateSE^2 + bestSE^2),
          pValue = pnorm(Ztest),
          pMark = as.factor(ifelse(cDeathRate==bestRate,"Highest",ifelse(pValue < .01,"Sig. Lower (p<.01)","No Difference")))
   ) 



# Sex -------------------------------------------------------------------------------------------------------------------------------

sexTest <- datCounty_3year %>%  
   filter(Ndeaths > nCut, !is.na(aRate),sex != "Total" ) %>%
   select(-YLL,-mean.age,-YLLper,-cDeathRate,-rateLCI,-rateUCI,-YLL.adj.rate,LCI=aLCI,UCI=aUCI)

# LOWEST ---

sexTest2 <- sexTest %>% group_by(county,yearG3,CAUSE) %>%
   mutate(bestRate = min(aRate),
          bestSE   = aSE)  %>%
   filter(bestRate == aRate) %>%
   mutate(lowRace = sex) %>%
   select(-(Ndeaths:aSE),-sex)


sexTest_LOW <- left_join(sexTest,sexTest2,by=c("county","yearG3","CAUSE")) %>%
   mutate(rateRatio = round(aRate/bestRate,1),
          Ztest = (aRate - bestRate) / sqrt(aSE^2 + bestSE^2),
          pValue = 1-pnorm(Ztest),
          pMark  = as.factor(ifelse(aRate == bestRate,"Lowest", ifelse(pValue < .01,"Sig. Higher (p<.01)","No Difference")))
   ) 



# HIGHEST ---

sexTest2 <- sexTest %>% group_by(county,yearG3,CAUSE) %>%
   mutate(bestRate = max(aRate),
          bestSE   = aSE)  %>%
   filter(bestRate == aRate) %>%
   mutate(lowRace = sex) %>%
   select(-(Ndeaths:aSE),-sex)


sexTest_HIGH <- left_join(sexTest,sexTest2,by=c("county","yearG3","CAUSE")) %>%
   mutate(rateRatio = round(aRate/bestRate,1),
          Ztest = (aRate - bestRate) / sqrt(aSE^2 + bestSE^2),
          pValue = pnorm(Ztest),
          pMark = as.factor(ifelse(aRate==bestRate,"Highest",ifelse(pValue < .01,"Sig. Lower (p<.01)","No Difference")))
) 


# ------------------------------------------------------------------------------------

disparity <- function(myLHJ="CALIFORNIA",myCause="A",myCompare="lowest rate") {


#--RACE ------------------------------------------------------------------------------------------------------------------------

if(myCompare == "highest rate") {
raceTest <- raceTest_HIGH
fillColor <- c("Highest" = highColor, "Sig. Lower (p<.01)" = lowColor, "No Difference" = midColor)
}

if(myCompare == "lowest rate") {
raceTest <- raceTest_LOW
fillColor <- c("Lowest" = lowColor, "Sig. Higher (p<.01)" = highColor, "No Difference" = midColor)
}

myMeasureRace <- "aRate"

dat.1 <- filter(raceTest,county == myLHJ,CAUSE == myCause, yearG3==myYearG3, sex == "Total") %>%
           mutate(raceName = raceNameFull[match(raceCode,raceCodeFull)] )

if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those or all data are supressed because of SMALL NUMBERS")

placeLabels  <- max(dat.1$aRate)/18
placeLabels2 <- max(dat.1$aRate)/8


racePlot <- ggplot(data=dat.1, aes(x=raceName, y=aRate,fill=pMark)) +
   geom_bar(stat="identity") +
   
   geom_text(aes(y=placeLabels,label=paste("N =",comma(Ndeaths)))) +
   geom_text(aes(y=placeLabels2,label=paste("RR",number(rateRatio,accuracy = 0.1)))) +
   
   
   
   theme_grey() +   #base_size = myBaseSize
    scale_fill_manual("legend", values = fillColor) +
    geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.1, size=1, position=position_dodge(.9), color="gray") + 
   labs(title="Race/Ethnicity",y = "rate per 100,000 (age-adjusted)") +
      theme(legend.position="bottom",
            legend.spacing.x = unit(10.0, 'points'),  ### EXPLORE THIS for optimal look  - unit(10.0, 'px')
            plot.title=element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize),
            axis.title.y = element_text(size = myTextSize3),
            axis.title.x = element_blank(),
         axis.text.y = element_text(size = myAxisSize),
         axis.text.x = element_text(size = myAxisSize),
      legend.title = element_blank(),
         legend.text = element_text(size = myLegendSize),
         strip.text = element_text(size = myAxisSize)) 
      

 
#--AGE ------------------------------------------------------------------------------------------------------------------------

ageMap     <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/Age Group Standard and US Standard 2000 Population.xlsx"),sheet = "data"))

ageMap$ageG          <- paste(ageMap$lAge,"-",ageMap$uAge)

ageMap$ageOrder      <- paste(ageMap$lAge,"-",ageMap$uAge)
ageMap$ageOrder[1:2] <- paste("",ageMap$ageOrder [1:2])
ageMap$ageOrder[10]  <- "85+"

myMeasure <- "cDeathRate"

if(myCompare == "highest rate") {
   ageTest <- ageTest_HIGH
   fillColor <- c("Highest" = highColor, "Sig. Lower (p<.01)" = lowColor, "No Difference" = midColor)
}

if(myCompare == "lowest rate") {
   ageTest <- ageTest_LOW
   fillColor <- c("Lowest" = lowColor, "Sig. Higher (p<.01)" = highColor, "No Difference" = midColor)
}

dat.1 <- filter(ageTest,county == myLHJ,CAUSE == myCause, yearG3==myYearG3, sex == "Total") %>%
           left_join(ageMap,by="ageG")  

AGEplaceLabels <- max(dat.1$cDeathRate)/15 # Note, 10 not 15...  # TRIAL AND ERROR CRAP....

if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those or all data are supressed because of SMALL NUMBERS")

agePlot <- ggplot(data=dat.1, aes(x=ageOrder, y= cDeathRate, fill=pMark)) +
   geom_bar(stat="identity") +
   geom_text(aes(y=AGEplaceLabels, label=comma(Ndeaths))) +
   theme_grey() +   #base_size = myBaseSize
   scale_fill_manual("legend", values = fillColor) +
   geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.1, size=1, position=position_dodge(.9), color="gray") +
   
   labs(title = "Age Groups", y = "rate per 100,000 (age-specific)") +
   
   theme(legend.position="bottom",
         plot.title=element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize),
         axis.title.y = element_text(size = myTextSize3), axis.text.y = element_text(size = myAxisSize),
         axis.title.x = element_blank(),                  axis.text.x = element_text(size = myAxisSize),
         legend.title = element_blank(),                  legend.text = element_text(size = myLegendSize)
   )
  

#--SEX ------------------------------------------------------------------------------------------------------------------------

myMeasureRace <- "aRate"  # works...

if(myCompare == "highest rate") {
   sexTest <- sexTest_HIGH
   fillColor <- c("Highest" = highColor, "Sig. Lower (p<.01)" = lowColor, "No Difference" = midColor)
}

if(myCompare == "lowest rate") {
   sexTest <- sexTest_LOW
   fillColor <- c("Lowest" = lowColor, "Sig. Higher (p<.01)" = highColor, "No Difference" = midColor)
}

dat.1 <- filter(sexTest,county == myLHJ,CAUSE == myCause, yearG3==myYearG3) 
placeLabels <- max(dat.1$aRate)/15

if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those or all data are supressed because of SMALL NUMBERS")

myTit <-  "Sex (age-adjusted rate)"

sexPlot <- ggplot(data=dat.1, aes(x=sex, y=eval(parse(text=paste0(myMeasureRace))),fill=pMark)) +
   geom_bar(stat="identity") +
   geom_text(aes(y=placeLabels,label=comma(Ndeaths))) +
   theme_grey() +   #base_size = myBaseSize
   scale_fill_manual("legend", values =fillColor) +
  # guides(fill = guide_legend(reverse=TRUE)) +
   geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.1, size=1, position=position_dodge(.9), color="gray") + 
   labs(y = deathMeasuresNames[deathMeasures == myMeasureRace], x="Sex") +
   
labs(title = "Sex", y = "rate per 100,000 (age-adjusted)") +
   
   theme(legend.position="bottom",
         plot.title=element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize),
         axis.title.y = element_text(size = myTextSize3), axis.text.y = element_text(size = myAxisSize),
         axis.title.x = element_blank(),                  axis.text.x = element_text(size = myAxisSize),
         legend.title = element_blank(),                  legend.text = element_text(size = myLegendSize)

)


#------------------------------------------------------------------------------------
library(cowplot)
# https://wilkelab.org/cowplot/articles/plot_grid.html

mainTitle <- ggdraw() + 
   draw_label(paste0("Disparities in Deaths Rates, ", fullCauseList[fullCauseList[,"LABEL"]== myCause,"nameOnly"]," in ",myLHJ,", ",myYearG3), colour=myTitleColor, size=myTitleSize,fontface = "bold")


topRow <- cowplot::plot_grid(racePlot,sexPlot,rel_widths = c(5,2))
dPlot   <- plot_grid(mainTitle,topRow,agePlot,ncol=1,rel_heights = c(1,5,5))

list(plot=dPlot)
 }
