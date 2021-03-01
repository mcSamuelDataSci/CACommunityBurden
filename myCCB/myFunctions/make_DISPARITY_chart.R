if(1==2){
  myLHJ="CALIFORNIA" 
  myCause="0"
  myMeasure = "aRate"
  mySex   = "Total"
  myLogTrans=FALSE
  myMultiRace = FALSE
  myCompare = "highest rate"
  }


lowColor  <- "palegreen"
midColor  <- "paleturquoise"
highColor <- "tomato"



# ------------------------------------------------------------------------------------

disparity <- function(myLHJ="CALIFORNIA",myCause="A",myCompare="lowest rate",myAddN,myAddRR,myAddRate) {

mySmaller <- 0.8   
myAxisSize <- myAxisSize  * mySmaller
myTextSize3 <-  myTextSize3 * mySmaller
myLegendSize <- myLegendSize * mySmaller


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

dat.1 <- filter(raceTest,county == myLHJ,causeCode == myCause, yearG3==myYearG3, sex == "Total")
dat.1 <- left_join(dat.1,raceLink,by="raceCode")


if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those or all data are supressed because of SMALL NUMBERS")


tMax <- max(dat.1$aRate)
placeLabels  <- tMax/5 
placeLabels2 <- tMax/8 
placeLabels3 <- tMax/20




racePlot <- ggplot(data=dat.1, aes(x=raceNameShort, y=aRate,fill=pMark)) +
   geom_bar(stat="identity") +
   
   
   theme_grey() +   #base_size = myBaseSize
    scale_fill_manual("legend", values = fillColor) +
    geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.1, size=1, position=position_dodge(.9), color="gray") + 
   labs(title="Race/Ethnicity",y = "rate per 100,000 (age-adjusted)") +
      theme(legend.position="bottom",
            legend.spacing.x = unit(10.0, 'points'),  ### EXPLORE THIS for optimal look  - unit(10.0, 'px')
            plot.title=element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize),
            axis.title.y = element_text(size = myTextSize3, margin = margin(t=0, r=10, b=0, l= 0), face="bold"),
            axis.title.x = element_blank(),
         axis.text.y = element_text(size = myAxisSize),
         axis.text.x = element_text(size = myAxisSize),
      legend.title = element_blank(),
         legend.text = element_text(size = myLegendSize),
         strip.text = element_text(size = myAxisSize)) 
  

if (myAddN)  racePlot   <- racePlot + geom_text(aes(y=placeLabels,label=paste("N =",comma(Ndeaths)))) 
if (myAddRate) racePlot <- racePlot + geom_text(aes(y=placeLabels2,label=paste("Rate =",number(aRate,accuracy = 0.1))))
if (myAddRR) racePlot   <- racePlot + geom_text(aes(y=placeLabels3,label=paste("RR =",number(rateRatio,accuracy = 0.1))))


 
#--AGE ------------------------------------------------------------------------------------------------------------------------

ageMap     <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/Age Group Standard and US Standard 2000 Population.xlsx"),sheet = "data"))

myMeasure <- "cDeathRate"

if(myCompare == "highest rate") {
   ageTest <- ageTest_HIGH
   fillColor <- c("Highest" = highColor, "Sig. Lower (p<.01)" = lowColor, "No Difference" = midColor)
}

if(myCompare == "lowest rate") {
   ageTest <- ageTest_LOW
   fillColor <- c("Lowest" = lowColor, "Sig. Higher (p<.01)" = highColor, "No Difference" = midColor)
}

dat.1 <- filter(ageTest,county == myLHJ,causeCode == myCause, yearG3==myYearG3, sex == "Total") %>%
              mutate(ageGroup = factor(ageGroup,levels= ageMap$ageLabel))


###KEY new approach here:
#dat.1$ageGroup <- factor(dat.1$ageGroup,levels = ageMap$ageLabel)

tMax <- max(dat.1$cDeathRate)
AGEplaceLabels  <- tMax/5
AGEplaceLabels2 <- tMax/8
AGEplaceLabels3 <- tMax/20


if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those or all data are supressed because of SMALL NUMBERS")

agePlot <- ggplot(data=dat.1, aes(x=ageGroup, y= cDeathRate, fill=pMark)) +
   geom_bar(stat="identity") +
   theme_grey() +   #base_size = myBaseSize
   scale_fill_manual("legend", values = fillColor) +
   geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.1, size=1, position=position_dodge(.9), color="gray") +
   
   labs(title = "Age Groups", y = "rate per 100,000 (age-specific)") +
   
   theme(legend.position="bottom",
         plot.title=element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize),
         axis.title.y = element_text(size = myTextSize3, margin = margin(t=0, r=10, b=0, l= 0), face="bold"),
         axis.text.y = element_text(size = myAxisSize),
         axis.title.x = element_blank(),                  axis.text.x = element_text(size = myAxisSize),
         legend.title = element_blank(),                  legend.text = element_text(size = myLegendSize)
   ) + 
   scale_y_continuous(labels = comma)




if (myAddN)  agePlot   <- agePlot + geom_text(aes(y=AGEplaceLabels, label=paste("N =",comma(Ndeaths)))) 
if (myAddRate) agePlot <- agePlot + geom_text(aes(y=AGEplaceLabels2,label=paste("Rate =",number(cDeathRate,accuracy = 0.1))))
if (myAddRR) agePlot   <- agePlot + geom_text(aes(y=AGEplaceLabels3,label=paste("RR =",number(rateRatio,accuracy = 0.1))))




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

dat.1 <- filter(sexTest,county == myLHJ,causeCode == myCause, yearG3==myYearG3) 


tMax <- max(dat.1$aRate)
placeLabels  <- tMax/5 
placeLabels2 <- tMax/8 
placeLabels3 <- tMax/20


if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those or all data are supressed because of SMALL NUMBERS")

myTit <-  "Sex (age-adjusted rate)"

sexPlot <- ggplot(data=dat.1, aes(x=sex, y=eval(parse(text=paste0(myMeasureRace))),fill=pMark)) +
   geom_bar(stat="identity") +
   theme_grey() +   #base_size = myBaseSize
   scale_fill_manual("legend", values =fillColor) +
  # guides(fill = guide_legend(reverse=TRUE)) +
   geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.1, size=1, position=position_dodge(.9), color="gray") + 
   labs(y = deathMeasuresNames[deathMeasures == myMeasureRace], x="Sex") +
   
labs(title = "Sex", y = "rate per 100,000 (age-adjusted)") +
   
   theme(legend.position="bottom",
         plot.title=element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize),
         axis.title.y = element_text(size = myTextSize3,margin = margin(t=0, r=10, b=0, l= 10), face="bold"), axis.text.y = element_text(size = myAxisSize),
         axis.title.x = element_blank(),                  axis.text.x = element_text(size = myAxisSize),
         legend.title = element_blank(),                  legend.text = element_text(size = myLegendSize)

)

if (myAddN)  sexPlot <- sexPlot + geom_text(aes(y=placeLabels,label=paste("N =",comma(Ndeaths)))) 
if (myAddRate) sexPlot <- sexPlot + geom_text(aes(y=placeLabels2,label=paste("Rate =",number(aRate,accuracy = 0.1))))
if (myAddRR) sexPlot <- sexPlot + geom_text(aes(y=placeLabels3,label=paste("RR =",number(rateRatio,accuracy = 0.1))))



#------------------------------------------------------------------------------------







#------------------------------------------------------------------------------------
library(cowplot)
# https://wilkelab.org/cowplot/articles/plot_grid.html

mainTitle <- ggdraw() + 
   draw_label(paste0("Disparities in Deaths Rates, ", deathCauseLink$causeName[deathCauseLink$causeCode== myCause]," in ",myLHJ,", ",myYearG3), colour=myTitleColor, size=myTitleSize,fontface = "bold")


topRow <- cowplot::plot_grid(racePlot,sexPlot,rel_widths = c(6,3))
dPlot   <- plot_grid(mainTitle,topRow,agePlot,ncol=1,rel_heights = c(1,5,5))

list(plot=dPlot)
 
}
