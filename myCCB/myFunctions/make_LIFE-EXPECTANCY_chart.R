# server <- T
# if (!server) source("g:/FusionData/Standards/FusionStandards.R")
# if (server) source("/mnt/projects/FusionData/Standards/FusionStandards.R")



geoMap          <- as.data.frame(read_excel(paste0(ccbInfo,"/County Codes to County Names Linkage.xlsx"))) %>%
                     select(FIPSCounty,county=countyName)

lifeTableCounty <- readRDS(paste0(ccbData,"/e0ciCounty.RDS")) %>%
                     mutate(FIPSCounty=substr(GEOID,3,5))  %>%
                     left_join(geoMap,by="FIPSCounty") %>%
                     mutate(sex = str_to_title(sex))

lifeTableState  <- readRDS(paste0(ccbData,"/e0ciState.RDS")) %>%
                     mutate(county = "CALIFORNIA") %>%
                     mutate(sex = str_to_title(sex))

lifeTableSet   <- bind_rows(lifeTableCounty, lifeTableState) %>%
  left_join(select(raceLink, raceCode, raceNameShort), by = "raceCode")

# FIX MIN and MAX Year in global or other life tables function eventaully
  minYear_LT <- min(lifeTableSet$year)
  maxYear_LT <- max(lifeTableSet$year)

#== FUNCTION ========================================================================================================  
  
  
LEtrend <- function(myLHJ="CALIFORNIA", mySexMult, myRace, myCI, myYearGrouping = 1) {
   
 
#---BAR PART------------------------------------------------------------------------------------------------------
   
  
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
    myRace = c(  "Asian",   "Black",  "Latino",   "White")
    myCI = FALSE
  }  
  
  
  

 dat.1 <- lifeTableSet %>% filter(county==myLHJ, sex %in% mySexMult, raceNameShort %in% myRace) %>% 
             mutate(lineLabel = ifelse(sex == "Total", raceNameShort, paste(raceNameShort,"-",sex)))



 tplot_bar <- ggplot(data=filter(dat.1, year== 2020, nyrs == myYearGrouping), aes(x=raceNameShort, y=ex, fill=sex)) + 
                geom_bar(stat = "identity",position="dodge")  +
                scale_fill_manual(values = genderColors) + 
                labs(x = "Race/Ethnicity", y = "Life Expectancy at Birth", x = "Year") +
               coord_cartesian(ylim=c(65,90)) +
               geom_segment(aes(x = .3, y = 64.8, xend = .5, yend = 65.3),
                            color="red",size=1.2) +
               geom_segment(aes(x = .3, y = 64.4, xend = .5, yend = 64.9),
                    color="red",size=1.2) +
               geom_text(aes(label=format(round(ex, 1), nsmall = 1)), position=position_dodge(width=0.9), vjust=2,fontface="bold", color = 'white')
 
 # 
 # tplot_bar <- ggplot(data=filter(dat.1, year== 2019), aes(x=raceCode, y=ex, fill=sex)) + 
 #   geom_bar(stat = "identity",position="dodge")  +
 #   scale_fill_manual(values = genderColors) + 
 #   labs(x = "Race/Ethnicity", y = "Life Expectancy at Birth", x = "Year") +
 #   coord_cartesian(ylim=c(65,90),  clip="off") +
 #   geom_segment(aes(x = 1.25, y = 70, xend = 2.25, yend = 70), 
 #                arrow=arrow(angle = 45, length = unit(0.08, "inches"), ends = "last") ,
 #                color="blue",size=.5) +
 #   geom_segment(aes(x = 1.25, y = 68, xend = 3.25, yend = 68), 
 #                arrow=arrow(angle = 45, length = unit(0.08, "inches"), ends = "last") ,
 #                color="blue",size=.5) +
 #   geom_segment(aes(x = 1.25, y = 66, xend = 4.25, yend = 66), 
 #                arrow=arrow(angle = 45, length = unit(0.08, "inches"), ends = "last") ,
 #                color="blue",size=.5) +
 #   geom_segment(aes(x = -0.1, y = 64.5, xend = 0.1, yend = 64.9),
 #                color="red",size=1) +
 #   geom_segment(aes(x = -0.1, y = 63.5, xend = 0.1, yend = 63.9),
 #                color="red",size=1) 
 
 
 # coord_cartesian(ylim = c(0, 0.75), clip="off") +
 #   theme(plot.margin = unit(c(1,1,1,0), "lines"))
 

 #---LINE PART------------------------------------------------------------------------------------------------------
 
 myTitle <- paste0("Trend in Life Expectancy, ",myLHJ,", ",minYear_LT,"-",maxYear_LT)
 myTitle <-  wrap.labels(myTitle,myWrapNumber)
 
 
 
 
#  dat.1 <-  filter(dat.1, (county == "CALIFORNIA" & nyrs==1) | (county != "CALIFORNIA" & nyrs == 3))
 
   dat.1 <-  filter(dat.1, nyrs == myYearGrouping)
 
 
 
 
 if (nrow(dat.1)==0) stop("Sorry friend, data are suppressed per the California Health and Human Services Agency Data De-Identification Guidelines, or there are no cases that meet this criteria.")
 
 
 myBreaks <- minYear_LT:maxYear_LT
 myLabels <- myBreaks
 
 
 
 tplot<- ggplot(data=dat.1, aes(x=year, y=ex)) +                     # , nyrs == 1
                 geom_line(size=1.6,aes(color=raceNameShort,linetype=sex)) +
                 # geom_point(shape = 21, size=2.5, aes(color = raceNameShort), fill = "white")  +
                 ylim(62, 93) +
                 scale_x_continuous(minor_breaks=myBreaks,breaks=myBreaks,
                                    expand = expansion(mult = c(0, 0), add = c(1, 5)), # lower-limit: 2000 - (2018 - 2000) * 0 - 1... upper-limit: 2018 + (2018 - 2000) * 0 + 5
                                    #expand=c(0,5), # 
                                    labels=myLabels) +
                 scale_color_manual(values = raceNameShortColors) +   
                 labs(title =myTitle, y = "Life Expectancy at Birth", x = "Year")  +
                 theme_bw() +
                  theme(axis.text=element_text(size=myAxisSize),
                        axis.title=element_text(size=myAxisSize,face="bold"),
                        plot.title=element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize),
                        axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1), 
                        legend.position = "none"
                       ) +    
                  geom_dl(aes(label = lineLabel,color=raceNameShort), method = list(dl.trans(x = x + 0.2), "last.points", 
                                                                            #size = myLineLabelSize, # use this instead of cex
                                                                            cex=myLineLabelCex, 
                                                                            'last.bumpup',font="bold")) 
 
if (myCI) {
    tplot <- tplot +
        geom_line(data=dat.1,aes(x=year, y=exlow, color=raceNameShort,linetype=sex)) +
        geom_line(data=dat.1,aes(x=year, y=exhigh,color=raceNameShort,linetype=sex)) 
 }
     
 
# tplot <- tplot + ylim(62, 93)
 
 
   dat.1 <- dat.1 %>% mutate(ex=round(ex,2), exlow=round(exlow,2),exhigh=round(exhigh,2)) %>%
                      select(county,nyrs,year, sex,race=raceNameShort, LifeExpectancy=ex, LECI_lower = exlow, LECI_upper=exhigh)
 
   list(plotL = tplot, dataL = dat.1, bar=tplot_bar)

 }
 
 
 