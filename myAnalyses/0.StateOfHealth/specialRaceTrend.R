  myLHJ="CALIFORNIA" 
  myCause="0"
  myMeasure = "aRate"
  mySex   = "Total"
  myLogTrans = FALSE

  # myCex <- 1.6
  # myCol <- "blue"  
  # minYear <- 2000
  # maxYear <- 2017

  myAxisSize = 10
  myTitleSize = 10   
  myLineSize = 1  
  myCex1  = 0.8
    myLineLabel <- "raceName"
    
    myTrans    <- ifelse(myLogTrans,'log2','identity')
    myMin      <- ifelse(myLogTrans,NA,0)    
    
    
    dat.1 <- filter(datCounty_RE,
                    county == myLHJ,
                    CAUSE == myCause, 
                    sex=="Total",
                    raceCode != "Multi-NH") %>%
              mutate(raceName = raceNameFull[match(raceCode,raceCodeFull)] )
    
    yRange     <- chartYearMap$yearGroup3
    yMid       <- chartYearMap$midYear3
    myLabels   <- yRange
    myBreaks   <- yMid
    dat.1$year <- yMid[match(dat.1$yearG3,yRange)]

#### ====================================================================================================

p1 <- ggplot(data=dat.1, aes(x=year, y=aRate, group=raceCode, color=raceCode)) +
          geom_line(size=myLineSize)  +
          scale_x_continuous(minor_breaks=myBreaks,breaks=myBreaks,expand=c(0,3),labels=myLabels) +
          scale_y_continuous(limits = c(0, NA)) +
          scale_colour_discrete(guide = 'none') +   # removed legend
          geom_dl(aes(label = get(myLineLabel)), method = list(dl.trans(x = x + 0.2), "last.points", cex=myCex1, 'last.bumpup',font="bold")) +
          labs(title = "(age adjusted) All-Cause Mortailiy Rate by Race/Ethnicity and Year Group", y = "Rate") +
          theme_bw(base_size   = myAxisSize) +
          theme( plot.title  = element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize),
                 axis.title  = element_text(face="bold"),
                 axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1)) 


dat.2 <- filter(dat.1, raceCode %in% c("Black-NH","White-NH")) 

p2 <- ggplot(data=dat.2, aes(x=year, y=aRate, group=raceCode, color=raceCode)) +
  geom_line(size=myLineSize)  +
  scale_x_continuous(minor_breaks=myBreaks,breaks=myBreaks,expand=c(0,3),labels=myLabels) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_colour_discrete(guide = 'none') +   # removed legend
  geom_dl(aes(label = get(myLineLabel)), method = list(dl.trans(x = x + 0.2), "last.points", cex=myCex1, 'last.bumpup',font="bold")) +
  labs(title = "(age adjusted) All-Cause Mortailiy Rate by Race/Ethnicity and Year Group", y = "Rate") +
  theme_bw(base_size   = myAxisSize) +
  theme( plot.title  = element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize),
         axis.title  = element_text(face="bold"),
         axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1)) 


dat.3 <- filter(dat.1, raceCode %in% c("Black-NH","White-NH")) %>% 
                   select(year,raceCode,aRate) %>%
                   pivot_wider(names_from = raceCode,values_from=aRate) %>%
                   mutate(bwRR = `Black-NH`/`White-NH`,
                          bwRDif = `Black-NH` - `White-NH` )

p3 <- ggplot(data=dat.3, aes(x=year, y=bwRR)) +
  geom_line(size=myLineSize,color="blue")  +
  scale_x_continuous(minor_breaks=myBreaks,breaks=myBreaks,expand=c(0,3),labels=myLabels) +
  scale_y_continuous(limits = c(NA, NA)) +
  scale_colour_discrete(guide = 'none') +   # removed legend
  geom_dl(aes(label = "Rate Ratio"), method = list(dl.trans(x = x + 0.2), "last.points", cex=myCex1, 'last.bumpup',font="bold")) +
  labs(title = "Black: White Rate Ratio") +
  theme_bw(base_size   = myAxisSize) +
  theme( plot.title  = element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize),
         axis.title  = element_text(face="bold"),
         axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1)  ) 

p4 <- ggplot(data=dat.3, aes(x=year, y=bwRDif)) +
  geom_line(size=myLineSize,color="blue")  +
  scale_x_continuous(minor_breaks=myBreaks,breaks=myBreaks,expand=c(0,3),labels=myLabels) +
  scale_y_continuous(limits = c(NA, NA)) +
    scale_colour_discrete(guide = 'none') +   # removed legend
  geom_dl(aes(label = "Rate Difference"), method = list(dl.trans(x = x + 0.2), "last.points", cex=myCex1, 'last.bumpup',font="bold")) +
  labs(title = "Black: White Rate DIFFERENCE") +
  theme_bw( base_size   = myAxisSize) +
  theme(
    plot.title  = element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize),
    axis.title  = element_text(face="bold"),
    axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1)  ) 



p5 <- ggplot(data=dat.3, aes(x=year, y=bwRR)) +
  geom_line(size=myLineSize,color="blue")  +
  scale_x_continuous(minor_breaks=myBreaks,breaks=myBreaks,expand=c(0,3),labels=myLabels) +
  scale_y_continuous(limits = c(1, 1.4)) +
  scale_colour_discrete(guide = 'none') +   # removed legend
  geom_dl(aes(label = "Rate Ratio"), method = list(dl.trans(x = x + 0.2), "last.points", cex=myCex1, 'last.bumpup',font="bold")) +
  labs(title = "Black: White Rate Ratio") +
  theme_bw(base_size   = myAxisSize) +
  theme( plot.title  = element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize),
         axis.title  = element_text(face="bold"),
         axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1)  ) 

p6 <- ggplot(data=dat.3, aes(x=year, y=bwRDif)) +
  geom_line(size=myLineSize,color="blue")  +
  scale_x_continuous(minor_breaks=myBreaks,breaks=myBreaks,expand=c(0,3),labels=myLabels) +
  scale_y_continuous(limits = c(0, 300)) +
  scale_colour_discrete(guide = 'none') +   # removed legend
  geom_dl(aes(label = "Rate Difference"), method = list(dl.trans(x = x + 0.2), "last.points", cex=myCex1, 'last.bumpup',font="bold")) +
  labs(title = "Black: White Rate DIFFERENCE") +
  theme_bw( base_size   = myAxisSize) +
  theme(
    plot.title  = element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize),
    axis.title  = element_text(face="bold"),
    axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1)  ) 







junk <- plot_grid(p1,p2,p3,p4,p5,p6,ncol=2)




