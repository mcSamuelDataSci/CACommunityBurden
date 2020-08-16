tempDat        <- readRDS(path(myPlace,"/myData/",whichData,"datCounty65_RE.RDS"))


raceNameFull[6] <- "Native Hawaiian/Pac. Island."
raceNameFull[10] <- "Latino"
raceNameFull[4] <- "American Indian/Alaska Native"


tempDat  <- filter(tempDat, county == "CALIFORNIA", CAUSE == "C02", sex=="Total") %>%
  filter(! raceCode %in% c("-missing","Multi-NH","Unk-NH","Other-NH")) %>%
  mutate(raceName = raceNameFull[match(raceCode,raceCodeFull)] )   

tempDat$year <- yMid[match(tempDat$yearG3,yRange)]


yRange     <- chartYearMap$yearGroup3
yMid       <- chartYearMap$midYear3
myLabels   <- yRange
myBreaks   <- yMid

ggplot(data=tempDat, aes(x=year, y=aRate,color=raceName)) + geom_line(size=2, show.legend=FALSE)  +
  scale_x_continuous(minor_breaks=myBreaks, breaks=myBreaks, expand=c(0,3), labels=myLabels) +
  scale_y_continuous(limits = c(0, NA)) +
  geom_dl(aes(label = raceName), method = list(dl.trans(x = x + 0.3), "last.points", cex=1, 'last.bumpup',font="bold")) +
  labs(y="age-adjusted death rate") +
  theme_bw()





ggplot(data=tempDat, aes(x=year, y=aRate,color=raceName)) + geom_line(size=2, show.legend=FALSE)  +
  scale_x_continuous(minor_breaks=myBreaks, breaks=myBreaks, expand=expand_scale(mult = c(0, 0.6), 
                                                                                 add = c(1, 0)), labels=myLabels) +
  scale_y_continuous(limits = c(0, NA)) +
  geom_dl(aes(label = raceName), method = list(dl.trans(x = x + 0.3), "last.points", cex=1.2, 'last.bumpup',font="bold")) +
  labs(y="age-adjusted death rate") +
  theme_bw()


raceName         <- raceNameFull
raceColors        <- c("black","cyan","darkgreen","blue","firebrick","orange","black","black","black","purple")
#brewer.pal(length(raceNames), "Paired")
names(raceColors) <- raceName






ggplot(data=tempDat, aes(x=year, y=aRate,color=raceName)) + geom_line(size=2, show.legend=FALSE)  +
  scale_x_continuous(minor_breaks=myBreaks, breaks=myBreaks, expand=expand_scale(mult = c(0.3, 0), 
                                                                                 add = c(1, 1)), labels=myLabels) +
  scale_y_continuous(limits = c(0, NA)) +
  geom_dl(aes(label = raceName), method = list(dl.trans(x = x - 0), "first.points", cex=1.2, 'first.bumpup',font="bold")) +
  labs(y="age-adjusted death rate") +
  scale_color_manual(values = raceColors) +
  theme_bw()






scale_x_continuous(limits = c(1, 7), 
                   expand = 






tplot <-  ggplot(data=dat.1, 
                 aes(x=year, y=get(myMeasure), color=get(myVARIABLE))) +
  
  geom_line(size=myLineSize, show.legend=FALSE)  +
  geom_point(shape = myPointShape, size=myPointSize, show.legend=FALSE)  +
  #geom_dl(method = list(box.color = NA, "angled.boxes")) +
  scale_x_continuous(minor_breaks=myBreaks, breaks=myBreaks, expand=c(0,3), labels=myLabels) +
  scale_y_continuous(limits = c(0, NA)) +
  
  geom_dl(aes(label = get(myLineLabel)), method = list(dl.trans(x = x + 0.2), "last.points", cex=myCex1, 'last.bumpup',font="bold")) +
  geom_dl(aes(label = get(myLineLabel)), method = list(dl.trans(x = x - 0.2), "first.points",cex=myCex1,'first.bumpup' ,font="bold"))  +
  
  labs(title = myTitle,
       y = deathMeasuresNames[deathMeasures == myMeasure]
  ) +
  theme_bw(
    base_size   = myAxisSize) +
  theme(
    plot.title  = element_text(family='', face='bold', colour=myTitleColor, size=myTitleSize),
    axis.title  = element_text(face="bold",size=myTextSize3),
    axis.text   = element_text(size=myTextSize2),
    axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1,)
  ) 
