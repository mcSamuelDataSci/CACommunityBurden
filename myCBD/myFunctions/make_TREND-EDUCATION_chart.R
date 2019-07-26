
if(1==2){
  myLHJ="CALIFORNIA" 
  myCause="0"
  myMeasure = "cDeathRate"
  mySex   = "Total"
  myLogTrans=FALSE
}


eduMap     <- as.data.frame(read_csv(paste0(myPlace,"/myInfo/Education Codes and Names.csv")))




#TEMP
#datCounty_EDU$eduCode <- as.numeric(datCounty_EDU$eduCode )



datCounty_EDU <- left_join(datCounty_EDU,eduMap,by="eduCode")



trendRace <- function(myLHJ="CALIFORNIA",myCause="A",myMeasure = "cDeathRate",myLogTrans=FALSE) {

minYear <- 2012
maxYear <- 2017

myCex <- 1.6
myCol <- "blue"            #mycol <- rep("blue",nrow(dat.1))

dat.1 <- filter(datCounty_EDU,county == myLHJ,CAUSE == myCause, sex=="Total") 


if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those or all data are supressed because of SMALL NUMBERS")

myTit <- paste0("Trend in ",deathMeasuresNames[deathMeasures == myMeasure]," of ",fullCauseList[fullCauseList[,"LABEL"]== myCause,"nameOnly"]," in ",myLHJ," by RACE/ETHNIC Group*, ",minYear," to ",maxYear)
myTit <-  wrap.labels(myTit,80)

myTit <- "TEMP"

mySize1 <- 18
mySize2 <- 20
myCex1  <- 1.5

yRange     <- minYear:maxYear
yMid       <- minYear:maxYear


myTrans    <- ifelse(myLogTrans,'log2','identity')
myMin      <- ifelse(myLogTrans,NA,0)

 ggplot(data=dat.1, aes(x=year, y=eval(parse(text=paste0(myMeasure))), group=eduName, color=eduName))  +
    geom_line(size=2)  + geom_point()  +
       scale_x_continuous(minor_breaks=yMid,breaks=yMid,expand=c(0,3),labels=yRange) +
    # scale_x_continuous(minor_breaks=yMid,breaks=yMid,labels=yRange) +
    #   expand_limits(x = c(0, .2)) +
    scale_y_continuous(limits = c(myMin, NA),trans=myTrans) + 
   #,,limits = c(1, NA) trans=myTrans
    scale_colour_discrete(guide = 'none') +   # removed legend 
    labs(y = myMeasure)  + 
    geom_dl(aes(label = eduName), method = list(dl.trans(x = x + 0.2), "last.points", cex=myCex1, font="bold")) +
    geom_dl(aes(label = eduName), method = list(dl.trans(x = x - 0.2), "first.points",cex=myCex1, font="bold"))  +
    labs(title =myTit,size=mySize2) +
    labs(y = deathMeasuresNames[deathMeasures == myMeasure]) +
    theme_bw() +
    theme(axis.text=element_text(size=mySize1),
          axis.title=element_text(size=mySize1,face="bold"),
          plot.title=element_text(family='', face='bold', colour='black', size=myTitleSize, color=myTitleColor),
          axis.text.x = element_text(angle = 0,vjust = 0.5, hjust=.5),
          plot.caption = element_text(hjust = 0, face = "italic",size=14)) 
  
 
 
 #theme(axis.text.x = element_text(angle = 60, hjust = 1))
 
 
 }
