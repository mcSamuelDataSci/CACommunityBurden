# trend <- function(myLHJ="zz California",myCause=61,myMeasure = "YLL",mySex="Total") {
# 
#   minYear <- 2002
#   maxYear <- 2015
#   
#   myCex <- 1.6
#   myCol <- "blue"            #mycol <- rep("blue",nrow(dat.1))
# 
#   inDat <- datCounty
#   dat.1 <- filter(inDat,county==myLHJ,(year >= minYear & year <= maxYear ),CAUSE == myCause,sex==mySex)
#   
#   if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those; could be some other error")
#   
#   plot(dat.1$year,eval(parse(text=paste0("dat.1$",myMeasure))),
#        type="l",lwd=3,col="blue",
#        xlab="Year",ylab=names(lMeasures[lMeasures==myMeasure]),
#        main=paste("Trend in",names(lMeasures[lMeasures==myMeasure]),"of",causeList36[causeList36[,1]==myCause,2],"in",myLHJ)) 
#   
#   
# }
# https://stackoverflow.com/questions/29357612/plot-labels-at-ends-of-lines

library(directlabels)


trend <- function(myLHJ="CALIFORNIA",myCause="A",myMeasure = "YLL") {
  

minYear <- 2002
maxYear <- 2015

myCex <- 1.6
myCol <- "blue"            #mycol <- rep("blue",nrow(dat.1))

inDat <- datCounty
dat.1 <- filter(inDat,county == myLHJ,CAUSE == myCause)

if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those; could be some other error")

#ggplot(data=dat.1, aes(x=year, y=eval(parse(text=(myMeasure))), group=sex)) +
 ggplot(data=dat.1, aes(x=year, y=eval(parse(text=paste0(myMeasure))), group=sex, color=sex)) +
    geom_line(size=2) +
     scale_colour_discrete(guide = 'none') + labs(y = myMeasure) +
     scale_x_continuous(expand=c(0,3)) + scale_y_continuous(limits = c(0, NA)) +
     geom_dl(aes(label = sex), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1.6, font="bold")) +
   geom_dl(aes(label = sex), method = list(dl.trans(x = x - 0.2), "first.points", cex = 1.6, font="bold"))  

    
   
#    scale_x_continuous(limits = c(0, NA))
  
  
  
  
}
