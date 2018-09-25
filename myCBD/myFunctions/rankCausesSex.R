rankCauseSex  <- function(myLHJ="CALIFORNIA",myMeasure = "YLL",myYear=2017,myN=10,mySex="Total") {

  
  if (1==2) {
  myPlace   <- getwd()  
  whichData <-  "real"
  
  
  myLHJ = "CALIFORNIA"
  myN = 10
  myMeasure = "YLL"
  myMeasure = "aRate"
  myYear = 2015
  library(fs)
  library(ggplot2)
  library(dplyr)
  
  datCounty <- readRDS(path("e:","0.CBD/myCBD/","/myData/",whichData,"datCounty.RDS"))
  }
  
  
  
  
  inDat <- datCounty
  dat.1 <- filter(inDat,county==myLHJ,year==myYear,sex != "Total",CAUSE !=0)
  
   dat.1 <- dat.1[order( eval(parse(text=paste0("dat.1$",myMeasure)))),]

   if (myMeasure=="mean.age"){
      dat.1 <- dat.1[order( eval(parse(text=paste0("dat.1$",myMeasure))),decreasing=TRUE),]}
  
  nR    <- nrow(dat.1)
  myNX  <- min(nR,myN) 
  dat.1 <- dat.1[((nR-myNX):nR),]
  
  dat.1$info <- dat.1[,myMeasure]
  

 # https://drsimonj.svbtle.com/ordering-categories-within-ggplot2-facets
  
  
  
 # dat.1$CAUSE <- factor(dat.1$CAUSE, levels = dat.1$CAUSE[order(dat.1$info)])
  
  g <- ggplot(dat.1, aes(x=CAUSE,y=info,group=sex))
  g + geom_col() +  coord_flip() +
      facet_grid( ~ sex)
  
  
}
  
  
  