rankCause  <- function(myLHJ="CALIFORNIA",myMeasure = "YLL",myYear=2015,mySex="Total",myN=10) {

  myPlace   <- getwd()  
  whichData <-  "real"
  
  
  
  myMeasure = "YLL"
  myMeasure = "aRate"
  
  library(fs)
  library(ggplot2)
  library(dplyr)
  
  datCounty <- readRDS(path("e:","0.CBD/myCBD/","/myData/",whichData,"datCounty.RDS"))
  
  
  myCex <- 1.6
  myCol <- "blue"            #mycol <- rep("blue",nrow(dat.1))

  
  inDat <- datCounty
  dat.1 <- filter(inDat,county==myLHJ,year==myYear,sex != "Total",CAUSE !=0)
  
   dat.1 <- dat.1[order( eval(parse(text=paste0("dat.1$",myMeasure)))),]

   if (myMeasure=="mean.age"){
     dat.1 <- dat.1[order( eval(parse(text=paste0("dat.1$",myMeasure))),decreasing=TRUE),]}
  
  nR    <- nrow(dat.1)
  myNX  <- min(nR,myN) 
  dat.1 <- dat.1[((nR-myNX):nR),]
  
  dat.1$info <- dat.1[,myMeasure]
  
  
  
  g <- ggplot(dat.1, aes(CAUSE,info))
  g + geom_col() +  coord_flip() +
      facet_grid(. ~ sex)
  
  
}
  
  
  