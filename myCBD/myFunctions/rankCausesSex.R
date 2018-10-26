rankCauseSex  <- function(myLHJ="CALIFORNIA",myMeasure = "YLL",myYear=2017,myN=10,mySex="Total") {

  
  if (1==2) {
  myPlace   <- getwd()  
  whichData <- "fake"
  
  myLev="lev2"
  myLHJ = "CALIFORNIA"
  myN = 10
  myMeasure = "YLL"
  myYear = 2015
  library(fs)
  library(ggplot2)
  library(dplyr)
  
   datCounty <- readRDS(path("e:","0.CBD/myCBD/","/myData/",whichData,"datCounty.RDS"))
  }
  
  inDat <- datCounty
  dat.1 <- filter(inDat,county==myLHJ,year==myYear,sex != "Total",Level %in% myLev,CAUSE !=0)
  #dat.1 <- filter(inDat,county==myLHJ,year==myYear,sex != "Total",CAUSE !=0)
  
  dat.1 <- dat.1[order( eval(parse(text=paste0("dat.1$",myMeasure)))),]

   
   
  # ok to remove. sorting handled in ggplot
  # if (myMeasure=="mean.age"){
  #    dat.1 <- dat.1[order( eval(parse(text=paste0("dat.1$",myMeasure))),decreasing=TRUE),]}
  
  nR    <- nrow(dat.1)
  myNX  <- min(nR,myN) 
  dat.1 <- dat.1[((nR-myNX):nR),]
  
  dat.1$info <- dat.1[,myMeasure]
  

 # https://drsimonj.svbtle.com/ordering-categories-within-ggplot2-facets
  
  
 # ok to remove. sorting handled in ggplot  
 # dat.1$CAUSE <- factor(dat.1$CAUSE, levels = dat.1$CAUSE[order(dat.1$info)])
  
  
  
  dat.1 <- dat.1[with(dat.1,order(info,CAUSE)),]
  dat.1$order <- rank(factor(dat.1$info,levels = unique(dat.1$info)))
  
  dat.1$causeName <- causeList36[match(dat.1$CAUSE,causeList36[,"LABEL"]),"nameOnly"]
   

  myMeasureAlias <-        if (myMeasure=="YLL"){ "Years of Life Lost"
                    } else if (myMeasure=="YLLper") { "Years of Life Lost (per 100,000 population)"
                    } else if (myMeasure=="YLL.adj.rate") {"Age-Adjusted YLL Rate"
                    } else if (myMeasure=="Ndeaths") {"Number of Deaths"
                    } else if (myMeasure=="cDeathRate") {"Crude Death Rate"
                    } else if (myMeasure=="aRate") {"Age-Adjusted Death Rate"
                    } else if (myMeasure=="mean.age") {"Mean Age at Death"
                    } else if (myMeasure=="SMR") {"Standard Mortality Ratio"
                    }
  
  g <- ggplot(dat.1, aes(x=reorder(causeName, info),y=info,group=sex)) +
       labs(title=paste(myMeasureAlias,"by Cause Grouped by Gender,\n","California",myYear),
                y=paste("\n",myMeasureAlias)) +
       facet_grid( ~ sex) + 
       geom_col(colour = "white", fill="#428BCA") +  #alternative matching blue #0000FF
       #scale_x_discrete(labels= paste(dat.1$CAUSE))  +
       theme(text=element_text(size=20),
             plot.title = element_text(colour= "black", size=20, face="bold",hjust=0.5),
             axis.text = element_text(colour = "black", size=12),
             axis.title.x = element_text(size=12),
             axis.title.y = element_blank()
             #,panel.background = element_rect(fill = "white", colour="grey50")
                          )
  
  g  + coord_flip()

}