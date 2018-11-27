rankCauseSex  <- function(myLHJ="CALIFORNIA",myMeasure = "YLL",myYear=2017,myLev="lev1",myN=10,mySex="Total") {
  
  
  if (1==2) {
    myPlace   <- getwd()  
    whichData <- "fake"
    
    myLev="lev2"
    myLHJ = "CALIFORNIA"
    myN = 10
    myMeasure = "YLLper"
    myYear = 2015
    mySex = "Total"
    library(fs)
    library(ggplot2)
    library(dplyr)
    
    #datCounty <- readRDS(path("e:","0.CBD/myCBD/","/myData/",whichData,"datCounty.RDS"))
    datCounty <- readRDS(path(myPlace,"/myCBD/","/myData/",whichData,"datCounty.RDS"))
  }
  
# Filtering dat.1 and merging cause name
  
  inDat <- datCounty
  dat.1 <- filter(inDat,county==myLHJ,year==myYear,sex != "Total",Level %in% myLev,CAUSE !=0)
  dat.1 <- dat.1[order( eval(parse(text=paste0("dat.1$",myMeasure)))),]
  
  nR    <- nrow(dat.1)
  myNX  <- min(nR,myN) 
  dat.1 <- dat.1[((nR-myNX):nR),]
  dat.1$info <- dat.1[,myMeasure]
  
  dat.1$causeName <- causeList36[match(dat.1$CAUSE,causeList36[,"LABEL"]),"nameOnly"]
  
# Creating order column 
  
         if (mySex=="Female") #{dat.1       <- dat.1[with(dat.1,order(sex,-info)),]
                              # dat.1$order <- rank(factor(dat.1$info,levels = dat.1$info))
                              {dat.1           <- dat.1 %>%  group_by(CAUSE,add=T) %>%
                               mutate(sum.info = sum(info)) %>%
                               arrange(-sum.info,sex) %>%
                               transform(order=match(info,unique(info)))
  } else if (mySex=="Male")   #{dat.1 <- dat.1[with(dat.1,order(sex,info,decreasing=TRUE)),]
                              # dat.1$order <- rank(factor(dat.1$info,levels = dat.1$info))
      
                              {dat.1           <- dat.1 %>%  arrange(desc(sex),-info) %>%
                               transform(order=match(info,unique(info))) %>%
                               arrange(CAUSE,desc(sex)) #%>%
                               #transform(order=match(info,unique(info)))
                               ##  dat.1$order[duplicated(dat.1$CAUSE)] <- 
                              
  } else if (mySex=="Total")  {dat.1 <- dat.1 %>%  group_by(CAUSE,add=T) %>%
                               
                               mutate(sum.info = sum(info)) %>%
                               arrange(-sum.info) %>%
                               transform(order=match(sum.info,unique(sum.info)))
                               #dat.1$order <- factor(dat.1$order,levels = dat.1$order)
  }
  
  
  #dat.1$order[dat.1$CAUSE==dat.1$CAUSE[!duplicated(dat.1$CAUSE)]]
  #dat.1$order[first(dat.1$CAUSE),]
  #rank(dat.1$order)

# Creating Plot
 ## dat.1$value<-dat.1$info
 ## dat.1$info<-dat.1$order

  # Creating Title variable
  myMeasureAlias <-  lMeasuresC[lMeasures==myMeasure]

  # Plot (https://drsimonj.svbtle.com/ordering-categories-within-ggplot2-facets)
  g <- ggplot(dat.1, aes(x=order,y=info)) +
    labs(title=paste(myMeasureAlias,"by Cause Grouped by Gender,\n","California",myYear),
         y=paste("\n",myMeasureAlias)) +
    facet_grid( ~ sex) +
    scale_x_reverse(breaks = dat.1$order,labels = dat.1$causeName) +
    geom_col(colour = "white", fill="#428BCA") +  #alternative matching blue #0000FF
    #scale_x_discrete(labels= paste(dat.1$CAUSE))  +
    theme(text=element_text(size=20),
          plot.title = element_text(colour= "black", size=20, face="bold",hjust=0.5),
          axis.text = element_text(colour = "black", size=12),
          axis.title.x = element_text(size=12),
          axis.title.y = element_blank()
          ,panel.background = element_rect(fill = "white", colour="grey50")
    )
  
  g  + coord_flip() # scale_x_reverse()
  
}
