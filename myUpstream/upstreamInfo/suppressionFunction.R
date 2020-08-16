
mySuppress <- function(myDat,gBy,NcolName,critN=11)  {
 
  myDat$myN <- myDat[,NcolName]
  
  myDat <- myDat  %>%   
           mutate(suppress1   = ifelse(myN < critN,1,0) # supress if N < critN
           ) %>%   
           group_by(.dots = gBy) %>%
             mutate(rowsSuppressedA  = sum(suppress1==1,na.rm=TRUE) ,
                     nextSmallestN   = min(myN[myN >= critN],na.rm=TRUE),  # find next smallest N
                     suppress2       = ifelse((rowsSuppressedA== 1 & myN==nextSmallestN),1,0), # suppress complementarly row if only one row suppressed
                     SUPPRESS        = ifelse(suppress1==1 | suppress2==1,1,0) # final supression indicator
           ) 
           
  myDat$SUPPRESS       
 
}


# Comments Etc ================================================================

if (1 == 2) {

 library(dplyr)

 inDat           <- read.csv("f:/0.CBD/myUpstream/upstreamInfo/suppressionTestData.csv")
 gBy             <-  c("Year","Place")
 inDatNew        <- mutate(inDat, supIndictor = mySuppress(inDat,gBy,"events"))
 inDatSupprssed  <- filter(inDatNew,supIndictor != 1)


# for use if experimenting outside of function
  myDat <- inDat %>% mutate(myN = events)
  critN <- 11
   }


# All related to unsucessful work to get varible as input
# library(lazyeval)
# N.column <- enquo(N.column)
# N.column <- as.name(N.column)
# junk <- "events"
