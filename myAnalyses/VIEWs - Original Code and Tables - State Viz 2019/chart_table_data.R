# =============================================================================
#   Michael Samuel
#   2019
#
# =============================================================================

#-- Load Packages ------------------------------------------------------------

 library(dplyr)
 library(tidyr)
 library(readxl)
 library(readr) 
 library(fs)
library(stringr)

#-- Key Constants -----------------------------------------------------------

 myPlace   <- "f:/0.CBD/myCBD"
 datCounty <- readRDS(path(myPlace,"/myData/","real","datCounty.RDS"))
 
 gbdMap0        <- as.data.frame(read_excel( path(myPlace,"myInfo//gbd.ICD.Map.xlsx/"), sheet="main"))    
 fullCauseList  <- gbdMap0[!is.na(gbdMap0$causeList),c("LABEL","nameOnly")] %>% 
                     select(CAUSE = LABEL, causeName = nameOnly) 


datCounty      <-  left_join(datCounty,fullCauseList, by = "CAUSE" )

# --- WORK ---------------------------------------------------------

myLev    <- "lev2"
mySex     <- "Total"
myCounty  <- "CALIFORNIA"
myYear   <- c(2017)
 
work.dat <- datCounty %>% filter(Level==myLev, sex== mySex, county==myCounty,year %in% myYear) %>% 
                          select(causeName,Ndeaths,YLLper)  %>%
                          mutate(deaths.rank = round(rank(-Ndeaths)),
                                 yll.rank    = round(rank(-YLLper)))
 
t1 <-  work.dat %>% filter(deaths.rank < 16) %>% arrange(deaths.rank)  %>% select(causeName,Ndeaths) 
t2 <-  work.dat %>% filter(yll.rank    < 16) %>% arrange(yll.rank)     %>% select(causeName,YLLper) 

t3 <- datCounty %>% 
        filter(Level==myLev, sex== mySex, county==myCounty,year %in% c(2017,2007))  %>%
        select(year,causeName,aRate) %>%
        spread(key=year,value=aRate) %>%
        mutate(perChange = round((100*(`2017`-`2007`)/`2007`),1)  ) %>%
        mutate(change.rank  = rank(-perChange)) %>%
        arrange(change.rank) %>%
        filter(change.rank < 16) %>%   
        select(causeName,perChange) 
   
outTable <- cbind(t1,t2,t3)


#write_csv(outTable,"outTable.csv",append=F)  
write.csv(outTable,"outTable.csv")  

