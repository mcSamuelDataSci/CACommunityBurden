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
 library(markdown)

#-- Key Constants -----------------------------------------------------------

 myPlace   <- "E:/0.CBD/myCBD"
 
 STATE     <- "CALIFORNIA"
 yearGrp   <- "2013-2017"

 datCounty <- readRDS(path(myPlace,"/myData/","real","datCounty.RDS"))
 
 
 gbdMap0        <- as.data.frame(read_excel( path(myPlace,"myInfo//gbd.ICD.Map.xlsx/"), sheet="main"))    #extra "/" as examples
 fullCauseList  <- gbdMap0[!is.na(gbdMap0$causeList),c("LABEL","nameOnly")] %>% arrange(LABEL)
 

# --- WORK ---------------------------------------------------------

datCounty <-  left_join(datCounty,fullCauseList, by = c("CAUSE" = "LABEL"))
 
 
work.dat <- datCounty %>% filter(Level=="lev2",sex=="Total",county=="CALIFORNIA",year %in% c(2017)) %>% 
                          select(nameOnly,Ndeaths,YLLper)  %>%
                          mutate(deaths.rank = round(rank(-Ndeaths)),
                                 yll.rank  = round(rank(-YLLper)))
 
 
 



 
 
 
 
t1 <-  work.dat %>% filter(deaths.rank < 16) %>% arrange(deaths.rank)  %>% select("Cause" = nameOnly,Ndeaths) 
t2 <-  work.dat %>% filter(yll.rank    < 16) %>% arrange(yll.rank)  %>% select("Cause" = nameOnly,YLLper) 


 
work.dat <- datCounty %>% filter(Level=="lev2",sex=="Total",county=="CALIFORNIA",year %in% c(2017,2007)) 


  t3 <- work.dat  %>%
   select(year,nameOnly,aRate) %>%
   spread(key=year,value=aRate) %>%
   mutate(perChange = round((100*(`2017`-`2007`)/`2007`),1)  ) %>%
   mutate(change.rank  = rank(-perChange)) %>%
   arrange(change.rank)
   
  #############################
    
  ###  filter(change.rank < 16) %>% )   %>% select("Cause" = nameOnly,perChange) 
   

outTable <- cbind(t1,t2,t3)
write.csv(outTable,"outTable.csv",append=F)  
  
  
  
  
  
  
  
