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

 whichData <- "real"
 subSite   <- FALSE
 VERSION   <- "Version B1.3"
 myPlace   <- "E:/0.CBD/myCBD"
 
 
 STATE     <- "CALIFORNIA"
 yearGrp   <- "2013-2017"

 

 # --- CBD Key Inputs ---------------------------------------------------------
 
 allDat  <- readRDS("h:/0.Secure.Data/myData/cbdDat0-INVESTIGATION-FILE.RDS")
 allDat  <- readRDS("g:/CCB/0.Secure.Data/myData/cbdDat0-INVESTIGATION-FILE.RDS")
 
 datTract  <- readRDS(path(myPlace,"/myData/",whichData,"datTract.RDS"))
 datComm   <- readRDS(path(myPlace,"/myData/",whichData,"datComm.RDS"))
 datCounty <- readRDS(path(myPlace,"/myData/",whichData,"datCounty.RDS"))
 
 
 gbdMap0        <- as.data.frame(read_excel( path(myPlace,"myInfo//gbd.ICD.Map.xlsx/"), sheet="main"))    #extra "/" as examples
 fullCauseList  <- gbdMap0[!is.na(gbdMap0$causeList),c("LABEL","nameOnly")] %>% arrange(LABEL)
 
 
 
 
 
# --- WORK ---------------------------------------------------------

 
 
work.dat <- datCounty %>% filter(Level=="lev2",sex=="Total",county=="CALIFORNIA",year %in% c(2017,2010,2000)) %>% 
                          select(year,CAUSE,Ndeaths,aRate,YLLper,mean.age)  %>%
                          group_by(year) %>%
                          mutate(rate.rank = round(rank(-aRate)),
                                 yll.rank  = round(rank(-YLLper)))
 
 
 
 
 # junk <- work.dat %>%
 #   select(year,CAUSE,Ndeaths,aRate,YLLper) %>%
 #   nest(Ndeaths,aRate,YLLper,.key = 'value_col') %>%
 #   spread(key = year, value = value_col) %>%
 #   unnest("2017","2010","2000", .sep = '_')
 # 
 
 
 
 t.age <- work.dat  %>%
   select(year,CAUSE,mean.age) %>%
   spread(key=year,value=mean.age) %>%
   left_join(fullCauseList,by = c("CAUSE"= "LABEL"))

 
#-------------------------------------------------------------------------------------- 
 
 
 t.deaths <- work.dat  %>%
   select(year,CAUSE,Ndeaths) %>%
   spread(key=year,value=Ndeaths) 
 
  t.rate <- work.dat  %>%
   select(year,CAUSE,aRate) %>%
   spread(key=year,value=aRate) 

  names(t.rate)      <- c(names(t.rate)[1],     paste0("rate",names(t.rate)[2:4]))
  
  
t.rate.change  <- mutate(t.rate,
                    change_2000_2017 = round(100*(rate2017-rate2000)/rate2000,1)) %>%
                    select(CAUSE,change_2000_2017)
  
t.rate.rank <- work.dat  %>%
          select(year,CAUSE,rate.rank) %>%
          spread(key=year,value=rate.rank)
 
          
t.yll <- work.dat  %>%
  select(year,CAUSE,YLLper) %>%
  spread(key=year,value=YLLper)

  t.yll.rank <- work.dat  %>%
    select(year,CAUSE,yll.rank) %>%
    spread(key=year,value=yll.rank)
  
  
  names(t.deaths)    <- c(names(t.deaths)[1],   paste0("deaths",names(t.deaths)[2:4]))
  names(t.rate.rank) <- c(names(t.rate.rank)[1],paste0("rankRATE",names(t.rate.rank)[2:4]))
  names(t.yll)       <- c(names(t.yll)[1],      paste0("YLL",names(t.yll)[2:4]))
  names(t.yll.rank)  <- c(names(t.yll.rank)[1], paste0("rankYLL",names(t.yll.rank)[2:4]))
  
  
  
  
  

t.work <- full_join(t.deaths,t.rate.change,by="CAUSE") %>% 
           full_join(t.yll,by="CAUSE") %>%
           full_join(t.yll.rank,by="CAUSE") %>%
          full_join(t.rate,by="CAUSE")  %>%
         full_join(t.rate.rank,by="CAUSE")  


 

 
work.dat <-  full_join(fullCauseList,t.work, by = c("LABEL" = "CAUSE")) %>%
               filter(!is.na(deaths2017))
              
 
  
write.csv(work.dat,"rankingForJN.csv",append=F)  
  
  
  
  
  
  
  
