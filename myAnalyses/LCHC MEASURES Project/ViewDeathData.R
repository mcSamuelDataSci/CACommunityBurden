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

 myPlace      <- "f:/0.CBD/myCBD"
 
 
 gbdMap0        <- as.data.frame(read_excel( path(myPlace,"myInfo//gbd.ICD.Map.xlsx/"), sheet="main"))    
 fullCauseList  <- gbdMap0[!is.na(gbdMap0$causeList),c("LABEL","nameOnly")] %>% 
                     select(CAUSE = LABEL, causeName = nameOnly) 
 
 
 datCounty    <- readRDS(path(myPlace,"/myData/real/datCounty.RDS")) %>%
                     left_join(fullCauseList, by = "CAUSE" )
 
 datCounty_RE <- readRDS(path(myPlace,"/myData/real/datCounty_RE.RDS")) %>%
                     left_join(fullCauseList, by = "CAUSE" )
 
 
# --- WORK ---------------------------------------------------------

myLev    <- "lev2"
mySex     <- "Total"
myCounty  <- "CALIFORNIA"
myYear   <- c(2017)
 


#------------------------------------------------------------------

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



#--------------------------------------------------------------------------------------


# CHANGE CODES EVERYWHWERE  !!! no -
#         c("White-NH","Black-NH","AIAN-NH","Asian-NH","NHPI-NH", "Multi-NH","Hisp")
tRace <-  c("White-NH","Black-NH",          "Asian-NH",                      "Hisp")
#tRace  <- c("White-NH","Black-NH","AIAN-NH","Asian-NH","NHPI-NH",            "Hisp")
#!(raceCode %in% c("AIAN-NH","NHPI-NH","Multi-NH")) ) 


work.dat <- datCounty_RE %>% 
               filter(Level==myLev, sex== mySex, county==myCounty,yearG3 == "2015-2017",Level=="lev2",
                      raceCode %in% tRace) 
                                  


.t.2 <- work.dat  %>%  select(causeName,Ndeaths,raceCode) %>%          # N BY RACE + N RANK  (8)
  spread(key=raceCode,value=Ndeaths) %>%
  mutate(AsianRank = round(rank(-`Asian-NH`)),
         BlackRank = round(rank(-`Black-NH`)),
         HispRank  = round(rank(-`Hisp`)),
         WhiteRank = round(rank(-`White-NH`))
  )



t.3 <- work.dat %>%  select(causeName,aRate,raceCode) %>%           # ADJUSTED RATE BY RACE + RANK + RATIO (12)
  spread(key=raceCode,value=aRate) %>%
  mutate(AsianRank = round(rank(-`Asian-NH`)),
         BlackRank = round(rank(-`Black-NH`)),
         HispRank  = round(rank(-`Hisp`)),
         WhiteRank = round(rank(-`White-NH`)),
         minRate = pmin(`Asian-NH`,`Black-NH`,`Hisp`,`White-NH`,na.rm = TRUE),
         Aratio  = `Asian-NH`/minRate,
         Bratio  = `Black-NH`/minRate,
         Hratio  = `Hisp`    /minRate,
         Wratio  = `White-NH`/minRate) %>%
  select(-minRate)  %>% ungroup()



t.X <- t.3 %>% 
          mutate(maxRatio = pmax(Aratio,Bratio,Hratio,Wratio,na.rm=TRUE),
                 rankRatio = round(rank(-maxRatio))
                 ) %>%
          arrange(rankRatio) %>%
          filter(rankRatio < 16) %>%   
         select(causeName,maxRatio)

t.5      <- bind_cols(.t.2,.t.3) 


#write_csv(outTable,"outTable.csv",append=F)  
write.csv(outTable,"outTable.csv")  








