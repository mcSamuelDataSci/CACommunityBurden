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
 
 
 datCounty <- readRDS(path(myPlace,"/myData/",whichData,"datCounty.RDS"))
 datTract  <- readRDS(path(myPlace,"/myData/",whichData,"datTract.RDS"))
 
 
 gbdMap0        <- as.data.frame(read_excel( path(myPlace,"myInfo//gbd.ICD.Map.xlsx/"), sheet="main"))    #extra "/" as examples
 fullCauseList  <- gbdMap0[!is.na(gbdMap0$causeList),c("LABEL","nameOnly")] %>% arrange(LABEL)
 
 datCountyREVIEW <- right_join(fullCauseList,datCounty,by=c("LABEL"="CAUSE"))
 write_csv(datCountyREVIEW,"datCountyREVIEW.csv")
 
 datTractREVIEW <- right_join(fullCauseList,datTract,by=c("LABEL"="CAUSE"))
 write_csv(datTractREVIEW,"datTractREVIEW.csv")
 
# --- WORK ---------------------------------------------------------

 gbdMap0    <- as.data.frame(read_excel( path(myPlace,"myInfo//gbd.ICD.Map.xlsx/"), sheet="main")) 
 
 fullCauseList       <- gbdMap0[!is.na(gbdMap0$causeList),c("LABEL","nameOnly")] %>% arrange(LABEL)
 
 
work.dat <- datCounty %>% filter(Level=="lev2",sex=="Total",county=="CALIFORNIA",year %in% 2015:2017) %>%
                            group_by(CAUSE) %>%
                            summarize(nDeath= sum(Ndeaths))
                          

temp <- right_join(fullCauseList,work.dat,by=c("LABEL"="CAUSE"))


%>%
  select(-LABEL,-CAUSE1,-CAUSE2,-CAUSE3)
table(work.dat$CAUSE,useNA = "ifany")


%>% 
                          select(year,CAUSE,Ndeaths,aRate,YLLper,mean.age)  %>%
                          group_by(year) %>%
                          mutate(rate.rank = round(rank(-aRate)),
                                 yll.rank  = round(rank(-YLLper)))
 
 
 