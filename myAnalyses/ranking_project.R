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
 
 
 
 
 t.age <- work.dat  %>%
   select(year,CAUSE,mean.age) %>%
   spread(key=year,value=mean.age) %>%
   left_join(fullCauseList,by = c("CAUSE"= "LABEL"))

 
 
    
 t.deaths <- work.dat  %>%
   select(year,CAUSE,Ndeaths) %>%
   spread(key=year,value=Ndeaths) 
 t.deaths <- t.deaths[,c(1,4,3,2)]
  names(t.deaths) <- c(names(t.deaths)[1],paste0("deaths",names(t.deaths)[2:4]))
 
 
 t.rate.change <- work.dat  %>%
   select(year,CAUSE,aRate) %>%
   spread(key=year,value=aRate) 
  names(t.rate.change) <- c(names(t.rate.change)[1],paste0("x",names(t.rate.change)[2:4]))
 
  
  t.rate.change  <- mutate(t.rate.change,
                    change_2000_2017 = round(100*(x2017-x2000)/x2000,1)) %>%
                    select(CAUSE,change_2000_2017)
  
  
t.rate <- work.dat  %>%
          select(year,CAUSE,rate.rank) %>%
          spread(key=year,value=rate.rank)
t.rate <- t.rate[,c(1,4,3,2)]
names(t.rate) <- c(names(t.rate)[1],paste0("rankRATE",names(t.rate)[2:4]))


t.yll <- work.dat  %>%
  select(year,CAUSE,yll.rank) %>%
  spread(key=year,value=yll.rank)
t.yll <- t.yll[,c(1,4,3,2)]
names(t.yll) <- c(names(t.yll)[1],paste0("rankYLL",names(t.yll)[2:4]))



t.work <- full_join(t.deaths,t.rate.change,by="CAUSE") %>% 
           full_join(t.yll,by="CAUSE") %>%
           full_join(t.rate,by="CAUSE")



 

 
work.dat <-  full_join(fullCauseList,t.work, by = c("LABEL" = "CAUSE")) %>%
               filter(!is.na(deaths2017))
              
 
  
write.csv(work.dat,"rankingForJN.csv",append=F)  
  
  
  
  
  
  
  




 
  
   select(year,rate.rank,nameOnly)
 
 work2.dat <- spread(work.dat,key=year,value=rate.rank)



work3.dat<- work2.dat[,c(1,ncol(work2.dat):2)]

names(work3.dat) <- paste0("V",names(work3.dat))




work4.dat<- work3.dat %>% arrange(V2017)

newdata <- mtcars[order(mpg),] 



 causeNum36        <- fullCauseList[,"LABEL"]
 names(causeNum36) <- fullCauseList[,"causeList" ]
 
 phList   <- fullCauseList[nchar(fullCauseList$LABEL) <= 3,]
 phCode   <- phList[,"LABEL"]
 names(phCode) <- phList[,"causeList" ]
 
 bigList  <- fullCauseList[nchar(fullCauseList$LABEL) == 1,]
 bigCode  <- bigList[,"LABEL"]
 names(bigCode) <- bigList[,"causeList"]
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
# Shapes file: ------------------------------- 
 
# USE consistent map projection system throughout all app code !
 proj1 <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
 proj2 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
 
# Read shape files as simple features objects (st_read is from sf package)
 shape_Tract        <- st_read(path(myPlace,"/myData/shape_Tract.shp"),stringsAsFactors=FALSE)
 shape_Comm         <- st_read(path(myPlace,"/myData/shape_Comm.shp"),stringsAsFactors=FALSE)
 shape_County       <- st_read(path(myPlace,"/myData/shape_County.shp"),stringsAsFactors=FALSE)

# Prior Approaches to reading "shape" files, kept for refereance
# shape_County   <- readOGR(paste0(myPlace,"/myData/shape_County.shp"),p4s=proj1) 
# shape_County   <- st_read(paste0(myPlace,"/myData/shape_County.rds")) # --> very large!
# shape_County   <- readShapePoly(paste0(myPlace,"/myData/shape_County"),proj4string=CRS(proj1)) 

 shape_Tract$GEOID  <- as.character(shape_Tract$GEOID)    
 shape_Tract$county <- as.character(shape_Tract$county)   
 
# Data: ----------------------------------------- 
 

load(path(myPlace,"/myData/","sdohTract.R"))
load(path(myPlace,"/myData/","sdohComm.R"))
load(path(myPlace,"/myData/","sdohCounty.R"))

#-- Load Info Files and Functions ---------------------------------------------
  
  gbdMap0    <- as.data.frame(read_excel( path(myPlace,"myInfo//gbd.ICD.Map.xlsx/"), sheet="main"))    #extra "/" as examples
  
  source(paste0(myPlace,"/myFunctions/helperFunctions/wrapSentence.R"))
  source(paste0(myPlace,"/myFunctions/helperFunctions/wrapLabels.R"))
  source(paste0(myPlace,"/myFunctions/helperFunctions/compass.R"))

  source(paste0(myPlace,"/myFunctions/make_MAPS.R"))
  source(paste0(myPlace,"/myFunctions/make_rank_CAUSE_chart.R")) 
  source(paste0(myPlace,"/myFunctions/rankCausesSex.R")) 
  source(paste0(myPlace,"/myFunctions/make_cause_TABLE.R"))
  source(paste0(myPlace,"/myFunctions/make_rank_GEOGRAPHY_chart.R"))
  source(paste0(myPlace,"/myFunctions/make_TREND_chart.R"))
  source(paste0(myPlace,"/myFunctions/make_SDOH_scatter_chart.R"))

  source(paste0(myPlace,"/myData/appText/AppText.txt"))
  source(paste0(myPlace,"/myData/appText/newsUseText.txt"))

# === "SUB-SITE" Creation HERE ==============================================

if (subSite){
  mTitle <- subsiteName  
  shape_County <- shape_County[shape_County$county %in% subsiteList,]
  shape_Comm   <- shape_Comm[  shape_Comm $county  %in% subsiteList,]
  shape_Tract  <- shape_Tract[ shape_Tract$county  %in% subsiteList,]
  
  datCounty <- datCounty[datCounty$county %in% subsiteList,]
  datComm   <- datComm[datComm$county %in% subsiteList,]
  datTract  <- datTract[datTract$county %in% subsiteList,]  }

# --- Shiny Stuff and Constants -----------------------------------------------

# med.age, m.YLL  
lMeasures <- c("YLL","YLLper","YLL.adj.rate","Ndeaths","cDeathRate","aRate", "mean.age","SMR")


#   yll                     yll                        YLL
#   yllRate                 yll.rate                   YLL.rate
#   yllAdjustedRate         yll.adjusted.rate          YLL.adjusted.rate
#   deaths                  ndeaths                    Ndeaths
#   deathRate               death.rate
#   deathAdjustedRate       death.adjusted.rate
#   meanAge                 mean.age.at.death
#   SMR                     SMR 

#   similar nomenclature for confidence intervals


lMeasuresC <- c("Years of Life Lost (YLL)",
                "YLL Rate per 100,000 population",
                "Age-Adjusted YLL Rate",
                "Number of deaths",
                "Crude Death Rate per 100,000 population",
                "Age-Adjusted Death Rate",
                "Mean Age at Death",
                "Standard Mortality Ratio")

names(lMeasures) <- lMeasuresC

lMeasuresShort <- lMeasures[c(4,2,6,7,8)] # fix later

fullCauseList       <- gbdMap0[!is.na(gbdMap0$causeList),c("LABEL","causeList","nameOnly")] %>% arrange(LABEL)
causeNum36        <- fullCauseList[,"LABEL"]
names(causeNum36) <- fullCauseList[,"causeList" ]

phList   <- fullCauseList[nchar(fullCauseList$LABEL) <= 3,]
phCode   <- phList[,"LABEL"]
names(phCode) <- phList[,"causeList" ]

bigList  <- fullCauseList[nchar(fullCauseList$LABEL) == 1,]
bigCode  <- bigList[,"LABEL"]
names(bigCode) <- bigList[,"causeList"]

sdohVec  <- c("hpi2score", "insured", "inpreschool", "bachelorsed", "abovepoverty", "parkaccess","houserepair")

sdohVecL <- c(
"Healthy Places Index score",                                   
"Percentage of adults aged 18 to 64 years currently insured",
"Percentage of 3 and 4 year olds enrolled in school",                    
"Percentage of population over age 25 with a bachelor's education or higher",      
"Percent of the population with an income exceeding 200% of federal poverty level",
"Percentage of the population living within a half-mile of a park, beach, or open space greater than 1 acre",
"Percent of households with kitchen facilities and plumbing")

names(sdohVec) <- sdohVecL

lList         <- sort(as.character(unique(datCounty$county)))
lListNoState  <- lList[lList != STATE]


# if (sjcSite) {lList <- lList[lList %in% sjconsortium]}

nC       <- 5
myColor1 <- rev(brewer.pal(nC,"RdYlBu"))


# --- END ---------------------------------------------------------------------
# ----------------------------------------------------------------------------


library(fs)
CBD     <- dir_info(recursive = TRUE,type="file")
CBDinfo <- cbind(as.vector(path_dir(CBD$path)),as.vector(path_file(CBD$path)))


# NOTES:
# myYear <- 2013
# myLHJ  <- "Colusa"
# myLev <- 1
# myCause <- 104
# myCause  <- "Diabetes mellitus"



