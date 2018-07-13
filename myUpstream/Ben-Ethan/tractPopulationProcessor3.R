#===============================================#
#Population data pull for denominator           #
#                                               #
#This file is comprised of:                     #
#Loading packages                               #
#Loading Age grouping and MSSA files            #
#Merging Age grouping files for Census Pull     #
#Census Data Pull function using API            #
#                                               #
#Last Revision Date: 02/26/18                   #
#Author: Benjamin Hicks                         #
#===============================================#

myDrive  <- "E:"  
myPlace  <- paste0(myDrive,"/0.CBD/myCBD")  
#myPlace  <- paste0(myDrive,"/Users/bhicks/Desktop/cbd temp extracts/Take3")

#-- Load Packages ---------------------------------------------------------------------------------------------------------

  library(dplyr) #filter, %>%, full_join, select
  library(readxl) #read_excel
  library (tidycensus) #get_acs, get_decennial

  # census_api_key("b751f8899055dbb118c2aeb0f1f736d160a4816f")
  
#-- Loading Age grouping and MSSA files -----------------------------------------------------------------------------------

t.state   <- "CA"

#"ageList" = User defined age grouping. 
ageList <- read_excel(paste0(myPlace,"/myInfo/acsCensus.Map.xlsx"),sheet = "ageList")

#"tableList" = Census age grouping.
tableList  <- read_excel(paste0(myPlace,"/myInfo/acsCensus.Map.xlsx"),sheet = "tableList")

#"cbdLinkCA" = MSSA IDs (comID) to GEOID and County. Setting to "character" for eventual merging 
cbdLinkCA <- read.csv(paste0(myPlace,"/myInfo/cbdLinkCA.csv"),colClasses = c(GEOID="character",year="character"))

#adding columns to fill "tableList" with "ageList" groups.
tableList$lAgeGrouped <- NA
tableList$uAgeGrouped <- NA

#-- Merging Age grouping files for Census Pull -----------------------------------------------------------------------------

#LOOP to fill rows of tableList's AgeGrouped columns when tableList values fall between ageList row values. Fill with ageList values when true.
for (j in 1:nrow(ageList)) {  
tableList$lAgeGrouped[which (tableList$lAge[1:nrow(tableList)] >= ageList$lAge[j] &
                             tableList$uAge[1:nrow(tableList)] <= ageList$uAge[j]) ]<-ageList$lAge[j]
tableList$uAgeGrouped[which (tableList$lAge[1:nrow(tableList)] >= ageList$lAge[j] &
                             tableList$uAge[1:nrow(tableList)] <= ageList$uAge[j]) ]<-ageList$uAge[j]                                         
}

#-- Census Data Pull function using API -------------------------------------------------------------------------------------

#"Census Pull" = FUNCTION which combines census pull type and the compiling of pulled tables   
CensusPull <-function (Source,Year){ #Source = acs, decennial  ;  Year = end year for acs pulls
    for (i in 1:nrow(tableList)) {
      #checking pull type (acs/decennial)
      if        (Source == "acs")       {temp <-     get_acs(geography = "tract", variables = tableList$ACStableVar[i], state = t.state, endyear = Year, moe_level = 95)
                                        colnames(temp)[4]<-"value"
      } else if (Source == "decennial") {temp <-  get_decennial(geography = "tract", variables = tableList$DECtableVar[i], state = t.state, year = Year)
                                        temp$moe <- NA}
      #row compiling of multiple ACS/decennial tables
      temp <- cbind(temp,
                    tableList[i,3:7],
                    year   = paste(Year),
                    source = paste(Source))
      if (i == 1) {POPTRACT <- temp
      }  else     {POPTRACT <- rbind(temp,POPTRACT)}
     }
  #"POPTRACT" = Adding cbdLinkCA to census year table (POPTRACT) based on year  
  if (POPTRACT$year == 2000) {POPTRACT <- full_join(POPTRACT %>% select(-NAME,-variable) %>% rename(popAgeTot = value),filter(cbdLinkCA,year == 2000))}
  else                       {POPTRACT <- full_join(POPTRACT %>% select(-NAME,-variable) %>% rename(popAgeTot = value),filter(cbdLinkCA,year != 2000) %>% select(-year),by="GEOID")}
  POPTRACT
}

#"AGEDENOM.00.10.15" = DATAFRAME. Combining of user defined census tables
acswork0.R <- rbind(CensusPull("decennial",2000),CensusPull("acs",2010),CensusPull("acs",2015))









#== Junk (replacing Merge Dataframes step) ==================================================================================

#R's Findinterval() example
N <- 100
X <- sort(round(stats::rt(N, df = 2), 2))
tt <- c(-100, seq(-2, 2, len = 201), +100)
it <- findInterval(tt, X)
tt[it < 1 | it >= N] # only first and last are outside range(X)

#Michael's Findinterval() example
# make "ageMap" from info file to make standard age groups
ageMap  <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/acsCensus.Map2.xlsx"),sheet = "ageList"))
# subset ageMap to relevant rows and columns
ageMap  <- ageMap[!is.na(ageMap$inAgeG),c("lAge","uAge")]
#ageMap  <- read.csv(paste0(myPlace,"/myInfo/ageGroupMap2.csv"))  #simplier, but want to use same age info source for all files...
aL      <-      ageMap$lAge   # lower age ranges
aU      <- c(-1,ageMap$uAge)  # upper age ranges, plus inital value of "-1" to make all functions work properly

aMark         <- findInterval(cbdDat0$age,aU,left.open = TRUE)  # vector indicating age RANGE value of each INDIVIDUAL age value
faLabs         <- paste(aL,"-",aU[-1])                           # make label for ranges
cbdDat0$ageG  <- aLabs[aMark]   

#My Findinterval() attempt; the dataframe being added to needs to be sorted but if i sort i remove the range to findinterval from. I can create additional code but the code ends up being lengthy 

aMark         <- findInterval(ageList$uAge, tableList$uAge,left.open = FALSE)

tosee <- right_join(tableList,ageList, tableList$sex == ageList$sex, tableList$lAge >= ageList$lAge, tableList$uAge <= ageList$uAge )

tosee2<-findInterval(tableList,sort(ageList[which("sex" == "M")],method = "lAge"))
                     