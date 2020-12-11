
library(sqldf)
library(dplyr)
library(readxl)
library(fs)
library(stringr)
library(sqldf)

infoPlace <- "/mnt/projects/CCB/CCB Project/0.CCB/myCBD/myInfo"
focusData <- "/mnt/projects/CCB/CCB Project/0.CCB/myCBD/myData/real/age_race_focus_data"
ccbData   <- "/mnt/projects/CCB/CCB Project/0.CCB/myCBD/myData/real"

#==========================================================================================================


deathCauseLink  <- as.data.frame(read_excel( path(infoPlace,"icd10_to_CAUSE.xlsx"), sheet="main")) %>%
  filter(!is.na(causeList)) %>% arrange(LABEL)  %>%
  filter(!is.na(PH),is.na(DETAIL)) %>%
  select(causeCode = LABEL, causeName = nameOnly)  %>%
  mutate(t.cause = str_sub(causeCode,1,1),
         topLev = ifelse(t.cause== "A","Communicable",
                  ifelse(t.cause== "B","Cancer",
                  ifelse(t.cause== "C","Cardiovascular", 
                  ifelse(t.cause== "D","Other Chronic",  
                  ifelse(t.cause== "E","Injury","Ill-Defined")))))
  ) %>% select(-t.cause)


ccsCauseLink  <-   as.data.frame(read_excel( path(infoPlace,"CCS Code and Names Linkage.xlsx"))) %>%
  mutate(causeCode = str_pad(ccsCode, 5,"left",pad="o")) %>%
  select(causeCode, causeName = ccsName, topLev, birth)


#==========================================================================================================


## CHANGE TO AGE7  
deathAge <- readRDS(file= path(ccbData,"datCounty_AGE_3year.RDS")) %>%
             filter(sex == "Total", yearG3 == "2017-2019", Level == "lev2") %>%  
             select(yearG3, county, sex, ageGroup, causeCode = CAUSE, Ndeaths ) %>%   
             left_join(deathCauseLink, by = "causeCode")
  
# OLD
# ageDat <- cbdDat0 %>% 
#   filter(year %in% 2016:2018) %>%
#   mutate(ageG = ageChop(age,"age7")) %>%
#   select(county,ageG,CAUSE=lev2) %>%
#   group_by(county,ageG,CAUSE)  %>%
#   summarise(Ndeaths=n()) %>%
#   filter(Ndeaths > 10) %>%
#   mutate(yearG3 = "2016-2018", sex = "Total") %>%
#   filter(Ndeaths > 10)
# saveRDS(ageDat,paste0(focusData,"/deathAge.RDS"))


deathRace <- readRDS(file= path(ccbData,"datCounty_RE.RDS")) %>%
              filter(sex == "Total", yearG3 == "2017-2019", Level == "lev2") %>%  
              select(yearG3, county, sex, raceCode, causeCode = CAUSE, Ndeaths ) %>%   
              left_join(deathCauseLink, by = "causeCode")

#---------------------------------------------------------------------------------------------

hospAge   <- readRDS(file = path(focusData,"hospAge.rds"))    %>%  
              left_join(ccsCauseLink, by = "causeCode")

hospRace  <- readRDS(file = path(focusData,"hospRace.rds"))    %>%  
              left_join(ccsCauseLink, by = "causeCode")

edAge    <-  readRDS(file = path(focusData,"edAge.rds"))    %>%  
              left_join(ccsCauseLink, by = "causeCode")

edRace   <-  readRDS(file = path(focusData, "edRace.rds"))    %>%  
              left_join(ccsCauseLink, by = "causeCode")


makePlotRankData <- function(myDataSet     = "deathAge", 
                             myData        = "Deaths",   # "Hospitalizations", "Emergency Department"
                             myMAINSTRATA  = "ageGroup", # "raceCode" , "sex"
                             myRace        = "Black-NH",
                             myAgeG        = "80-94",
                             myCounty      = "Los Angeles",
                             myMeasure     = "Ndeaths",           # n_hosp
                             mySex         = "Total",
                             myN           = 10,
                             myYearG3      = "2016-2018",
                             myScale       = "fixed",
                             myFill        = NA
                            ){

    tDat   <-  get(myDataSet)         %>% 
                filter(sex == mySex)     %>%   # , !is.na(ageGroup)
                select(MAINSTRATA = myMAINSTRATA, causeCode, county, measure=myMeasure) 
    
    causeTemp  <- data.frame(causeCode   = unique(tDat$causeCode) ,stringsAsFactors = FALSE)
    strataTemp <- data.frame(MAINSTRATA  = unique(tDat$MAINSTRATA),stringsAsFactors = FALSE)
    countyTemp <- data.frame(county      = unique(tDat$county),stringsAsFactors = FALSE)
    fullMat    <- sqldf(" select * from  countyTemp cross join causeTemp cross join strataTemp")
    
    t.dataSet <- full_join(tDat,fullMat,by=c("county","causeCode","MAINSTRATA")) %>%
      arrange(county,causeCode, MAINSTRATA) %>%
      mutate(measure = ifelse(is.na(measure),0,measure)) 
 
    
    if (myData == "Deaths")                                         tCause <- deathCauseLink
    if (myData %in% c("Hospitalizations", "Emergency Department"))  tCause <-  ccsCauseLink

    t.dataSet <- t.dataSet %>%  left_join(tCause,by="causeCode")
    
} 
    
t.DeathAge  <-   makePlotRankData(myDataSet = "deathAge",  myData = "Deaths",              myMAINSTRATA  = "ageGroup", myMeasure = "Ndeaths")
t.HospAge   <-   makePlotRankData(myDataSet = "hospAge",   myData = "Hospitalizations",    myMAINSTRATA  = "ageGroup", myMeasure = "n_hosp")
t.EDAge     <-   makePlotRankData(myDataSet = "edAge",     myData = "Emergency Department",myMAINSTRATA  = "ageGroup", myMeasure = "n_ED")

t.DeathRace <-   makePlotRankData(myDataSet = "deathRace", myData = "Deaths",              myMAINSTRATA  = "raceCode", myMeasure = "Ndeaths")   ### should be raceCode
t.HospRace  <-   makePlotRankData(myDataSet = "hospRace",  myData = "Hospitalizations",    myMAINSTRATA  = "raceCode", myMeasure = "n_hosp")
t.EDRace    <-   makePlotRankData(myDataSet = "edRace",    myData = "Emergency Department",myMAINSTRATA  = "raceCode", myMeasure = "n_ED")

    

saveRDS(t.DeathAge,path(focusData, "deaths_age_stn.RDS"))
saveRDS(t.HospAge,path(focusData,  "hospital_age_stn.RDS"))
saveRDS(t.EDAge,path(focusData,    "ed_age_stn.RDS"))

saveRDS(t.DeathRace,path(focusData, "deaths_race_stn.RDS"))
saveRDS(t.HospRace,path(focusData,  "hospital_race_stn.RDS"))
saveRDS(t.EDRace,path(focusData,    "ed_race_stn.RDS"))


# ============================================

if(1==2){
  myCounty = "Los Angeles"
  myScale  = "fixed"
  mySex = "Total"
  myN = 10
  myYearG3 = "2016-2018"
  myRaceG = "80-94"
  
  myMAINSTRATA = "ageGroup"
  myMAINSTRATA = "ageGroup"
  
  myData        = "Deaths"
  myMeasure     = "Ndeaths"
  myDataSet     = "deathAge"
  
  myData    = "Hospitalizations"
  myMeasure = "n_hosp"
  myDataSet = "hospAge"
  
}

    
    