library(sqldf)

server <- TRUE
if (!server) source("g:/FusionData/Standards/FusionStandards.R")
if (server) source("/mnt/projects/FusionData/Standards/FusionStandards.R")


raceLink <- raceLink %>% select(raceCode, raceNameShort)

#==========================================================================================================

popCounty        <- readRDS(paste0(ccbUpstream,"/upData/popCounty.RDS")) %>% ungroup()  


popRace3Year     <-  popCounty %>% filter(year %in% 2017:2019, ageGroup == "Total", sex == "Total", raceCode != "Total") %>%
                        group_by(county,raceCode) %>%
                        summarize(population = sum(population))  %>% ungroup()

# check
sum( filter(popRace3Year, county == "CALIFORNIA") %>% pull(population)) / 3


popAge3Year     <-  popCounty %>% filter(year %in% 2017:2019, ageGroup != "Total", sex == "Total", raceCode == "Total") %>%
                       group_by(county,ageGroup) %>%
                      summarize(population = sum(population))  %>% ungroup()


deathAge <- readRDS(file= path(ccbData,"real/datCounty_AGE_3year.RDS")) %>%
             filter(sex == "Total", yearG3 == "2017-2019", Level == "lev2") %>%  
             select(yearG3, county, sex, ageGroup, causeCode, N = Ndeaths, cRate = cDeathRate ) 
  
deathRace <- readRDS(file= path(ccbData,"real/datCounty_RE.RDS")) %>%
              filter(sex == "Total", yearG3 == "2017-2019", Level == "lev2") %>%  
              left_join(raceLink,by="raceCode") %>%
              select(yearG3, county, sex, raceNameShort, causeCode, N =  Ndeaths, aRate, cRate = cDeathRate ) 

#---------------------------------------------------------------------------------------------

focusData <- paste0(ccbData,"real/age_race_focus_data/")

critN <- 11

hospAge   <- readRDS(file = path(focusData,"inData/hospAge.rds"))%>%  
              left_join(popAge3Year,by=c("county","ageGroup")) %>%
              rename(N = n_hosp) %>%
              mutate(cRate = 100000*N/population) %>%
              filter(N >= critN)

hospRace  <- readRDS(file = path(focusData,"inData/hospRace.rds"))    %>%  
              left_join(popRace3Year,by=c("county","raceCode")) %>%
              left_join(raceLink,by="raceCode") %>%
              rename(N = n_hosp) %>%
              mutate(cRate = 100000*N/population) %>%
              filter(N >= critN)

edAge    <-  readRDS(file = path(focusData,"inData/edAge.rds"))      %>%  
              left_join(popAge3Year,by=c("county","ageGroup"))%>%
              rename( N = n_ED) %>%
              mutate(cRate = 100000*N/population) %>%
              filter(N >= critN)

edRace   <-  readRDS(file = path(focusData, "inData/edRace.rds"))    %>%  
              left_join(popRace3Year,by=c("county","raceCode")) %>%
              left_join(raceLink,by="raceCode") %>%
              rename( N = n_ED) %>%
              mutate(cRate = 100000*N/population)  %>%
              filter(N >= critN)


makePlotRankData <- function(myDataSet     = "deathAge", 
                             myData        = "Deaths",   # "Hospitalizations", "Emergency Department"
                             myMAINSTRATA  = "ageGroup", # "raceCode" , "sex"
                             myRace        = "Black-NH",
                             myAgeG        = "80-94",
                             myCounty      = "Los Angeles",
                             mySex         = "Total",
                             myN           = 10,
                             myYearG3      = "2016-2018",
                             myScale       = "fixed",
                             myFill        = NA
                            ){

    tDat   <-  get(myDataSet)         %>% 
                filter(sex == mySex)     %>%   # , !is.na(ageGroup) %>%
                select(MAINSTRATA = myMAINSTRATA, causeCode, county,  N:cRate) 
    
    causeTemp  <- data.frame(causeCode   = unique(tDat$causeCode) ,stringsAsFactors = FALSE)
    strataTemp <- data.frame(MAINSTRATA  = unique(tDat$MAINSTRATA),stringsAsFactors = FALSE)
    countyTemp <- data.frame(county      = unique(tDat$county),stringsAsFactors = FALSE)
    fullMat    <- sqldf(" select * from  countyTemp cross join causeTemp cross join strataTemp")
    
    t.dataSet <- full_join(tDat,fullMat,by=c("county","causeCode","MAINSTRATA")) %>%
      arrange(county,causeCode, MAINSTRATA) %>%
      mutate(cRate = ifelse(is.na(cRate),0,cRate),
             N     = ifelse(is.na(N),    0,N)) 
   
    if (myData == "Deaths")                                         tCause <- deathCauseLink
    if (myData %in% c("Hospitalizations", "Emergency Department"))  tCause <-  hospCauseLink
    
    t.dataSet <- t.dataSet %>%  left_join(tCause,by="causeCode")
     
    
} 
    
t.DeathAge  <-   makePlotRankData(myDataSet = "deathAge",  myData = "Deaths",              myMAINSTRATA  = "ageGroup")
t.HospAge   <-   makePlotRankData(myDataSet = "hospAge",   myData = "Hospitalizations",    myMAINSTRATA  = "ageGroup")
t.EDAge     <-   makePlotRankData(myDataSet = "edAge",     myData = "Emergency Department",myMAINSTRATA  = "ageGroup")

t.DeathRace <-   makePlotRankData(myDataSet = "deathRace", myData = "Deaths",              myMAINSTRATA  = "raceNameShort")   
t.HospRace  <-   makePlotRankData(myDataSet = "hospRace",  myData = "Hospitalizations",    myMAINSTRATA  = "raceNameShort")
t.EDRace    <-   makePlotRankData(myDataSet = "edRace",    myData = "Emergency Department",myMAINSTRATA  = "raceNameShort")

    

saveRDS(t.DeathAge,path(focusData, "deaths_age_stn.RDS"))
saveRDS(t.HospAge,path(focusData,  "hospital_age_stn.RDS"))
saveRDS(t.EDAge,path(focusData,    "ed_age_stn.RDS"))

saveRDS(t.DeathRace,path(focusData, "deaths_race_stn.RDS"))
saveRDS(t.HospRace,path(focusData,  "hospital_race_stn.RDS"))
saveRDS(t.EDRace,path(focusData,    "ed_race_stn.RDS"))

