# =====================================================================================
# "makeDatasets.R" file                                                               |
#            designate key constants, folder locations, and load packages             |
#            load data mapping files                                                  |
#            load population denominator data                                         |
#            load death data (cbdDat0)                                                |  
#            build functions for YLL and rate calcuations                             |
#            contruction initial tract, community & county CBD data files             |
#            process data and calculate age-adjusted rates                            |
#            final merges and processing of main CBD data files                       |
#            export files for use in CBD app                                          |
#======================================================================================

# -- Designate locations and load packages---------------------------------------------------------

whichDat <- "fake"

STATE    <- "California"

myDrive <- "E:"  
myPlace <- paste0(myDrive,"/0.CBD/myCBD") 
upPlace <- paste0(myDrive,"/0.CBD/myUpstream") 

library(tidyverse)
library(epitools)
library(sqldf)
library(readxl)
library(fs)

yF   <- 100000  # rate constant 
pop5 <- 5       # 5 years
pop1 <- 1       # 1 year

#-- LOAD STANDARDS AND DATA MAPPING FILES ---------------------------------------------------------

# add to technical notes the purposes and contents of each data mapping file 

# this "as.data.frame" below and elsewhere is really annoying.... but at least icdToGroup function below does not work otherwise;
# becuase the "tibble" is double precision or for some other reason this messes up; 
# and get error "Error: Can't use matrix or array for column indexing"




leMap      <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/le.Map.xlsx"), sheet="LifeExpLink", range = cell_cols("A:B")))
yearMap    <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/year.Map.xlsx")))
geoMap     <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/countycodes.Map.xlsx")))
cbdLinkCA  <- read.csv(paste0(myPlace,"/myInfo/cbdLinkCA.csv"),colClasses = "character")  # file linking MSSAs to census 
comName    <- unique(cbdLinkCA[,c("comID","comName")])                                    # dataframe linking comID and comName
ageMap     <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/Age Groups and Standard US 2000 pop.xlsx"),sheet = "data"))

#-- LOAD AND PROCESS POPULATION DATA --------------------------------------------------------------

popTract            <- readRDS(path(upPlace,"/upData/popTract2013.RDS"))
popTractSexTot      <- filter(popTract,ageG == "Total")
popTractSexTotAgeG  <- filter(popTract,ageG != "Total")

popCommSexTot       <- popTractSexTot     %>% group_by(yearG,county,comID,sex)      %>% summarise(pop=sum(pop))   
popCommSexTotAgeG   <- popTractSexTotAgeG %>% group_by(yearG,county,comID,sex,ageG) %>% summarise(pop=sum(pop))

popCounty           <- readRDS(path(upPlace,"/upData/popCounty2000to2015.RDS"))
popCountySexTot     <- filter(popCounty,ageG == "Total")
popCountySexTotAgeG <- filter(popCounty,ageG != "Total")

popStandard         <- ageMap %>% mutate(ageG = paste0(lAge," - ",uAge))

 # == LOAD AND PROCESS DEATH DATA =================================================================

if (whichDat == "real") {
  # CAUTION --- if using REAL DATA INCLUDE these two lines below and edit the first one with your secure location
  # load("G:/CCB/0.Secure.Data/myData/cbdDat0FULL.R")     
  load("H:/0.Secure.Data/myData/cbdDat0FULL.R")      
  cbdDat0 <- cbdDat0FULL    
}

if (whichDat == "fake") { 
  # Load FAKE Data --- COMMENT OUT these two lines if using REAL DATA
  load(paste0(upPlace,"/upData/cbdDat0SAMP.R"))      
  cbdDat0 <- cbdDat0SAMP
}

#forEthan <- sample_n(cbdDat0SAMP,100000)
#saveRDS(forEthan, file=paste0(upPlace,"/upData/forEthan.RDS"))

cbdDat0       <- mutate(cbdDat0,
                         sex    = c("Male","Female")[match(sex,c("M","F"))],
                         age    = as.numeric(age),                                                  # redundant...
                         ICD10  = as.character(ICD10),                                              # redundant...
                         comID  = cbdLinkCA[match(cbdDat0$GEOID,cbdLinkCA[,"GEOID"]),"comID"],   
                         yll    = leMap[match(cbdDat0$age,leMap[,"Age"]),"LE"],
                         yearG  = yearMap[match(year,yearMap[,"year"]),"yGroup1"]           
                        )

cbdDat0Save   <- cbdDat0
.cbdDat0Sex   <- mutate(cbdDat0, sex = "Total")
cbdDat0       <- bind_rows(cbdDat0,.cbdDat0Sex)


# Add Age-Group variable ----------------------------------------

aL            <-      ageMap$lAge     # lower age ranges
aU            <- c(-1,ageMap$uAge)    # upper age ranges, plus inital value of "-1" for lower limit
aLabs         <- paste(aL,"-",aU[-1]) # make label for ranges
aMark         <- findInterval(cbdDat0$age,aU,left.open = TRUE)  # vector indicating age RANGE value of each INDIVIDUAL age value
cbdDat0$ageG  <- aLabs[aMark]                                   # make new "ageG" variable based on two objects above 


# Map ICD-10 codes to GBD conditions ----------------------------

gbdMap0   <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/NEWgbd.ICD.Map.xlsx"), sheet="main"))   # also have e.g. range="A1:J167"
mapICD    <- gbdMap0[!is.na(gbdMap0$CODE),c("CODE","regEx10")]


icdToGroup <- function(myIn) {
  Cause   <- rep(NA,length(myIn))
  for (i in 1:nrow(mapICD)) {Cause[grepl(mapICD[i,"regEx10"],myIn)] <- mapICD[i,"CODE"] } 
  Cause}

cbdDat0$icdCODE  <- icdToGroup(myIn=cbdDat0$ICD10)
temp             <- nchar(str_sub(cbdDat0$icdCODE,2,5))
cbdDat0          <- cbdDat0  %>% mutate(lev0  = "0",
                                        lev1  = str_sub(icdCODE,2,2),
                                        lev2  = str_sub(icdCODE,2,4),
                                        lev3  = ifelse(temp==4,str_sub(icdCODE,2,5),NA)
                                       )

table(cbdDat0$lev0,useNA = "ifany")
table(cbdDat0$lev1,useNA = "ifany")
table(cbdDat0$lev2,useNA = "ifany")
table(cbdDat0$lev3,useNA = "ifany")

junk <- filter(cbdDat0,is.na(lev1))

# cbdDat0$gbd3    <- icdToGroup(myIn=cbdDat0$ICD10,gbdMap0[is.na(gbdMap0$L2)     ,c("gbdCode","regEx10")])      
# cbdDat0$gbdSpec <- icdToGroup(myIn=cbdDat0$ICD10,gbdMap0[ (gbdMap0$L2 %in% c(2,6,8,11,13,14,15,16,21,22) & !is.na(gbdMap0$L3) & is.na(gbdMap0$L4) & is.na(gbdMap0$list36) )    ,c("gbdCode","regEx10")])      


# DATA CLEANING ISSUES (see at bottom of file) ----------------------------------------------------


# DEATH MEASURES FUNCTIONS =========================================================================


calculateYLLmeasures <- function(group_vars,levLab){
  
  dat <- cbdDat0 %>% group_by_(.dots = group_vars) %>% 
    summarize(Ndeaths = n() , 
              YLL     = sum(yll,   na.rm = TRUE),     # NEED TO ADD CIs
              m.YLL   = mean(yll,  na.rm = TRUE),     # NEED TO ADD CIs
              mean.age = mean(age,na.rm=TRUE)
    ) %>%  ungroup 
 
    names(dat)[grep("lev", names(dat))] <- "CAUSE"
    
    dat$Level <- levLab
    
    dat <- filter(dat,!is.na(CAUSE)) # "HARD FIX" that should be assessed carefully
      
    dat %>%  data.frame

    
}

calculateRates <- function(inData,yearN){
  transform(inData, 
            YLLper      = yF*YLL/(yearN*pop),
            YLLrateLCI  = yF*pois.approx(YLL,yearN*pop, conf.level = 0.95)$lower,  # need to confirm that this is correct
            YLLrateUCI  = yF*pois.approx(YLL,yearN*pop, conf.level = 0.95)$upper,
            
            cDeathRate  = yF*Ndeaths/(yearN*pop),
            rateLCI     = yF*pois.approx(Ndeaths,yearN*pop, conf.level = 0.95)$lower,
            rateUCI     = yF*pois.approx(Ndeaths,yearN*pop, conf.level = 0.95)$upper
  )
}


# == build TRACT-level file =======================================================================

# Group Causes
grp       <- c("county","GEOID","yearG","sex","lev1")

datTract  <- calculateYLLmeasures(grp,grp[length(grp)]) %>% 
  filter(yearG == "2011-2015")  %>%    # 2011-2015 ONLY!!!
  arrange(county,GEOID,yearG,CAUSE)
# NOTE -- includes many with NA GEOID



# MERGE Death and Population files
datTract <- merge(datTract,popTractSexTot,by = c("yearG","county","GEOID","sex"))                     
# NOTE -- merge "drops" NA GEOID  --> good/fine here

# Calculate Rates
datTract <- calculateRates(datTract,5) %>%
  arrange(county,GEOID,yearG,CAUSE)




# == build COMMUNITY-level file ===================================================================

grp36   <- c("county","comID","yearG","sex","gbd36")
#grp3    <- c("county","comID","yearG","sex","gbd3")

datComm <- rbind(calculateYLLmeasures(grp36,grp36[length(grp36)])) %>%  #,
                   #               calculateYLLmeasures( grp3, grp3[ length(grp3)],myTotal=FALSE)) %>%
    filter(yearG == "2011-2015")  %>%   # 2011-2015 ONLY!!!
  arrange(county,comID,yearG,CAUSE)

datComm  <- merge(datComm,popCommSexTot,by = c("yearG","county","comID","sex"))

datComm  <- calculateRates(datComm,5)

# add community names  POSSIBLE REMOVE
datComm  <- merge(datComm, comName, by = "comID",all=TRUE) %>%
  arrange(county,comID,yearG,CAUSE)


# == build COUNTY-level file ======================================================================

c.t1      <- calculateYLLmeasures(c("county","year","sex","lev0"),"lev0")
c.t2      <- calculateYLLmeasures(c("county","year","sex","lev1"),"lev1")
c.t3      <- calculateYLLmeasures(c("county","year","sex","lev2"),"lev2")
c.t4      <- calculateYLLmeasures(c("county","year","sex","lev3"),"lev3")
datCounty <- bind_rows(c.t1,c.t2,c.t3,c.t4)

s.t1      <- calculateYLLmeasures(c("year","sex","lev0"),"lev0")
s.t2      <- calculateYLLmeasures(c("year","sex","lev1"),"lev1")
s.t3      <- calculateYLLmeasures(c("year","sex","lev2"),"lev2")
s.t4      <- calculateYLLmeasures(c("year","sex","lev3"),"lev3")
datState  <- bind_rows(s.t1,s.t2,s.t3,s.t4)
datState$county = STATE

datCounty <- bind_rows(datCounty,datState)

# datCounty$causeName <- gbdMap0[match(datCounty$CAUSE,gbdMap0[,1]),"nameOnly"]  #  "needed?"

datCounty <- merge(datCounty,popCountySexTot,by = c("year","county","sex"))

datCounty <- calculateRates(datCounty,1)

datState  <-  datCounty  %>% filter(county == STATE) %>%
  mutate(stateRate = cDeathRate) %>%
  select(year,sex,Level,CAUSE,stateRate)

# for LOCAL installation of application EXCLUDE save line and INCLUDE load line
save(datState, file= paste0(upPlace,"/upData/datState.R"))
#load(file= paste0(upPlace,"/upData/datState.R"))

datCounty            <- merge(datCounty,datState,by = c("year","sex","Level","CAUSE"))
datCounty$SMR        <- datCounty$cDeathRate / datCounty$stateRate


# == AGE ADJUSTED ("AA") RATES =========================================================================================

# makes dataframe of all possible combinations of county, year, CAUSE, and ageG 

year   <- data.frame(year   = 2000:2015) # these "vectors" need to be dataframes for the sq merge below to work
yearG  <- data.frame(yearG  = "2011-2015")
CAUSE  <- data.frame(CAUSE  = c(0,sort(unique(cbdDat0$gbd36))))  # "0" is for "all cause"
sex    <- data.frame(sex    = c("Male","Female","Total"))
ageG   <- data.frame(ageG   = sort(unique(cbdDat0$ageG)))
county <- data.frame(county = geoMap$countyName)         
comID  <- data.frame(comID  = unique(cbdLinkCA[,"comID"]))
GEOID  <- data.frame(GEOID  = cbdLinkCA[,"GEOID"])

# other cool approach from Adam:
# fullMatCounty <- Reduce(function(...) merge(..., all = TRUE), list(county, year, CAUSE, sex, ageG))
fullMatCounty <- sqldf(" select * from  county cross join year  cross join CAUSE cross join sex cross join ageG")
fullMatComm   <- sqldf(" select * from  comID  cross join yearG cross join CAUSE cross join sex cross join ageG")
fullMatTract  <- sqldf(" select * from  GEOID  cross join yearG cross join CAUSE cross join sex cross join ageG")

fullMatCounty <- mutate(fullMatCounty, county = as.character(county),                             sex = as.character(sex), ageG   = as.character(ageG), tester = 0)
fullMatComm   <- mutate(fullMatComm,   comID  = as.character(comID), yearG = as.character(yearG), sex = as.character(sex), ageG   = as.character(ageG), tester = 0)
fullMatTract  <- mutate(fullMatTract,  GEOID  = as.character(GEOID), yearG = as.character(yearG), sex = as.character(sex), ageG   = as.character(ageG), tester = 0)

# County age deaths -------------------------------------------------------------------------------

datAA1       <- cbdDat0 %>% group_by(county,year, sex, ageG,gbd36) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) )  # BY CAUSE
datAA1.State <- cbdDat0 %>% group_by(       year, sex, ageG,gbd36) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) )  %>% mutate(county=STATE)
datAA1       <- bind_rows(datAA1,datAA1.State) %>% mutate(CAUSE=gbd36) %>% select(-gbd36)  

datAA0       <- cbdDat0 %>% group_by(county,year, sex, ageG      ) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) )  # BY CAUSE
datAA0.State <- cbdDat0 %>% group_by(       year, sex, ageG      ) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) )  %>% mutate(county=STATE)
datAA0       <- bind_rows(datAA0,datAA0.State) %>% mutate(CAUSE=0)   

datAA1 <- bind_rows(datAA1,datAA0)   # merge cause-specific and all-cause  # 140,192 records

# DATA CLEANING ISSUES as above
datAA1 <- filter(datAA1,!is.na(ageG))   # remove 403 records with missing age (0.065% of deaths)  -- impact of this?
datAA1 <- filter(datAA1,!is.na(CAUSE))  # remove 6955 records with missing CAUSE
datAA1 <- filter(datAA1,!is.na(county)) # remove 758 records with missing county
datAA1 <- filter(datAA1,!is.na(sex))    # remove 

ageCounty   <- merge(fullMatCounty,datAA1,by = c("county","year","sex","ageG","CAUSE"),all=TRUE)  %>%    # merge death data and "fullMatCounty"
               merge(popCountySexTotAgeG, by = c("county","year","sex","ageG")        ,all=TRUE)  %>%    # merge population
               merge(popStandard[,c("ageG","US2000POP")],           by="ageG")                           # merge standard population

ageCounty$Ndeaths[is.na(ageCounty$Ndeaths)] <- 0    # if NA deaths in strata change to "0"
ageCounty$YLL[is.na(ageCounty$YLL)]         <- 0    # if NA deaths in strata change to "0"

countyAA <- ageCounty %>% group_by(county,year,sex,CAUSE) %>%
  summarize(oDeaths = sum(Ndeaths),
            aRate   = ageadjust.direct(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            YLL.adj.rate   = ageadjust.direct(count=YLL, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) # CONFIRM

countyAA <- countyAA[!(countyAA$oDeaths==0),c("county","year","sex","CAUSE","aRate","aLCI","aUCI","YLL.adj.rate")]  # remove strata with no deaths and select columns  

#tester <- filter(ageCounty,year==2015,county=="Alameda",sex=="Male",CAUSE==0) 
#ageadjust.direct(count=tester$Ndeaths, pop=tester$pop, rate = NULL, stdpop=tester$US2000POP, conf.level = 0.95)*100000


# Community age deaths ----------------------------------------------------------------------------

datAA1 <- cbdDat0 %>% group_by(comID, yearG, sex, ageG, gbd36) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) )  %>% mutate(CAUSE=gbd36) %>% select(-gbd36)   # BY CAUSE
datAA0 <- cbdDat0 %>% group_by(comID, yearG, sex, ageG)        %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) )  %>% mutate(CAUSE=0)                          # ALL CAUSE

datAA1 <- bind_rows(datAA1,datAA0)  %>% filter(comID != "")  
#datAA1 <- na.omit(datAA1)           

ageComm   <- merge(fullMatComm,datAA1,by = c("comID","yearG","sex","ageG","CAUSE"),all=TRUE)  %>% 
                filter(yearG == "2011-2015")                                                  %>%
             merge(popCommSexTotAgeG, by = c("comID","yearG","sex","ageG"),all=TRUE)          %>% # population
             merge(popStandard[,c("ageG","US2000POP")],by="ageG")                                 # standard population


ageComm$Ndeaths[is.na(ageComm$Ndeaths)] <- 0    # if NA deaths in strata change to "0"
ageComm$YLL[is.na(ageComm$YLL)]         <- 0    # if NA deaths in strata change to "0"

commAA <- ageComm %>% group_by(comID,yearG,sex,CAUSE) %>%
  summarize(oDeaths = sum(Ndeaths),
            aRate   = ageadjust.direct(count=Ndeaths, pop=pop*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct(count=Ndeaths, pop=pop*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct(count=Ndeaths, pop=pop*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            YLL.adj.rate   = ageadjust.direct(count=YLL, pop=pop*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) 
commAA <- commAA[!(commAA$oDeaths==0),c("comID","yearG","sex","CAUSE","oDeaths","aRate","aLCI","aUCI","YLL.adj.rate")]  

# removes rows with aRate = inf HERE there are only ALPINE 
commAA  <- commAA[!(commAA$aRate > 10000),]

# tester <- filter(ageComm,yearG=="2011-2015",comID=="1.1",sex=="Female",CAUSE==0) 
# ageadjust.direct(count=tester$Ndeaths, pop=tester$pop, rate = NULL, stdpop=tester$US2000POP, conf.level = 0.95)*100000

# Tract age deaths -----------------------------------------------------------------------------------------------------

datAA1 <- cbdDat0 %>% group_by(GEOID, yearG, sex, ageG, gbd36) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) )  %>% mutate(CAUSE=gbd36) %>% select(-gbd36)   # BY CAUSE
datAA0 <- cbdDat0 %>% group_by(GEOID, yearG, sex, ageG)        %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) )  %>% mutate(CAUSE=0)                          # ALL CAUSE

datAA1 <- bind_rows(datAA1,datAA0)  %>% filter(GEOID != "")  
#datAA1 <- na.omit(datAA1)           

ageTract   <- merge(fullMatTract,datAA1,by = c("GEOID","yearG","sex","ageG","CAUSE"),all=TRUE)  %>% 
                filter(yearG == "2011-2015")                                                    %>%
              merge(popTractSexTotAgeG,by = c("GEOID","yearG","sex","ageG"),all=TRUE)           %>%   # add population
              merge(popStandard[,c("ageG","US2000POP")],by="ageG")                                    # add standard population 

ageTract$Ndeaths[is.na(ageTract$Ndeaths)] <- 0    # if NA deaths in strata change to "0"
ageTract$YLL[is.na(ageTract$YLL)]         <- 0    # if NA deaths in strata change to "0"

tractAA <- ageTract %>% group_by(GEOID,yearG,sex,CAUSE) %>%
  summarize(oDeaths = sum(Ndeaths),
            aRate   = ageadjust.direct(count=Ndeaths, pop=pop*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct(count=Ndeaths, pop=pop*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct(count=Ndeaths, pop=pop*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            YLL.adj.rate   = ageadjust.direct(count=YLL, pop=pop*pop5, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) 
tractAA <- tractAA[!(tractAA$oDeaths==0),c("GEOID","yearG","sex","CAUSE","oDeaths","aRate","aLCI","aUCI","YLL.adj.rate")]  


# removes rows with aRate = inf, or infinity becuase some population strata is 0 AND some other odd strata
tractAA  <- tractAA[!(tractAA$aRate > 5000),]
tractAA  <- tractAA[!(is.na(tractAA$aRate)),]

# -- Merge adjusted rates into main data files ----------------------------------------------------------

datTract  <- merge(datTract,  tractAA ,by = c("GEOID","yearG","sex","CAUSE"),all=TRUE) 
datComm   <- merge(datComm,    commAA ,by = c("comID","yearG","sex","CAUSE"),all=TRUE) 
datCounty <- merge(datCounty,countyAA ,by = c("county","year","sex","CAUSE"),all=TRUE) %>% select(-ageG,-stateRate)

# == Final Data Clean Up and Export ==================================================================================

datCounty <- datCounty %>% mutate_if(is.numeric, signif,digits=4)                        %>%  # much smaller file and easier to read
                           mutate(county = ifelse(county==STATE, toupper(STATE),county),      # e.g. California --> CALIFORNIA
                                  SMR    = ifelse(Ndeaths < 6,   0             ,SMR)          # better NA, but then "highest on ranking..."
                           )
datComm  <- datComm  %>% mutate_if(is.numeric, signif,digits=4)         
datTract <- datTract %>% mutate_if(is.numeric, signif,digits=4)         

  
# "SMALL CELL and "RISKY CAUSE" supression ----------
# xCause0 <- c(14,41,50,139,4,49,192)
# xCause1 <- c(xCause0,10)
# datTract  <- filter(datTract, !(CAUSE %in% xCause1))
# datComm   <- filter(datComm,  !(CAUSE %in% xCause1))
# datCounty <- filter(datCounty,!(CAUSE %in% xCause0))

#write.csv(datTract,(paste0(upPlace,"/tempOutput/Tract CCB Work.csv")))
#write.csv(datComm,(paste0(upPlace,"/tempOutput/Community CCB Work.csv")))
#write.csv(datCounty,(paste0(upPlace,"/tempOutput/County CCB Work.csv")))
#write.csv(datState,(paste0(upPlace,"/tempOutput/State CCB Work.csv")))

saveRDS(datTract,  file= path(myPlace,"/myData/",whichDat,"datTract.RDS"))
saveRDS(datComm,   file= path(myPlace,"/myData/",whichDat,"datComm.RDS"))
saveRDS(datCounty, file= path(myPlace,"/myData/",whichDat,"datCounty.RDS"))

# END ===================================================================================================================


 # DATA CLEANING ISSUES ----------------------------------
 
 # in 2012 Los Angeles Census Tract 9304.01 was merged into tract 1370.00
 # "The deletion of Census 2000 Tract 1370.00 is now corrected, and the tract is reinstated
 #   with its former boundaries. This change incorporates all of former (2010) Census Tract 9304.01
 #   and part of (2010) Census Tract 8002.04 into the reinstated (2012) tract 1370.00.
 # https://www.census.gov/programs-surveys/acs/technical-documentation/table-and-geography-changes/2012/geography-changes.html
 
 # LA CENSUS TRACT TO RECODE
 # 06037930401 should be recoded to  06037137000 in all data files
 
 # CENSUS TRACTS
 # current from acsWork0     has 8057 tracts 
 # current cbdLinkCA         has 8036 (2010 data)
 # current cbddat0           has 8603! bad geocodes?        
 # something ??              has 8035 ... check...
 
 #temp <- popCensusCom$GEOID
 #junk <- cbdDat0[!(cbdDat0$GEOID %in% temp),]
 #junk <- junk[junk$GEOID != "",]
 #write.csv(junk,(paste0(upPlace,"/tempOutput/junk Tracts.csv")))
 
 # these records have a GEOID but not comID suggesting the GEOID is "bad"
 # junk <- filter(cbdDat0,is.na(comID) & GEOID != ""  & year > 2004)  
 # 651 records
 # length(unique(junk$GEOID))
 # 590 unique GEOID not in California (based on current link file)
 #  write.csv(table(junk$GEOID,junk$year),(paste0(upPlace,"/tempOutput/junk Tracts.csv")))
 
 # county missing from 3797 records     
 # junk <- filter(cbdDat0,is.na(county))   
 # 3797 records
 # countyFIPS blank=2145 and 999=1652 (but State="CA; based on "F71" only)
 #  write.csv(table(junk$year,junk$countyFIPS),(paste0(upPlace,"/tempOutput/missing County FIPS.csv")))
 
 # MAJOR cleaning issue!!!
 # junk <- filter(cbdDat0,is.na(gbd36))   
 # 82775 records where ICD10 does not map to gbd36 -- errors in info file!
 #  write.csv(table(junk$year,junk$countyFIPS),(paste0(upPlace,"/tempOutput/no ICD10 to gbd36.csv")))
 
# Potentially useful old code bits:


# could make aL and aU like this, or as below based on an input file:
# aL            <- c(   0, 5,15,25,35,45,55,65,75,85)
# aU            <- c(-1,4,14,24,34,44,54,64,74,84,999)


# "Manual' calcuation of age-adjustment
# popStandard <- readRDS(paste0(upPlace,"/upData/popStandard.RDS"))
# ageCounty   <- merge(ageCounty,popStandard,by = c("ageG"),all=TRUE)  # merge with "Standard" population

#calculate number of expected deaths in strata among standard population
#ageCounty$deathsE <- (ageCounty$Ndeaths/ageCounty$pop)*ageCounty$popStandard

# "manual" calculation of age-adjusted rates, AND using ageadjust.direct function from EpiTools package
# NOTE: oDeaths etc  != total deaths in other files because of missings removed# 
#   summarize(oDeaths = sum(Ndeaths),         # na.rm=TRUE not needed becuase of cleaning above
#             oPop    = sum(pop),
#             cRate   = 100000*oDeaths/oPop,
#             eDeaths = sum(deathsE),
#             ePop    = sum(popStandard),
#             aRate   = 100000*eDeaths/ePop)

# age-adjustment reference
# https://www.cdc.gov/nchs/data/nvsr/nvsr47/nvs47_03.pdf





