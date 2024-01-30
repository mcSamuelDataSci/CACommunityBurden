#== LOAD AND PROCESS POPULATION DATA ==============================================================

# ungrouping important for subsequent data set merging

popCounty <- readRDS(paste0(ccbUpstream, "upData/lhj-population-ars.RDS")) %>% 
  ungroup()

# For County and Region
popCountySex     <- filter(popCounty,ageGroup == "Total", raceCode == "Total") %>% select(-ageGroup, -raceCode) # no need for ageGroup
popCountySexAgeG <- filter(popCounty,ageGroup != "Total", raceCode == "Total") %>% select(-raceCode)

# Standard 2000 Age Group Pop - Used for age-adjustment
popStandard         <- ageMap    %>% mutate(ageGroup = ageLabel)
popStandard_EDU     <- ageMap_EDU  %>% mutate(ageG_EDU = ageLabel)

# == CALCULATE CRUDE RATES ========================================================================

# -- COUNTY -------------------------------------------------------------------

c.t1      <- calculateYLLmeasures(c("county","year","sex","lev0"),"lev0")
c.t2      <- calculateYLLmeasures(c("county","year","sex","lev1"),"lev1")
c.t3      <- calculateYLLmeasures(c("county","year","sex","lev2"),"lev2")
c.t4      <- calculateYLLmeasures(c("county","year","sex","lev3"),"lev3")
datCounty <- bind_rows(c.t1,c.t2,c.t3,c.t4)

s.t1      <- calculateYLLmeasures(c(         "year","sex","lev0"),"lev0")
s.t2      <- calculateYLLmeasures(c(         "year","sex","lev1"),"lev1")
s.t3      <- calculateYLLmeasures(c(         "year","sex","lev2"),"lev2")
s.t4      <- calculateYLLmeasures(c(         "year","sex","lev3"),"lev3")
datState  <- bind_rows(s.t1,s.t2,s.t3,s.t4)
datState$county = STATE

l.t1      <- calculateYLLmeasures(c("city_lhj", "year","sex","lev0"),"lev0")
l.t2      <- calculateYLLmeasures(c("city_lhj", "year","sex","lev1"),"lev1")
l.t3      <- calculateYLLmeasures(c("city_lhj", "year","sex","lev2"),"lev2")
l.t4      <- calculateYLLmeasures(c("city_lhj", "year","sex","lev3"),"lev3")
datCityLHJ  <- bind_rows(l.t1,l.t2,l.t3,l.t4) %>% 
  filter(!is.na(city_lhj)) %>% 
  rename(county = city_lhj)

datCounty <- bind_rows(datCounty,datCityLHJ,datState)

# MERGE Death and Population files
datCounty <-  merge(datCounty,popCountySex,by = c("year","county","sex"))

# CALCULATE RATES
datCounty <- calculateRates(datCounty,1)






# makes dataframes of all possible combinations
month    <- data.frame(month     = sort(unique(cbdDat0$month)),                   stringsAsFactors = FALSE)
quarter  <- data.frame(quarter  = sort(unique(cbdDat0$quarter)),                  stringsAsFactors = FALSE)
year     <- data.frame(year     = sort(unique(cbdDat0$year))) # these "vectors" need to be dataframes for the sq merge below to work
yearG5   <- data.frame(yearG5   = sort(unique(cbdDat0$yearG5)),                   stringsAsFactors = FALSE) 
yearG3   <- data.frame(yearG3   = sort(unique(cbdDat0$yearG3)),                   stringsAsFactors = FALSE)
CAUSE1   <- data.frame(causeCode    = allCauseCodes,                                  stringsAsFactors = FALSE) 
CAUSE2   <- data.frame(causeCode    = CAUSE1[nchar(as.character(CAUSE1$causeCode)) < 4,], stringsAsFactors = FALSE)
CAUSE3   <- data.frame(causeCode    = CAUSE1[nchar(as.character(CAUSE1$causeCode)) < 2,], stringsAsFactors = FALSE)
sex      <- data.frame(sex      = c("Male","Female","Total"),                     stringsAsFactors = FALSE)
ageGroup     <- data.frame(ageGroup     = sort(unique(cbdDat0$ageGroup)),                     stringsAsFactors = FALSE)
county   <- data.frame(county   = c(geoMap$countyName,STATE, unique(filter(cbdDat0, !is.na(city_lhj))$city_lhj)), stringsAsFactors = FALSE) 
region   <- data.frame(region   = c(unique(regionLink$region),STATE),                 stringsAsFactors = FALSE) 
comID    <- data.frame(comID    = unique(mssaLink[,"comID"]),                    stringsAsFactors = FALSE)
GEOID    <- data.frame(GEOID    = mssaLink[,"GEOID"],                            stringsAsFactors = FALSE)
raceCode <- data.frame(raceCode = sort(unique(cbdDat0$raceCode)),                 stringsAsFactors = FALSE)
# rural <- data.frame(ruca = unique(popRuralSex$ruca), stringsAsFactors = FALSE)

yearForQuarterly <- data.frame(year = forQuarter_selectYears) # forQuarter_selectYears assigned at the top of script. Run into memory issues when too many years are selected
yearForRE1 <- data.frame(year = RE_1year_years)
raceCodeForQuarterly <- raceCode %>% tibble::add_row(raceCode = "Total") # For the quarterly dataset, we need to add Total race to crossjoin in fullmat

# other cool approach from Adam:
# fullMatCounty <- Reduce(function(...) merge(..., all = TRUE), list(county, year, CAUSE, sex, ageGroup))
fullMatCounty          <- sqldf(" select * from  county cross join year   cross join CAUSE1 cross join sex cross join ageGroup")  %>% mutate(tester=0)


# -- COUNTY (age-adjusted) ----------------------------------------------------

tA1      <- cbdDat0 %>% group_by(county,year, sex, ageGroup,causeCode=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA2      <- cbdDat0 %>% group_by(county,year, sex, ageGroup,causeCode=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA3      <- cbdDat0 %>% group_by(county,year, sex, ageGroup,causeCode=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA4      <- cbdDat0 %>% group_by(county,year, sex, ageGroup,causeCode=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) 
tA5      <- cbdDat0 %>% group_by(       year, sex, ageGroup,causeCode=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA6      <- cbdDat0 %>% group_by(       year, sex, ageGroup,causeCode=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA7      <- cbdDat0 %>% group_by(       year, sex, ageGroup,causeCode=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)
tA8      <- cbdDat0 %>% group_by(       year, sex, ageGroup,causeCode=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% mutate(county=STATE)

tA9 <- cbdDat0 %>% filter(!is.na(city_lhj)) %>% group_by(city_lhj, year, sex, ageGroup,causeCode=lev0) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% rename(county=city_lhj)
tA10 <- cbdDat0 %>% filter(!is.na(city_lhj)) %>% group_by(city_lhj, year, sex, ageGroup,causeCode=lev1) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% rename(county=city_lhj)
tA11 <- cbdDat0 %>% filter(!is.na(city_lhj)) %>% group_by(city_lhj, year, sex, ageGroup,causeCode=lev2) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% rename(county=city_lhj)
tA12 <- cbdDat0 %>% filter(!is.na(city_lhj)) %>% group_by(city_lhj, year, sex, ageGroup,causeCode=lev3) %>% summarize(Ndeaths = n(), YLL = sum(yll,na.rm=TRUE) ) %>% rename(county=city_lhj)

datAA1 <- bind_rows(tA1,tA2,tA3,tA4,tA5,tA6,tA7,tA8, tA9, tA10, tA11, tA12)  %>% ungroup()  # UNGROUP HERE!!!!

# DATA CLEANING ISSUES as above
datAA1 <- filter(datAA1,!is.na(ageGroup))     # remove 403 records with missing age (0.065% of deaths)  -- impact of this?
# datAA1 <- filter(datAA1,!is.na(causeCode))  # remove 6955 records with missing causeCode
datAA1 <- filter(datAA1,!is.na(county))   # remove 758 records with missing county
# datAA1 <- filter(datAA1,!is.na(sex))    # remove 

ageCounty   <- full_join(fullMatCounty,datAA1 ,by = c("county","year","sex","ageGroup","causeCode"))  %>%    # merge death data and "fullMatCounty"
  full_join(popCountySexAgeG, by = c("county","year","sex","ageGroup") )             %>%    # merge population
  full_join(popStandard[,c("ageGroup","US2000POP")],          by="ageGroup")                    # merge standard population

ageCounty$Ndeaths[is.na(ageCounty$Ndeaths)] <- 0    # if NA deaths in strata change to "0"
ageCounty$YLL[is.na(ageCounty$YLL)]         <- 0    # if NA deaths in strata change to "0"

countyAA <- ageCounty %>% group_by(county,year,sex,causeCode) %>%
  summarize(oDeaths = sum(Ndeaths,na.rm=TRUE),
            aRate   = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE     = ageadjust.direct.SAM(count=Ndeaths, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000, 
            YLL.adj.rate   = ageadjust.direct.SAM(count=YLL, population=population, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) # CONFIRM


countyAA <- countyAA[!(countyAA$oDeaths==0),c("county","year","sex","causeCode","aRate","aLCI","aUCI","aSE","YLL.adj.rate")]  # remove strata with no deaths and select columns  


# == MERGE CRUDE AND AGJECT RATES AND CLEAN UP ====================================================

# For sex-specific cancers =================================
sexCauseList <- list("Female" = c("B09", "B10", "B11"), 
                     "Male" = c("B12"))

sexCause <- function(myData, myCauseCodes = sexCauseList) {
  
  femaleCause <- myData %>% 
    filter(sex == "Female", causeCode %in% myCauseCodes$Female) %>% 
    bind_rows(mutate(., sex = "Total"))
  
  
  maleCause <- myData %>% 
    filter(sex == "Male", causeCode %in% myCauseCodes$Male) %>% 
    bind_rows(mutate(., sex = "Total"))
  
  
  tDat <- myData %>% 
    filter(!causeCode %in% c(myCauseCodes$Female, myCauseCodes$Male)) %>% 
    bind_rows(femaleCause, maleCause)
  
  return(tDat)
  
}


# COUNTY ----------------------------------------------------------------------

datCounty <- merge(datCounty,countyAA ,by = c("county","year","sex","causeCode"),all=TRUE)

datCounty <- sexCause(datCounty)

# SMR Calculations START

datState  <- datCounty  %>% 
  filter(county == STATE) %>%
  mutate(stateCrudeRate = cDeathRate,
         stateAdjustedRate = aRate) %>%
  select(year,sex,Level,causeCode,stateCrudeRate,stateAdjustedRate)

if (!subSite & whichDat == "real") saveRDS(datState, path(ccbData,"datState.RDS"))
if ( subSite)                      datState <- readRDS(path(ccbData,"datState.RDS"))

datCounty  <- merge(datCounty,datState,by = c("year","sex","Level","causeCode")) %>%
  mutate(SMRcrude = cDeathRate / stateCrudeRate,
         SMR      = aRate      / stateAdjustedRate)

# SMR Calculations END


datCounty <-  datCounty %>% 
  filter(!(is.na(causeCode)))                                       %>% # removes "Level3" NA (most 'causes' are NA on Level3) 
  select(-stateCrudeRate,-stateAdjustedRate)                    %>%
  mutate_if(is.numeric, signif,digits = myDigits)               %>%  # much smaller file and easier to read
  mutate(county = ifelse(county==STATE, toupper(STATE),county))      # e.g. California --> CALIFORNIA  




source(path(ccbUpstream,"upstreamInfo","suppressionFunction.R"))

# COUNTY
gBy       <-  c("county","year","Level","causeCode")         # FOR MAIN
datCounty <- mutate(datCounty, supIndicator = mySuppress(datCounty,gBy,"Ndeaths"))
datCounty <- filter(datCounty, 
                    supIndicator != 1, 
                    !(causeCode=="A09" & sex %in% c("Male","Female"))
) %>%
  select(-supIndicator)



# Compare ============================================================================================

ccbCounty <- readRDS(paste0(ccbData, "real/datCounty.RDS")) %>% 
  arrange(year, sex, causeCode, county)


tDat <- datCounty %>% 
  filter(year != 2023, !county %in% c("Berkeley", "Long Beach", "Pasadena", "Alameda HD", "Los Angeles HD")) %>% 
  arrange(year, sex, causeCode, county)

nrow(ccbCounty)
nrow(tDat)

identical(ccbCounty, tDat) # County and state levl identical!

## Inspect city lhjs

tDat <- datCounty %>% 
  filter(year != 2023, county %in% c("Berkeley", "Long Beach", "Pasadena", "Alameda HD", "Los Angeles HD", "Alameda", "Los Angeles"), causeCode == "0") %>% 
  select(year, sex, county, Ndeaths, YLL, population, cDeathRate, aRate)

library(directlabels)

ggplot(filter(tDat, sex == "Total"), aes(x = year, y = aRate)) +
  geom_line(aes(color = county), size = 1.5) +
  scale_x_continuous(expand = expansion(add = c(0, 5))) +
  geom_dl(aes(label = county), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1, font="bold"))
  
