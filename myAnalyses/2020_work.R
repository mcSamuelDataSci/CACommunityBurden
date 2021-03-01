  server <- T
  if (!server) source("g:/FusionData/Standards/FusionStandards.R")
  if (server) source("/mnt/projects/FusionData/Standards/FusionStandards.R")

# dat.0.1   <- readRDS(path(ccbData, "real/datCounty_RACE_AGE_Q.RDS"))

dat.0      <- readRDS(path(ccbData, "real/datCounty_Q.RDS"))  %>%
                left_join(deathCauseLink, by = "causeCode")

dat.00     <- readRDS(path(ccbData, "real/datCounty_M.RDS"))  %>%
                left_join(deathCauseLink, by = "causeCode")


tDat <- dat.0 %>% filter(Level == "lev2", county=="CALIFORNIA", sex == "Total", raceCode == "Total")  %>%
          select(year,quarter,Ndeaths,causeName,causeNameShort,aRate,cDeathRate)   # population,






tDat2 <- filter(tDat,
                year %in% 2017:2020,
                quarter %in% c("02","03") ) %>% select(-cDeathRate) %>%
  #group_by(causeName, year, quarter)  %>%
  pivot_wider(names_from = year,values_from=c(Ndeaths,aRate)) %>%               # , population
  mutate(change17_18 = round(100*(aRate_2018 - aRate_2017) / aRate_2017  ,1),
         change18_19 = round(100*(aRate_2019 - aRate_2018) / aRate_2018  ,1),  
         change19_20 = round(100*(aRate_2020 - aRate_2019) / aRate_2019  ,1) 
  )   %>%
  filter(Ndeaths_2020 > 500) %>% select(-causeNameShort)
 





tDat2 <- filter(tDat,
                year %in% 2017:2020,
                quarter %in% c("02","03") ) %>% select(-aRate) %>%
  #group_by(causeName, year, quarter) %>%
  pivot_wider(names_from = year,values_from=c(Ndeaths,cDeathRate)) %>%               # , population
  mutate(change17_18 = round(100*(cDeathRate_2018 - cDeathRate_2017) / cDeathRate_2017  ,1),
         change18_19 = round(100*(cDeathRate_2019 - cDeathRate_2018) / cDeathRate_2018  ,1),  
         change19_20 = round(100*(cDeathRate_2020 - cDeathRate_2019) / cDeathRate_2019  ,1) 
  )   %>%
  filter(Ndeaths_2020 > 50) %>% select(-causeNameShort)



# tDat2 <- filter(tDat,
#                 year %in% 2016:2020)  %>%
#   mutate(yearG = ifelse(year == 2020, "2020","2016.19"),
#          quarter = as.numeric(quarter)) %>%
#   group_by(causeNameShort,quarter, yearG) %>%
#   summarise(mRate = mean(aRate),
#             bigN  = sum(Ndeaths))  %>% pivot_wider(names_from = yearG,values_from=mRate:bigN) %>%
#   mutate(percentIncrease = round(100*(mRate_2020 - mRate_2016.19) / mRate_2016.19  ,1) )



tDat4  <- tDat3 %>% filter(causeNameShort %in% c("Hypertensive heart disease","	
Mouth and oropharynx cancers","Drug poisonings","Mental Health disorders"))


# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------



library(zoo)
#https://stackoverflow.com/questions/31198144/formatting-a-scale-x-continuous-axis-with-quarterly-data

dat.1 <- filter(dat.0,
                Level == "lev2",
                raceCode == "Total",
                county   == "CALIFORNIA") %>%
         mutate( quart = as.yearqtr(paste0(year,"-",as.numeric(quarter))),
                quart2 = paste0(year,"-",quarter))
