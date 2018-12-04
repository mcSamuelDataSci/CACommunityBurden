library(dplyr)
library(epitools)

githubURL <- "https://raw.githubusercontent.com/mcSamuelDataSci/CACommunityBurden/master/myCBD/myData/fake/forZev.ageCounty.RDS"
download.file(githubURL,"temp.rds", method="curl")
ageCounty <- readRDS("temp.rds")


# THIS WORKS - INEFFICIENT
countyAA <- ageCounty %>% group_by(county,year,sex,CAUSE) %>%
  summarize(aRate   = ageadjust.direct(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE     = ageadjust.direct(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000 
            ) 

# MY ATTEMPTS... CLOSE BUT NO CIGAR
countyAA_try2 <- ageCounty %>% group_by(county,year,sex,CAUSE) %>%
  summarize(aMulti = list(unique(
                            round(
                              ageadjust.direct(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)*100000,2)
                            )
                          )
            ) 


# CLOSER, STILL NO CIGAR
countyAA_try3 <- ageCounty %>% 
  group_by(county, year, sex, CAUSE) %>% {
    sam <- ageadjust.direct(
      count = .$Ndeaths, 
      pop = .$pop, 
      rate = NULL, 
      stdpop = .$US2000POP, 
      conf.level = 0.95
    )
    
    summarize(
      .,
      aRate = sam[2] * 100000,
      aLCI = sam[3] * 100000,
      aUCI = sam[4] * 100000, 
      aSE  = sam[5] * 100000
    ) 
  }
  
  
  