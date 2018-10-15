crap <- ageCounty

crap <- datAA1
crap <- countyAA


crap3 <- filter(crap,county=="California",CAUSE=="D05",sex !="Total",year>2014)





crap4   <- crap3 %>% group_by(county,year,sex,CAUSE) %>%
           summarize(oDeaths = sum(Ndeaths,na.rm=TRUE),
                     oPop    = sum(pop,na.rm=TRUE),                 ###NEEDED THIS
                     oRate   = 100000*oDeaths/oPop)





crap4   <- crap3 %>% group_by(county,year,sex,CAUSE) %>%
  summarize(oDeaths = sum(Ndeaths,na.rm=TRUE))
            

            
            

countyAA <- ageCounty %>% group_by(county,year,sex,CAUSE) %>%
  summarize(oDeaths = sum(Ndeaths,na.rm=TRUE),
            aRate   = ageadjust.direct(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000,
            aLCI    = ageadjust.direct(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[3]*100000,
            aUCI    = ageadjust.direct(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[4]*100000, 
            aSE     = ageadjust.direct(count=Ndeaths, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[5]*100000, 
            YLL.adj.rate   = ageadjust.direct(count=YLL, pop=pop, rate = NULL, stdpop=US2000POP, conf.level = 0.95)[2]*100000) # CONFIRM





crapX <-  filter(datCounty,county=="CALIFORNIA",CAUSE=="D05",sex !="Total",year>2014)





popCountySexTotAgeG

crapXx <-  filter(popCountySexTotAgeG,county=="California",sex !="Total",year>2014)

CRAPxxx <- crapXx %>% group_by(year,sex) %>%
  summarize(pop = sum(pop))


popCountySexTotAgeG









crap3 <- filter(datCounty,county=="CALIFORNIA",CAUSE=="D05",sex !="Total") %>%
            select(county,year,sex,CAUSE,Ndeaths,pop,cDeathRate,aRate)










crap2 <- filter(crap,county=="California",CAUSE=="D05",sex !="Total",(ageG >=  "55" & ageG < "75"))


crap2 <- filter(crap,county=="California",CAUSE=="D05",sex !="Total")





p1 <- ggplot( crap2, aes( x = year, y = pop) ) + geom_bar( stat = "identity" )  
p1 <- p1 + facet_grid(cols = vars(ageG), rows = vars(sex))
p1




junk <- filter(popCountySexTotAgeG,county=="California",sex !="Total")





p1 <- ggplot( junk, aes( x = year, y = pop) ) + geom_bar( stat = "identity" )  
p1 <- p1 + facet_grid(cols = vars(ageG), rows = vars(sex))
p1



junk2 <- tA7
junk3 <- filter(junk2,CAUSE=="D05",sex !="Total")






> junk <- filter(ageCounty, CAUSE=="D05", county=="California",sex=="Female",year==2017)
> junk
county year CAUSE    sex     ageG Ndeaths      YLL     pop
1 California 2017   D05 Female    0 - 4       1    91.94 1242519
2 California 2017   D05 Female  15 - 24       1    76.04 2637379
3 California 2017   D05 Female  35 - 44       2   105.40 2605410
4 California 2017   D05 Female  45 - 54      10   422.65 2620592
5 California 2017   D05 Female   5 - 14       1    86.02 2483274
6 California 2017   D05 Female  55 - 64     133  4223.31 2498268
7 California 2017   D05 Female  65 - 74     697 15696.05 1735422
8 California 2017   D05 Female  75 - 84    3378 47179.71  891197
9 California 2017   D05 Female 85 - 999   12325 82144.36  464601
> junk <- filter(ageCounty, CAUSE=="0", county=="California",sex=="Female",year==2017)
> junk
county year CAUSE    sex     ageG Ndeaths       YLL     pop
1  California 2017     0 Female    0 - 4     980  89844.80 1242519
2  California 2017     0 Female  15 - 24     738  52870.45 2637379
3  California 2017     0 Female  25 - 34    1371  85231.68 2717999
4  California 2017     0 Female  35 - 44    2440 127524.46 2605410
5  California 2017     0 Female  45 - 54    5851 246424.93 2620592
6  California 2017     0 Female   5 - 14     224  18445.13 2483274
7  California 2017     0 Female  55 - 64   13064 427170.74 2498268
8  California 2017     0 Female  65 - 74   20050 468658.61 1735422
9  California 2017     0 Female  75 - 84   29052 422769.86  891197
10 California 2017     0 Female 85 - 999   54808 383325.28  464601
> 


