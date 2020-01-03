


oshpd_PDD_any.t <- readRDS(file=path(myPlace, "myData/",whichData,"/oshpd_PDD_any.rds")) %>%
                    mutate(year=2016) %>% ## TODO fix year
                    select(year, county, sex, ccsCode, Nany = n_hosp_any)

countyOSHPD.t   <- readRDS(file = path(myPlace, "myData/",whichData,"/countyOSHPD.rds")) %>%
                   filter(type == "n_hosp") %>%
                   select(year, county, sex, ccsCode=ccs_diagP,Nprimary=measure)

nrow(oshpd_PDD_any.t)
nrow(countyOSHPD.t)


primary_any <- left_join(oshpd_PDD_any.t,countyOSHPD.t,by=c("year","county","sex","ccsCode")) %>%
                       mutate(Nprimary = ifelse(is.na(Nprimary),0,Nprimary)) %>%
                       mutate(Nother = Nany - Nprimary,
                              percentPrimary = Nprimary/Nany,
                              percentOther   = Nother/Nany)


ccsMap  <- as.data.frame(read_excel( path(myPlace,"myInfo/CCS Code and Names Linkage.xlsx")))
ccsMap  <- ccsMap %>% 
  mutate(ccsCode = str_pad(ccsCode, 5,"left",pad="o")) %>% select(ccsCode,ccsName,birthPregnancyRelated)


primary_any <- left_join(primary_any,ccsMap,by="ccsCode") %>%
                 filter(Nany > 50,
                        ccsCode != "oo259")  ### TODO need to study this



primary_any_NOPREG <- primary_any %>% filter(is.na(birthPregnancyRelated))
