junk <- filter(datCounty_AGE_3year,county=="CALIFORNIA",CAUSE=="0",sex=="Total",yearG3=="2016-2018")


100*junk[10,"Ndeaths"]/sum(junk$Ndeaths)
100*junk[10,"pop"]/sum(junk$pop,na.rm=TRUE)
