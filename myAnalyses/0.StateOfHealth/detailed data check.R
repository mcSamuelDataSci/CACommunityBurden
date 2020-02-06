temp <- readRDS(file= path(securePath,"/myData/cbdDat0-INVESTIGATION-FILE.RDS"))

library(summarytools)   

temp1 <- filter(temp,lev2 %in% c("E02","E03"),sex=="Total")  %>%
             group_by(year,lev2,ICD10) %>%
             summarize(nDeath = n())





temp2 <- filter(temp,lev2 %in% c("E02","E03"),sex=="Total")  %>%
  group_by(year,lev2) %>%
  summarize(nDeath = n()) %>%
  pivot_wider(names_from = lev2,values_from = nDeath)

write_csv(temp2,"check poisen.csv")



temp1 <- filter(temp,lev2 %in% c("A99"),sex=="Total",year==2018)  %>%
  group_by(year,lev2,ICD10) %>%
  summarize(nDeath = n())

write_csv(temp1,"other infectious.csv")




temp1 <- filter(temp,lev2 %in% c("Z01","Z02"),sex=="Total",year==2018)  %>%
  group_by(year,lev2,ICD10) %>%
  summarize(nDeath = n())
