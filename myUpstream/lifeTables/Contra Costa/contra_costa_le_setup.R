setwd("C:/Users/ldiemoz/Documents/le")
setwd("/mnt/projects/FusionData/0.CCB/myUpstream/lifeTables/Contra Costa")
      
library("readxl")
library("dplyr")
library("readr")

  deaths <- read_xlsx("test.xlsx") %>% 
   mutate(year=2018)
  
 
  nxCity <- read_csv('ContraCosta_pop.csv')   %>%
    mutate(sex = ifelse(sex == "Total", "T",
                 ifelse(sex == "Male",  "M",
                 ifelse(sex == "Female","F",NA)))) %>%
    filter(year==2018) %>% 
    mutate(Nx=Nx*5)
  
  
  mxContraCosta <- full_join(nxCity, deaths, by = c("year", "agell", "ageul", "GEOID", "sex", "raceCode"))
 
  
   source("calculate_le_function.R")
  
 
   
# agell note: must be numeric    
  
  le.richmond  <- calculateLE( mxContraCosta %>% arrange(agell) %>% filter(GEOID == "Richmond") )
  le.san_pablo <- calculateLE( mxContraCosta %>% arrange(agell) %>% filter(GEOID == "San Pablo") )

  
  write_csv(bind_rows(le.richmond,le.san_pablo),"ccle.csv")
   