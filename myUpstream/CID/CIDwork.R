library(dplyr)
library(readr)
library(readxl)
library(fs)
library(stringr)


myYear <- 2017

myDrive <- getwd()  
myPlace <- paste0(myDrive,"/myCBD") 
upPlace <- paste0(myDrive,"/myUpstream")


std.url <- "https://data.chhs.ca.gov/dataset/4de76cd0-0ac9-4260-8ac3-0637acb444fb/resource/563ba92b-8ac5-48ec-9afd-2f515bbbad66/download/stds-by-disease-county-year-sex.csv"

idb.url <- "https://data.chhs.ca.gov/dataset/03e61434-7db8-4a53-a3e2-1d4d36d6848d/resource/75019f89-b349-4d5e-825d-8b5960fc028c/download/odp_idb_30apr19-13-47-36.csv"

vpd.url <- "https://data.chhs.ca.gov/dataset/bd6a1ca8-dd41-4cbf-84a2-0e899af64b67/resource/a7a4d868-f6d2-49c9-8a11-904c69a0e3a0/download/izb_odp_final_03262019.csv"

tb.url <- path(upPlace,"CID","opd-tb-by-jur-2005-2017.xlsx")


std.dat <- read_csv(std.url)  %>%
  select(Jurisdiction=County,Year,Sex,Disease,Cases,Rate,LCI=`Lower 95% CI`,UCI=`Upper 95% CI`)

idb.dat <- read_csv(idb.url) %>%
  mutate(Rate=as.numeric(str_remove(Rate, "[*]")),
         County=str_to_title(County)
         ) %>%
  select(Jurisdiction=County,Year,Sex,Disease,Cases,Rate,LCI=`Lower 95% CI`,UCI=`Upper 95% CI`) %>%
  mutate(Sex=str_to_title(Sex))

vpd.dat <- read_csv(vpd.url) %>% 
  mutate(Sex="Total") %>%
  select(Jurisdiction=county,Year=year,Sex,Disease=disease,Cases=count)

tb.dat  <- read_excel(tb.url) %>%
  mutate(Sex="Total",Disease="TB") %>%
  select(Jurisdiction,Year,Sex,Disease,Cases,Rate=`Rate per 100,000 population`)




dcdc.dat <- bind_rows(std.dat,idb.dat,vpd.dat,tb.dat) 
             
    
# note -- TB data has local health jurisdictions, such that Berkeley, Long Beach, and Passadena data are not 
#  included in their respective counties
#  the "mutate", "group_by", and "summarize" code below "fix" this issue

dcdc.work <- dcdc.dat %>%
              filter(Sex=="Total",Year==2017) %>%
              mutate(County=ifelse(Jurisdiction %in% c("Long Beach","Pasadena"), "Los Angeles",
                            ifelse(Jurisdiction == "Berkeley", "Alameda",
                            Jurisdiction))
              ) %>%
              group_by(Year,County,Disease) %>%       
              summarize(Cases=sum(Cases)) %>%
              select(Year, County, Disease, Cases) %>% ungroup() %>%
              mutate(County=ifelse(County=="California","CALIFORNIA",County))
             
write_csv(dcdc.work, "Z:/lghcBurdenView/Data/CID/dcdcData2017.csv") 


  
