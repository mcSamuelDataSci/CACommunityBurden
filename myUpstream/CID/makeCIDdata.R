library(dplyr)
library(readr)
library(readxl)
library(fs)
library(stringr)

myDrive <- getwd()  
myPlace <- paste0(myDrive,"/myCBD") 
upPlace <- paste0(myDrive,"/myUpstream")


# NOTE - NO IDB Data on Open Data POrtal?
# idb.url <- "https://data.chhs.ca.gov/dataset/03e61434-7db8-4a53-a3e2-1d4d36d6848d/resource/75019f89-b349-4d5e-825d-8b5960fc028c/download/odp_idb_30apr19-13-47-36.csv"
# idb.dat <- read_csv(idb.url) %>%
#              mutate(Rate=as.numeric(str_remove(Rate, "[*]")),
#                   County=str_to_title(County) ) %>%
#                   select(Jurisdiction=County,Year,Sex,Disease,Cases,Rate,LCI=`Lower 95% CI`,UCI=`Upper 95% CI`) %>%
#                   mutate(Sex=str_to_title(Sex))


vpd.url <- "https://data.chhs.ca.gov/dataset/bd6a1ca8-dd41-4cbf-84a2-0e899af64b67/resource/a7a4d868-f6d2-49c9-8a11-904c69a0e3a0/download/izb-cases-by-county-and-year.csv"
vpd.dat <- read_csv(vpd.url) %>% 
             mutate(Sex="Total") %>%
             select(County=county, Year=year, Sex, Disease=disease, Cases=count)

# note -- TB data has local health jurisdictions, such that Berkeley, Long Beach, and Passadena data are not 
#  included in their respective counties
#  the "mutate", "group_by", and "summarize" code below "fix" this issue

tb.url  <- path(upPlace,"CID","tb_cases_rates_lhj.xlsx")
tb.dat  <- read_excel(tb.url) %>%
              mutate(County=ifelse(jurisdiction %in% c("Long Beach","Pasadena"), "Los Angeles",
                            ifelse(jurisdiction == "Berkeley", "Alameda",
                            jurisdiction))) %>%
               group_by(year,County) %>%       
               summarize(Cases=sum(cases)) %>%
               mutate(Sex="Total", Disease="TB") %>%
               select(County, Year = year, Sex, Disease, Cases)

std.url <- "https://data.chhs.ca.gov/dataset/4de76cd0-0ac9-4260-8ac3-0637acb444fb/resource/563ba92b-8ac5-48ec-9afd-2f515bbbad66/download/stds-by-disease-county-year-sex.csv"
std.dat <- read_csv(std.url)  %>%
  select(County, Year, Sex, Disease, Cases)

dcdc.dat <- bind_rows(std.dat, vpd.dat, tb.dat) 

diseasesODP <- dcdc.dat$Disease             
idb.dat     <- read_csv(paste0(upPlace,"/CID/dcdcData2017.csv")) 
idb.dat     <- idb.dat %>% filter(! Disease %in% diseasesODP) %>% mutate(Sex="Total")   

dcdc.dat    <- bind_rows(dcdc.dat, idb.dat) 

dcdc.dat    <- dcdc.dat %>% filter(Sex == "Total") %>% 
                            group_by(County, Disease) %>% 
                            filter(Year == max(Year)) %>%
                            mutate(County=ifelse(County=="California","CALIFORNIA",County))


write_csv(dcdc.dat, paste0(upPlace,"/CID/dcdcData.csv")) 


  
