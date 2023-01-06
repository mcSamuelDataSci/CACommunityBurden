server <- T
if (!server) source("g:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
if (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")

# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------


# General Infectious Disease - Infectious Disease Branch IDB---------------------------------------------------------------------------------------------------
idb.url <- "https://data.chhs.ca.gov/dataset/03e61434-7db8-4a53-a3e2-1d4d36d6848d/resource/75019f89-b349-4d5e-825d-8b5960fc028c/download/idb_odp_2001-2019.csv"
idb.dat <- read_csv(idb.url) %>%
             mutate(Rate=as.numeric(str_remove(Rate, "[*]")),
                  County=str_to_title(County) ) %>%
                  mutate(Sex=str_to_title(Sex)) %>%
            select(County, Year, Sex, Disease, Cases) %>%
            mutate(Branch = "IDB")


# Vaccine Preventable Diseases ---------------------------------------------------------------------------------------------------------------------------------
vpd.url <- "https://data.chhs.ca.gov/dataset/bd6a1ca8-dd41-4cbf-84a2-0e899af64b67/resource/a7a4d868-f6d2-49c9-8a11-904c69a0e3a0/download/izb-cases-by-county-and-year.csv"
vpd.dat <- read_csv(vpd.url) %>% 
             mutate(Sex="Total") %>%
          #  select(County=county, Year=year, Sex, Disease=disease, Cases=count) %>%
             select(County, Year, Sex, Disease, Cases=Count) %>%
             mutate(Branch = "VPD")


# TB -------------------------- ---------------------------------------------------------------------------------------------------------------------------------
# can not read tb excel file directly from ODP; for now, downloaded manually and "tb-" added to front of file name
# tb.url <- "https://data.chhs.ca.gov/dataset/0530d22a-127b-4524-9611-9850fb47e494/resource/fb4fbe6d-d5e9-4290-b3ee-7f8f1867839d/download/case-rates-by-lhj.xlsx"
# tb.dat  <- read_excel(tb.url)          

# note -- TB data has local health jurisdictions, such that Berkeley, Long Beach, and Pasadena data are not included in their respective counties
#  the "mutate", "group_by", and "summarize" code below "fix" this issue


tb.url  <- path(ccbUpstream,"CID","case-rates-by-lhj.xlsx")
tb.dat  <- read_excel(tb.url) %>%
              mutate(County=ifelse(Jurisdiction %in% c("Long Beach","Pasadena"), "Los Angeles",
                            ifelse(Jurisdiction == "Berkeley", "Alameda",
                            Jurisdiction))) %>%
               group_by(Year, County) %>%       
               dplyr::summarize(Cases=sum(Cases)) %>%
               mutate(Sex="Total", Disease="TB") %>%
               select(County, Year, Sex, Disease, Cases) %>%
               ungroup() %>%
               mutate(Branch = "TB")



# Sexually Transmitted Diseases ------------------------------------------------------------------------------------------------------------------------------------
std.url <- "https://data.chhs.ca.gov/dataset/4de76cd0-0ac9-4260-8ac3-0637acb444fb/resource/563ba92b-8ac5-48ec-9afd-2f515bbbad66/download/stds-by-disease-county-year-sex.csv"
std.dat <- read_csv(std.url)  %>%
             select(County, Year, Sex, Disease, Cases) %>%
             mutate(Branch = "STD")


# ===================================================================================================================================================================

dcdc.dat <- bind_rows(idb.dat, std.dat, vpd.dat, tb.dat)  %>% 
                        mutate(County=ifelse(County=="California" | County=="ACalifornia" ,"CALIFORNIA",County))


dcdc.bestyear  <- dcdc.dat   %>% 
                    group_by(County, Disease) %>%
                    filter(Year == max(Year), Sex == "Total") # NOTE: MAX year differs by data source


write_csv(dcdc.bestyear, paste0(ccbUpstream,"/CID/dcdcData.csv")) 

# ----------------------------------------------------------------------------------------------

popCounty    <- readRDS(path(ccbUpstream,"upData/popCounty.RDS")) %>% ungroup()  %>%
                         filter(ageGroup == "Total", raceCode == "Total") %>% 
                         select(Year = year, County = county, Sex = sex, Pop = population) 

dcdc.dat.app <- left_join(dcdc.dat, popCounty, by = c("County", "Year", "Sex")) %>%
                   mutate(Rate = Cases*100000/Pop) %>%
                   filter(!is.na(Cases))

dat.ci       <- epitools::pois.exact(dcdc.dat.app$Cases, pt = dcdc.dat.app$Pop, conf.level = 0.95) %>%
                   select(rate, LCI = lower, UCI = upper)  * 100000  
  
dcdc.dat.app.plus <-bind_cols(dcdc.dat.app, dat.ci)
write_csv(dcdc.dat.app.plus , paste0(ccbUpstream,"/CID/dcdcData.app.csv")) 

