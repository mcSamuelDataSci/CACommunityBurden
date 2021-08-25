server <- T

if (server) source('/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R')
if (!server) source('G:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R')

source(paste0(fusionPlace, 'SDOH/pullACS_function.R'))

years <- 2016:2019


grabSDOH <- function(myYears, mySDOH) {
  
  sdohList <- lapply(myYears, function(x) {
    
    sdoh <- pullACS(State=06, # 06 = California
                    Geography="tract", # Options: county, mssa, tract, zcta... for acs1, puma
                    Survey="acs5", # Options: "acs1", "acs5"
                    Year=x,
                    moeLevel=90, # margin of error level
                    asList = F, # Want as a list, or as one data frame?
                    server = T, # Set to T if in R Studio Pro; otherwise set to F
                    sdohVars = mySDOH) %>%
      mutate(year = x)
    
    if (x == 2019) {
      
      sdoh_2020 <- sdoh %>%
        mutate(year = 2020)
      
      sdoh_2021 <- sdoh %>%
        mutate(year = 2021)
      
      sdoh <- bind_rows(sdoh, sdoh_2020) %>%
        bind_rows(sdoh_2021)
      
      
    }
    
    return(sdoh)
    
    
  })
  
  
  
}


# Race/Ethnicity

acs_raceList <- grabSDOH(myYears = years,
                     mySDOH = c("asian", "black", "latino", "white", "total"))

acs_race <- bind_rows(acs_raceList)

saveRDS(acs_race, paste0(ccbUpstream, 'E1.SDOH/acs_race.RDS'))


# Poverty

acs_povList <- grabSDOH(myYears = years,
                        mySDOH = "poverty100")

acs_pov <- bind_rows(acs_povList) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(pov = cut(estimate, breaks = c(-1, 0.10, 0.20, 0.30, 100),
                   labels = c("<10%", "10-<20%", "20-<30%", ">=30%"), right = F)
  ) %>%
  ungroup() %>%
  mutate(pov = as.character(pov))

acs_pov_total <- acs_pov %>%
  mutate(pov = ifelse(is.nan(estimate), pov, "Total"))

acs_pov_final <- bind_rows(acs_pov, acs_pov_total)

saveRDS(acs_pov_final, paste0(ccbUpstream, 'E1.SDOH/acs_poverty.RDS'))




# Overcrowding

acs_ocList <- grabSDOH(myYears = years,
                        mySDOH = "overcrowding")

acs_oc <- bind_rows(acs_ocList) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(overcrowding = cut(estimate, breaks = c(-1, 0.05, 0.1, 0.15, 100),
                            labels = c("<5%", "5-<10%", "10-<15%", ">=15%"), right = F)
  ) %>%
  ungroup() %>%
  mutate(overcrowding = as.character(overcrowding))

acs_oc_total <- acs_oc %>%
  mutate(overcrowding = ifelse(is.nan(estimate), overcrowding, "Total"))

acs_oc_final <- bind_rows(acs_oc, acs_oc_total)

saveRDS(acs_oc_final, paste0(ccbUpstream, 'E1.SDOH/acs_overcrowding.RDS'))
