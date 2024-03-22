server <- F
if (!server) source("g:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
if (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")

library(readxl)
library(purrr)

dat <- readRDS(paste0(securePlace, "myData/mcod_processed_deaths.RDS"))


# CHECK -- SHOULD RETURN 0 ROWS
dat %>% 
  filter(if_any(starts_with("causeCode_2"), ~. == "D99")) %>% 
  filter(causeCode_primary == "D99")

# Part 1: Get Ndeaths for each Cause - Primary and Other -----------------------------------------------

primaryNdeaths <- dat %>%
  count(year, county, causeCode = causeCode_primary, name = "Ndeaths_primary")

otherNdeaths_list <- lapply(paste0("causeCode_", 222:240), function(x) {
  
  dat %>%
    mutate(rowN = row_number()) %>%
    rename(causeCode = !!as.symbol(x)) %>%
    select(rowN, year, county, causeCode) %>% 
    filter(!is.na(causeCode)) %>% 
    mutate(causeCode = as.character(causeCode))
  
})

otherNdeaths <- otherNdeaths_list %>%
  bind_rows() %>%
  distinct(rowN, year, county, causeCode) %>%
  count(year, county, causeCode, name = "Ndeaths_other")


primary_other_Ndeaths <- full_join(primaryNdeaths, otherNdeaths) %>%
  replace_na(list(Ndeaths_primary = 0, Ndeaths_other = 0))

# Part 2 - For each secondary cause of death, what were the primary causes? ---------------------------

causeCodes <- deathCauseLink %>% 
  filter(nchar(causeCode) == 3) %>% 
  pull(causeCode)

primaryNdeaths_perCause <- lapply(causeCodes, function(x) {
  
  tDat <- dat %>% 
    filter(if_any(starts_with("causeCode_2"), ~. == x))
  
  if (nrow(tDat) == 0) {
    return()
  } else {
    tDat %>% 
      count(year, county, causeCode = causeCode_primary, name = "Ndeaths_primary") %>% 
      mutate(causeCode_other = x) %>% 
      group_by(year, county, causeCode_other) %>% 
      nest(dataPrimary = c(causeCode, Ndeaths_primary)) %>% 
      rename(causeCode = causeCode_other)
  } 
  
  
  
}) %>% 
  bind_rows()



# Part 3 - For each primary cause of death, what were the secondary causes? ---------------------------

createDF_otherNdeaths <- function(myData) {
  
  tDat <- myData %>%
    select_if(~!all(is.na(.))) # Don't select column if all rows are NA
  
  if (ncol(tDat) == 0) { # If there are no columns, then return NULL, which indicates there were no secondary causes for group
    return(NULL)
  } else {
    tDat %>%
      filter_all(any_vars(!is.na(.))) %>% # Removes rows with all NA entries.
      mutate(rowN = row_number()) %>%
      pivot_longer(-rowN, names_to = "type", values_to = "causeCode") %>%
      filter(!is.na(causeCode)) %>%
      distinct(rowN, causeCode) %>%
      count(causeCode, name = "Ndeaths_other") %>%
      arrange(desc(Ndeaths_other))
  }
  
}

startTime <- Sys.time()
otherNdeaths_perCause <- dat %>%
  group_by(year, county, causeCode_primary) %>%
  nest(data = starts_with("causeCode_2")) %>%
  mutate(data = map(data, ~ createDF_otherNdeaths(.x)))
endTime <- Sys.time()
endTime - startTime
# Took 31 minutes to run!!!!


save(primary_other_Ndeaths, primaryNdeaths_perCause, otherNdeaths_perCause, file = paste0(securePlace, "myData/intermediateMCOD_Objects.RData")) # Save just in case.....


# Checks -------------------------------------------------------------
nrow(primary_other_Ndeaths) - nrow(otherNdeaths_perCause)

junk <- primary_other_Ndeaths %>%
  full_join(mutate(select(otherNdeaths_perCause, year, county, causeCode = causeCode_primary), junk = "x")) %>% 
  full_join(mutate(select(primaryNdeaths_perCause, year, county, causeCode), junk1 = "x")) 

colSums(is.na(junk))

# There are 2,300 more groups (year x county x sex x causeCode) in primary_other_Ndeaths than otherNdeaths_perCause. These groups have have no primary cause of death but at least one secondary cause
# All groups in primary_other_Ndeaths are in otherNdeaths_perCause

final <- primary_other_Ndeaths %>%
  full_join(rename(otherNdeaths_perCause, causeCode = causeCode_primary))  %>% # For rows not in otherNdeaths_perCause, it automatically sets the values in the data column (that contains data frames) to NULL
  full_join(primaryNdeaths_perCause) %>% 
  arrange(year, county, causeCode)

final %>% 
  filter(Ndeaths_primary == 0) %>% 
  View()

final %>% 
  filter(Ndeaths_other == 0) %>% 
  View()


saveRDS(final, paste0(ccbData, "real/datCounty_MCOD.RDS"))
