# Fetch Group Quarters Population (Table B26001)

library(readxl)
library(dplyr)

myYear <- 2019

# Read in df

df <- read_xlsx("/mnt/projects/FusionData/SDOH/linkageSDOH.xlsx", sheet = "groupQuarters") %>%
  filter(`Our variable name` %in% c("gqTotal1")) %>%
  mutate(Numerator = as.character(Numerator))

df1 <- read_xlsx("/mnt/projects/FusionData/SDOH/linkageSDOH.xlsx", sheet = "AgeRaceSex") %>%
  filter(`Our variable name` == "population") %>%
  mutate(Numerator = as.character(Numerator))

df2 <- bind_rows(df1, df)

source("/mnt/projects/FusionData/SDOH/pullACS_function.R")

gq <- getACS(State=06, # Set to NULL if pulling ZCTA
             Geography="tract", # Options: county, zcta, tract.. for acs1, puma
             MSSA = F, # if you want MSSA output, set T and geograph = Tract
             Survey="acs5",
             Year=myYear,
             moeLevel=90,
             asList = T, # Want as a list, or as one data frame?
             df2)

final_df <- gq[["gqTotal1"]] %>%
  select(GEOID, NAME, Numerator, NumeratorMOE) %>%
  left_join(select(gq[["population"]], GEOID, Denominator, DenominatorMOE), by = "GEOID") %>%
  mutate(estimate = Numerator/Denominator, 
         moe=moe_ratio(Numerator,Denominator,NumeratorMOE,DenominatorMOE))



saveRDS(final_df, paste0("/mnt/projects/FusionData/0.CCB/myUpstream/upData/popGroupQuarter_Tract", myYear, ".RDS"))

