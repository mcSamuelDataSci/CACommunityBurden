
library(readxl)
library(readr)
library(dplyr)
library(tidyr)



raceLink   <- read_excel("raceLink.xlsx")
regionLink <- read_excel("countyLink.xlsx") %>% mutate(fips = paste0("6",FIPSCounty))


dofDat0 <- read_csv("P3_2020-2060.csv") %>% mutate(fips = as.character(fips)) %>%
            filter(year %in% 2020:2023) %>%
            left_join(regionLink, by = "fips") %>%
            left_join(raceLink, by = "race7")  %>%
            group_by(year, RPHO_separate_LA, raceName) %>%
            summarise(popN = sum(perwt))  %>%
            pivot_wider(names_from = year, values_from = popN)

write_csv(dofDat0,"CA RPHO Pop R-E 2020-2023.csv")

