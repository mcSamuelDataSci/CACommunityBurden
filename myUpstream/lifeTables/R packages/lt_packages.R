## Test different R packages for life table calculations
## Packages include demography, actLifer, and PHEindicatormethods

server <- TRUE
if (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
if (!server) source("G:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")

myRootDir <- ifelse(server, "/mnt/projects/FusionData/", "G:/FusionData/")
myDir <- paste0(myRootDir, "0.CCB/myUpstream/lifeTables/R packages/")

setwd(myDir)

library(dplyr)
library(readxl)
library(readr)
library(PHEindicatormethods)
library(actLifer)
library(demography)


#============ demography package ============
# Works for 1-year or 5-year age interval
## Human Mortality Database (HMD) 5-year age group
## Death data
hmd_deathRate5 <- read.delim("Mx_5x5.txt", sep = "")

hmd_deathRate5_2017 <- hmd_deathRate5 %>% 
  filter(Year == "2015-2019") %>% 
  mutate(startage = str_split_i(Age, "-", i = 1)) %>% 
  relocate(startage, .after = Year) %>% 
  select(-Age)

## Population data
hmd_pop5 <- read.delim("Population5.txt", sep ="")

hmd_pop5_2017 <- hmd_pop5 %>% 
  filter(Year == "2017") %>% 
  mutate(startage = str_split_i(Age, "-", i = 1)) %>% 
  relocate(startage, .after = Year) %>% 
  select(-Age)

write.table(hmd_deathRate5_2017, "hmd_deathRate5.txt", append = FALSE, sep = "\t", dec = ".",
            row.names = FALSE, col.names = TRUE)

write.table(hmd_pop5_2017, "hmd_pop5.txt", append = FALSE, sep = "\t", dec = ".",
            row.names = FALSE, col.names = TRUE)


demogdat5 <- read.demogdata(file = "hmd_deathRate5.txt", popfile = "hmd_pop5.txt", label = "US", type = "mortality", skip = 0, scale = 1)

demog_hmd5Pre <- demography::lifetable(demogdat5, series = "total", years = demogdat5$year, ages = demogdat5$age, 
                                   max.age = 90, type = "period")

demog_hmd5 <- data.frame(demog_hmd5Pre)


## Human Mortality Database (HMD) 1-year age group
hmd_deathRate <- read.delim("Mx_1x5.txt", sep = "")

hmd_deathRate2017 <- hmd_deathRate %>% 
  filter(Year == "2015-2019")

hmd_pop <- read.delim("Population.txt", sep ="")

hmd_pop2017 <- hmd_pop %>% 
  filter(Year == "2017")

write.table(hmd_deathRate2017, "hmd_deathRate.txt", append = FALSE, sep = "\t", dec = ".",
            row.names = FALSE, col.names = TRUE)

write.table(hmd_pop2017, "hmd_pop.txt", append = FALSE, sep = "\t", dec = ".",
            row.names = FALSE, col.names = TRUE)


demogdat <- read.demogdata(file = "hmd_deathRate.txt", popfile = "hmd_pop.txt", label = "US", type = "mortality", skip = 0, scale = 1)

demog_hmdPre <- demography::lifetable(demogdat, series = "total", years = demogdat$year, ages = demogdat$age, 
                                  max.age = 90, type = "period")

demog_hmd<- data.frame(demog_hmdPre)


#============ actLifer package ============
# Only works with unabridged (single year age interval) life table
## Human Mortality Database (HMD) 1-year age group
hmd_data <- left_join(hmd_deathRate2017 %>% select(c(Age, Total)) %>% rename(deathRate = Total), 
                      hmd_pop2017 %>% select(c(Age, Total)) %>% rename(population = Total), by = "Age")

hmd_data <- hmd_data %>% 
  mutate(deaths = round(deathRate*population, 0)) %>% 
  select(-deathRate)


al_hmd <- actLifer::lifetable(data = hmd_data, age = "Age", pop = "population", deaths = "deaths")

al_hmd <- al_hmd %>% 
  mutate(Age = as.numeric(Age))

# PHE data
PHE_LEpre <- read_csv(file = "phe_test.csv") %>% 
  mutate(ageGroup = str_replace(ageGroup, "\x96", "-"))

al_PHE <- actLifer::lifetable(data = PHE_LEpre, age = "ageGroup", pop = "population", deaths = "Ndeaths", includeAllSteps = TRUE)

# CDC WONDER data
al_wonder <- actLifer::lifetable(data = mortality2, age = "age_group", pop = "population", deaths = "deaths")


###------ actLifer vs. demography ------
# Difference between actLifer and demography for 1-year age intervals
comp_al_demog <- inner_join(al_hmd, demog_hmd %>% select(-year), by = c("Age" = "x")) %>% 
  mutate(e_diff = LifeExpectancy - ex)


#============ PHEindicatormethods package ============
# Only works with 20 age groups
## Human Mortality Database (HMD) 5-year age group
hmd_death_PHE <- hmd_deathRate5 %>% 
  filter(Year == "2015-2019")

hmd_pop_PHE <- hmd_pop5 %>% 
  filter(Year == "2017")

hmd_PHE <- left_join(hmd_death_PHE %>% rename(deathRate = Total) %>% select(c(Age, deathRate)), 
                       hmd_pop_PHE %>% rename(population = Total) %>% select(c(Age, population)), by = "Age")

hmd_PHE <- hmd_PHE %>% 
  mutate(deaths = round(deathRate*population, 0), 
         lower_age = case_when(
           Age == "0" ~ 0,
           #Age == "90+" ~ 90,
           TRUE ~ as.numeric(str_split_i(Age, "-", i = 1))
         ), 
         upper_age = case_when(
           Age == "0" ~ 0,
           TRUE ~ as.numeric(str_split_i(Age, "-", i = 2))
         ))

# combine the last 5 age groups into 90+
ageGroup90 <- c("90-94", "95-99", "100-104", "105-109", "110+")

hmd_90plusPre <- hmd_PHE %>% 
  filter(Age %in% ageGroup90) %>% 
  summarize(population = sum(population), deaths = sum(deaths))

hmd_90plus <- data.frame("90+", hmd_90plusPre$population, hmd_90plusPre$deaths, 90, 99)
names(hmd_90plus) <- c("Age", "population", "deaths", "lower_age", "upper_age")

hmd_90below <- hmd_PHE %>% 
  filter(!Age %in% ageGroup90) %>% 
  select(-deathRate)

hmd_PHE <- rbind(hmd_90below, hmd_90plus)


PHE_hmd <- phe_life_expectancy(data = hmd_PHE, deaths = deaths, population = population, startage = Age, age_contents = hmd_PHE$Age)

###------ PHEindicatormethods vs. demography ------
# Difference between PHE and demography for 5-year age intervals
comp_PHE_demog <- cbind(PHE_hmd %>% select(c(Age:uppercl)), demog_hmd5 %>% select(-year)) %>%
  mutate(e_diff = value - ex)

source("life_table_PHE.R")

PHE_hmd_SC <- lt_phe(df = hmd_PHE, lAge = "lower_age", uAge = "upper_age", a0 = 0.1, ageGroup = "Age", Ndeaths = "deaths", population = "population")

# Use Shuo's replication of PHE to compare intermediate results between PHE and demography
comp_PHE_demog_full <- cbind(PHE_hmd_SC %>% select(-c(lower_age, upper_age, Vp:Ve, e_lCI, e_uCI)), demog_hmd5 %>% select(-year)) %>% 
  mutate(e_diff = e - ex)
