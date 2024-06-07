## Life table for calculating life expectancy based on Public Health England's methodology 
# https://www.google.com/url?sa=t&source=web&rct=j&opi=89978449&url=https://fingertips.phe.org.uk/documents/PHE%2520Life%2520Expectancy%2520Calculator.xlsm&ved=2ahUKEwjursLp4bOGAxVaAjQIHSpGCf4QFnoECBMQAQ&usg=AOvVaw0KPxc900f_zCB23xR2YrPo

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


 
## Function to create life table based on PHE methodology
lt_phe <- function(df, lAge, uAge, a0 = 0.1, ageGroup, Ndeaths, population, confLevel = 0.95) {

  first_ageGroup <- head(df, n = 1) %>% pull(!!as.symbol(ageGroup))
  last_ageGroup <- tail(df, n = 1) %>% pull(!!as.symbol(ageGroup))
  
  df <- df %>% 
    mutate(M = !!as.symbol(Ndeaths)/!!as.symbol(population), # Death Rate in Interval
           a = ifelse(!!as.symbol(ageGroup) == first_ageGroup, a0, 0.5), # Fraction of Last Age Interval Survived 
           n = ifelse(!!as.symbol(ageGroup) == last_ageGroup, 2/M, !!as.symbol(uAge) - !!as.symbol(lAge) + 1), # Interval Width
           q = ifelse(!!as.symbol(ageGroup) == last_ageGroup, 1, n*M/(1 + (1 - a)*n*M)), # Probability of Dying in Interval
           l = cumprod(ifelse(!!as.symbol(ageGroup) == first_ageGroup, 100000, 1 - lag(q))), # Number Alive at Start of Interval
           d = ifelse(!!as.symbol(ageGroup) == last_ageGroup, l, l - lead(l)), # Number Dying in Interval
           L = ifelse(!!as.symbol(ageGroup) == last_ageGroup, l/M, n*(lead(l) + a*d)), # Person-Years Lived in Interval
           T = rev(cumsum(rev(L))), # Person-Years Lived Beyond Start of Interval
           e = T/l, # Observed Life Expectancy at Start of Interval
           
           # Variances & Confidence Intervals
           Vp = ifelse(!!as.symbol(ageGroup) == last_ageGroup, 4/(!!as.symbol(Ndeaths)*M^2), q^2*(1-q)/!!as.symbol(Ndeaths)), # Sample Variance of Proportion Surviving in Interval
           Vp_wgt = ifelse(!!as.symbol(ageGroup) == last_ageGroup, (l/2)^2*Vp, l^2*((1 - a)*n + lead(e))^2*Vp), # Weighted Sample Variance of Proportion Surviving in Interval
           Vt = rev(cumsum(rev(Vp_wgt))), # Sample Variance of Person-Years Lived Beyond Start of Interval
           Ve = Vt/l^2, # Sample Variance of Observed Life Expectancy at Start of Interval
           e_lCI = e - qnorm(confLevel + (1 - confLevel)/2)*sqrt(Ve), # Lower Bound of 95% Confidence Interval for Observed Life Expectancy
           e_uCI = e + qnorm(confLevel + (1 - confLevel)/2)*sqrt(Ve) # Upper Bound of 95% Confidence Interval for Observed Life Expectancy
    ) %>% 
    relocate(e, .before = e_lCI)
}


# Test with CCB 2022 data
ageLink <- read_xlsx(paste0(standardsPlace, "ageLink.xlsx"), sheet = "standard")

CCB_LEpre <- read_csv(file = paste0(myDir, "ccbLE.csv")) %>% 
  mutate(ageGroup = ifelse(ageGroup == "14-May", "5 - 14", ageGroup)) %>% 
  left_join(ageLink %>% select(-ageCode), by = c("ageGroup" = "ageName")) %>% 
  mutate(ageGroup = factor(ageGroup, levels = ageLink$ageName)) %>% 
  arrange(ageGroup) %>% 
  select(-c(causeNameShort))

CCB_LE <- lt_phe(df = CCB_LEpre, lAge = "lAge", uAge = "uAge", a0 = 0.25, ageGroup = "ageGroup", Ndeaths = "Ndeaths", population = "population")


# Test with data from PHE's template
PHE_LEpre <- read_csv(file = paste0(myDir, "phe_test.csv")) %>% 
  mutate(ageGroup = str_replace(ageGroup, "\x96", "-"))

PHE_LE <- lt_phe(df = PHE_LEpre, lAge = "lAge", uAge = "uAge", a0 = 0.1, ageGroup = "ageGroup", Ndeaths = "Ndeaths", population = "population")


# Test using PHEindicatormethods package with PHE's template
PHE_Pkg <- phe_life_expectancy(data = PHE_LEpre, deaths = Ndeaths, population = population, startage = ageGroup, age_contents = PHE_LEpre$ageGroup)


# Compare results between lt_phe and phe_life_expectancy
PHE_diff <- left_join(PHE_LE %>% select(c(ageGroup, e, e_lCI, e_uCI)), PHE_Pkg %>% select(c(ageGroup, value:uppercl)), by = "ageGroup") %>% 
  mutate(e_diff = e - value, 
         lower_diff = e_lCI - lowercl, 
         upper_diff = e_uCI - uppercl)


