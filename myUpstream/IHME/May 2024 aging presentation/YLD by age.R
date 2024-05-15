server <- T
if (!server) source("g:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
if (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")



ihmeDat0 <- read_csv(paste0(ccbUpstream, "IHME/May 2024 aging presentation/IHME-GBD_2019_DATA-04270b10-1.csv"))

ihmeDat  <- ihmeDat0 %>%
             mutate(ageG = ifelse(age %in% c("55-59 years", "60-64 years"), "55-64", substr(age,1,5))) %>%
             filter(metric == "Number") %>%
             group_by(cause, ageG) %>%
             summarise(YLD = sum(val)) %>% ungroup()
