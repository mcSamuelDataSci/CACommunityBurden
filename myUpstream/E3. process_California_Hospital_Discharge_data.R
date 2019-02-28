myDrive <- getwd()  
myPlace <- paste0(myDrive,"/myCBD") 
upPlace <- paste0(myDrive,"/myUpstream")

library(tidyverse)
library(ggplot2)

library(haven)
# oshpdHD0  <- haven::read_sas("h:\\0.Secure.Data\\OSHPD\\PDD\\cdph_pdd_rln2016.sas7bdat") 
# oshpdHD   <- select(oshpdHD0,admtyr,patcnty,patzip,sex,agyrdsch,mdc,diag_p,charge)

library(fs)
# saveRDS(oshpdHD, file=path(upPlace,"upData/oshpdHD.rds"))

oshpdHD <- readRDS(file=path(upPlace,"upData/oshpdHD.rds"))
tWork   <- oshpdHD %>% mutate(mdc = as.numeric(mdc)) %>%
                              group_by(mdc) %>%
                              summarise(tCount = n(),
                              tBucks = sum(charge)) %>%
                              gather(tCount, tBucks, key = "count_charges", value = "number")

library(readxl)
oshpdMdcMap <- read_excel(path(myPlace,"myInfo/App_I_MDC_PDD.xlsx"), sheet="v32.0 ",skip=4)
names(oshpdMdcMap)[1] <- "mdc"
names(oshpdMdcMap)[2] <- "MDC"

tWork <- full_join(oshpdMdcMap,tWork,by="mdc")
tWork <- tWork %>% select(-mdc)
tWork <- as.data.frame((tWork))

ggplot(data = tWork, aes(x = MDC, y = number, fill = count_charges)) +  coord_flip() +
        geom_bar(stat = "identity" ) + 
        facet_grid(. ~ count_charges,scales="free_x")


