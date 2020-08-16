 junk <-  filter(oshpd.PDD.16
                 
                 
                 
                 oshpd.PDD.16  <- read_sas(paste0(secure.location,"rawOSHPD/PDD/cdph_pdd_rln2016.sas7bdat") )
                 
                 #Subset with only variables of interest
                 junk <-  select(oshpd.PDD.16,
                                         diag_p, 
                                         contains("odiag"),
                                         ccs_diagP,
                                         contains("ccs_odiag")
                                 
library(summarytools)



filter(oshpd.PDD.16, ccs_diagP == 255) %>% freq(diag_p)


filter(oshpd.PDD.16,ccs_odiag1 == 255) %>% freq(odiag1)
filter(oshpd.PDD.16,ccs_odiag2 == 255) %>% freq(odiag2)

                 
                 
Z590   2647 
Z590   2647 
Z62810    428
Z62811    220
