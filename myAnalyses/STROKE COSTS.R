library(janitor)


temp <- oshpd_PDD %>% filter(sex=="Total",county=="CALIFORNIA")  %>%
                      filter(ccsCode %in% c("oo111","oo112","oo109")) %>%
                      select(type,measure,ccsCode) %>%
                      filter(type %in% c("n_hosp","charges","avgcharge")) %>%
                      left_join(ccsLinker()) %>% select(-ccsCode,-birth)  %>%
                      pivot_wider(names_from = type, values_from = measure ) %>%
                      adorn_totals("row")


write_csv(temp,"strokeExplore.csv")
