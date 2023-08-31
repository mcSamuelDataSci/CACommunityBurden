
ccsMap <- read_excel(paste0(ccbUpstream,"upstreamInfo/Fusion-ccs_dx_icd10cm_2019_1-BETA.xlsx"))  %>%
                       select(icd10    = "'ICD-10-CM CODE'",
                              ccscode  = "'CCS CATEGORY'") %>%
                       mutate(icd10    = str_remove_all(icd10,"'"),
                              ccscode  = str_remove_all(ccscode,"'")
                            )

## note that we have a little "hack" in the file above such that "U071" (COVID-19) maps to ccscode "3000"


library(haven)  
  
  pdd.2021        <- read_sas(paste0(securePlace,"rawOSHPD/PDD/pdd_2021.sas7bdat") )
  
  pdd.2021.work   <- pdd.2021  %>% 
                      rename(icd10 = diag_p) %>%
                      left_join(ccsMap, by="icd10") %>% 
                      rename(ccs_diagP = ccscode , diag_p = icd10)

    
  # Map ICD-10-CM to css in odiag1 - odiag24 
  for (i in 1:24) {
    column_name     <- paste0("odiag", i)
    ccs_column_name <- paste0("ccs_odiag", i)
    
    pdd.2021.work <- pdd.2021.work %>%
      mutate(icd10 = !!sym(column_name)) %>%
      left_join(ccsMap, by="icd10") %>% 
      mutate(ccscode = ifelse(icd10 == "U071", 3000, ccscode)) %>%
      rename(!!sym(ccs_column_name) := ccscode) %>%
      select(-icd10)
  }
  
  
  
  pdd.2021.work <- pdd.2021.work %>% select(-(odiag1:odiag24))    
  
  
