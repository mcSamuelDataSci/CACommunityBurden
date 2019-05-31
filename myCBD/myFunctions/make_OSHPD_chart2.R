
#OSHPD Chart 2 = plotly interactive version

# #---------------------------------------Plotly subplots option----------------------------------------------------#
oshpdPlot2<- function(myCounty = "CALIFORNIA", myOSHPDtype = "Number of Hospitalizations", mySex = "Total", myN = 10) {
  
  

#Ordering dataset, converting "type" values from short names to full names
calculated_metrics <- calculated_metrics %>% mutate(type = factor(type, levels = c("n_hosp", "cHospRate", "ahospRate","avg_los", "charges", "cChargeRate", "avgcharge", "avgcharge_per_day"))) %>%
  mutate(type = plyr::revalue(type, hospDiscMeasures)) %>% #replaces values with full name labels
  left_join(., fullCauseList, by = c("CAUSE" = "LABEL"))
  
  
#Selecting specified county, sex, level, selecting the top N rows for the given myOSHPDtype
  myOSHPDtype_N_cause <- calculated_metrics %>%
    filter(!is.na(CAUSE), Level == "lev2", county == myCounty, sex == mySex) %>%
    group_by(type) %>% arrange(desc(measure)) %>% dplyr::slice(1:myN) %>% #this selects the top N rows for myOSHPDtype
    filter(type == myOSHPDtype) %>% ungroup() %>% pull(CAUSE) 
  
  
  #creates dataframe with data only for CAUSEs from myOSHPDtype_N_cause, i.e. the top N CAUSES for the specified myOSHPDtype
  plotData <- calculated_metrics %>%
    filter(!is.na(CAUSE), Level == "lev2", county == myCounty, !(type %in% c("Crude Hosp Rate","Crude Charge Rate"))) %>% filter(., CAUSE %in% myOSHPDtype_N_cause, sex == mySex) %>%
    group_by(type) %>%
    mutate(nameOnly = forcats::fct_reorder(nameOnly, filter(., type == myOSHPDtype) %>%
                                             pull(measure)))

  #Creating a plotly plot
  
  num_hosp <- plotData %>% filter(type == "Number of Hospitalizations") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, type = "bar", name = "Number of Hospitalizations")
  
  aahosp <- plotData %>% filter(type == "Age-Adjusted Hospitalization Rate") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, orientation = "h", type = "bar", name = "Age-Adjusted Hospitalization Rate")
  
  charges <- plotData %>% filter(type == "Total Charges") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, orientation = "h", type = "bar", name = "Total Charges")
  
  avgcharges <- plotData %>% filter(type == "Average Charges") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, orientation = "h", type = "bar", name = "Average Charges")
  
  avg_los <- plotData %>% filter(type == "Average Length of Stay (Days)") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, orientation = "h", type = "bar", name = "Average Length of Stay (Days)")
  
  avgcharge_per_day <- plotData %>% filter(type == "Average Charges Per Day") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, orientation = "h", type = "bar", name = "Average Charges Per Day")
  

  
  subplot(num_hosp, aahosp, avg_los, charges, avgcharges, avgcharge_per_day, shareY = TRUE) %>% layout(yaxis = list(title = ""))

  
}




