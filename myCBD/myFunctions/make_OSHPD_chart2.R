
#OSHPD Chart 2 = plotly interactive version

# #---------------------------------------Plotly subplots option----------------------------------------------------#
oshpdPlot2<- function(myCounty = "CALIFORNIA", myOSHPDtype = "Number of Hospitalizations", mySex = "Total", myN = 10) {
  
  
  #OPTION 1--selects  the number of rows you specify--what this does is select the top N for 
  #each type (nhosp, avgcharges, etc), which may not be the same for each.
  if(1==2){
    plotData <-    calculated_metrics %>%
      mutate(type = factor(type, levels = c("n_hosp", "cHospRate", "ahospRate", "charges", "cChargeRate", "avgcharge"))) %>%
      mutate(type = plyr::revalue(type, hospDiscMeasures)) %>% #replaces values with full name labels
      left_join(., fullCauseList, by = c("CAUSE" = "LABEL")) %>%
      filter(!is.na(CAUSE), Level == "lev2", county == myCounty, !(type %in% c("Crude Hosp Rate","Crude Charge Rate"))) %>%
      filter(sex == mySex) %>%
      group_by(type) %>%
      mutate(nameOnly = forcats::fct_reorder(nameOnly, filter(., type == myOSHPDtype) %>%
                                               pull(measure))) %>% dplyr::arrange(desc(measure)) %>% dplyr::slice(1:myN) #slice only selects the number of rows you specify--what this does is select the top N for 
    #each type (nhosp, avgcharges, etc), which may not be the same for each. Can we sort it so that based on the ordering variable (eg n_hosp), it pulls the top N rows for n_hosp, and those variables are what are the corresponding conditions for all the other values?
  }
  
  
  
  #OPTION 2-- sorts that based on the ordering variable (eg n_hosp), it pulls the top N rows for n_hosp, and those variables are what are the corresponding conditions for all the other values
  #create a vector of CAUSE for top N of myOSHPDtype
  calculated_metrics <- calculated_metrics %>% mutate(type = factor(type, levels = c("n_hosp", "cHospRate", "ahospRate","avg_los", "charges", "cChargeRate", "avgcharge", "avgcharge_per_day"))) %>%
    mutate(type = plyr::revalue(type, hospDiscMeasures)) %>% #replaces values with full name labels
    left_join(., fullCauseList, by = c("CAUSE" = "LABEL"))
  
  
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

  num_hosp <- plotData %>% filter(type == "Number of Hospitalizations") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, type = "bar", name = "Number of Hospitalizations")
  
  aahosp <- plotData %>% filter(type == "Age-Adjusted Hospitalization Rate") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, orientation = "h", type = "bar", name = "Age-Adjusted Hospitalization Rate")
  
  charges <- plotData %>% filter(type == "Total Charges") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, orientation = "h", type = "bar", name = "Total Charges")
  
  avgcharges <- plotData %>% filter(type == "Average Charges") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, orientation = "h", type = "bar", name = "Average Charges")
  
  avg_los <- plotData %>% filter(type == "Average Length of Stay (Days)") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, orientation = "h", type = "bar", name = "Average Length of Stay (Days)")
  
  avgcharge_per_day <- plotData %>% filter(type == "Average Charges Per Day") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, orientation = "h", type = "bar", name = "Average Charges Per Day")
  

  
  subplot(num_hosp, aahosp, avg_los, charges, avgcharges, avgcharge_per_day, shareY = TRUE) %>% layout(yaxis = list(title = ""))

  
}

#####---------------------ggplotly option--------------------------------------------------------------#####


if (1 == 2) {
oshpdPlot <- function(myCounty = "CALIFORNIA", myOSHPDtype = "Number of Hospitalizations", mySex = "Total") {

  plotData <-    calculated_metrics %>%
                   mutate(type = factor(type, levels = c("n_hosp", "cHospRate", "ahospRate", "charges", "cChargeRate", "avgcharge"))) %>%
                    mutate(type = plyr::revalue(type, hospDiscMeasures)) %>% #replaces values with full name labels
                    left_join(., fullCauseList, by = c("CAUSE" = "LABEL")) %>%
                    filter(!is.na(CAUSE), Level == "lev2", county == myCounty, !(type %in% c("Crude Hosp Rate","Crude Charge Rate"))) %>%
                    filter(sex == mySex) %>%
                    group_by(type) %>%
                     mutate(nameOnly = forcats::fct_reorder(nameOnly, filter(., type == myOSHPDtype) %>%
                    pull(measure)))
 
 plot <- ggplot(plotData, aes(x = nameOnly, y = measure)) +
       coord_flip() + geom_bar(stat = "identity", fill = "blue") +
       facet_grid(. ~ type, scales = "free_x", labeller=labeller(type = label_wrap_gen(5))) +
       theme_bw() + #need to specify theme first, before changing/removing axis titles/labels etc. If theme_bw() is put at end, it negates all of these changes
       scale_y_continuous(labels = scales::comma) + #numbers shown with commas rather than scientific notation
       scale_x_discrete(labels = scales::wrap_format(10)) + #x-axis is condition label--wrapping text so it stacks on top of each other
     #within theme--x and y axis refer to the way it looks, with coord_flip(), so y refers to vertical label (which technically is really x axis) and vice versa with x axis
     theme(axis.title.y = element_blank(), #removes nameOnly label
           axis.title.x = element_blank(), #removes measure label
           axis.text.y = element_text(size = 15), #increases size of disease condition labels
           axis.text.x = element_text(size = 10, face="bold"), #controls size of measure labels
           strip.text.x = element_text(size = 10)) #increases the size of the facet labels
 
 ggplotly(plot) %>% layout(margin = list(b = 90))
 
 
}

}
