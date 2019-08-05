
#OSHPD Chart 2 = plotly interactive version

# #---------------------------------------Plotly subplots option----------------------------------------------------#
oshpdPlot2<- function(myCounty = "CALIFORNIA", myOSHPDtype = "Number of Hospitalizations", mySex = "Total", myN = 10) {
  
  

#Ordering dataset, converting "type" values from short names to full names
calculated_metrics <- calculated_metrics %>% mutate(type = factor(type, levels = c("n_hosp", "cHospRate", "ahospRate","avg_los", "charges", "cChargeRate", "avgcharge", "avgcharge_per_day", "medcharge", "medcharge_per_day"))) %>%
  mutate(type = plyr::revalue(type, hospMeasures_Revalue)) %>% #replaces values with full name labels
  left_join(., fullCauseList, by = c("CAUSE" = "LABEL"))
  
  
#Selecting specified county, sex, level, selecting the top N rows for the given myOSHPDtype
  myOSHPDtype_N_cause <- calculated_metrics %>%
    filter(!is.na(CAUSE), Level == "lev2", county == myCounty, sex == mySex) %>%
    group_by(type) %>% arrange(desc(measure)) %>% dplyr::slice(1:myN) %>% #this selects the top N rows for myOSHPDtype
    filter(type == myOSHPDtype) %>% ungroup() %>% pull(CAUSE) 
  
  
  #creates dataframe with data only for CAUSEs from myOSHPDtype_N_cause, i.e. the top N CAUSES for the specified myOSHPDtype
  plotData <- calculated_metrics %>%
    filter(!is.na(CAUSE), Level == "lev2", county == myCounty, !(type %in% c("Crude Hospitalization Rate","Crude Charge Rate"))) %>% filter(., CAUSE %in% myOSHPDtype_N_cause, sex == mySex) %>%
    group_by(type) %>%
    mutate(nameOnly = forcats::fct_reorder(nameOnly, filter(., type == myOSHPDtype) %>%
                                             pull(measure)))

  #Creating a plotly plot
  #This code changes the formatting/order of the hovertext in the plotly plot (y specifies nameOnly, x is the $ amount): hovertemplate = paste("<b>%{y}:</b><br>","%{x: $}<br>","<extra></extra>")
  
  if(1==2){
  num_hosp <- plotData %>% filter(type == "Number of Hospitalizations") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, type = "bar", name = "Number of Hospitalizations",
                                                                                            text = "Number of Hospitalizations for",
                                                                                            hovertemplate = paste(
                                                                                              "<b>%{text}<br>",
                                                                                              "<b>%{y}:</b><br>",
                                                                                              "%{x}<br>",
                                                                                              "<extra></extra>"
                                                                                            ))
  
  #need to round numbers
  aahosp <- plotData %>% filter(type == "Age-Adjusted Hospitalization Rate") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, orientation = "h", type = "bar", name = "Age-Adjusted Hospitalization Rate",
                                                                                                 text = "Age-Adjusted Hospitalization Rate for",
                                                                                                 hovertemplate = paste(
                                                                                                   "<b>%{text}<br>",
                                                                                                   "<b>%{y}:</b><br>",
                                                                                                   "%{x}<br>",
                                                                                                   "<extra></extra>"
                                                                                                 ))
  
  charges <- plotData %>% filter(type == "Total Charges") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, orientation = "h", type = "bar", name = "Total Charges",
                                                                              text = "Total Charges for",
                                                                              hovertemplate = paste(
                                                                                "<b>%{text}<br>",
                                                                                "<b>%{y}:</b><br>",
                                                                                "%{x}<br>",
                                                                                "<extra></extra>"
                                                                              ))
  #need to round numbers
  avgcharges <- plotData %>% filter(type == "Average Charges") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, orientation = "h", type = "bar", name = "Average Charges",
                                                                                   text = "Average Charges for",
                                                                                   hovertemplate = paste(
                                                                                     "<b>%{text}<br>",
                                                                                     "<b>%{y}:</b><br>",
                                                                                     "%{x}<br>",
                                                                                     "<extra></extra>"
                                                                                   ))
  #need to round numbers
  avg_los <- plotData %>% filter(type == "Average Length of Stay (Days)") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, orientation = "h", type = "bar", name = "Average Length of Stay (Days)",
                                                                                              text = "Average Length of Stay (Days) for",
                                                                                              hovertemplate = paste(
                                                                                                "<b>%{text}<br>",
                                                                                                "<b>%{y}:</b><br>",
                                                                                                "%{x}<br>",
                                                                                                "<extra></extra>"
                                                                                              ))
  
  #need to round numbers
  avgcharge_per_day <- plotData %>% filter(type == "Average Charges per Day") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, orientation = "h", type = "bar", name = "Average Charges Per Day",
                                                                                                  text = "Average Charges Per Day for",
                                                                                                  hovertemplate = paste(
                                                                                                    "<b>%{text}<br>",
                                                                                                    "<b>%{y}:</b><br>",
                                                                                                    "%{x}<br>",
                                                                                                    "<extra></extra>"
                                                                                                  ))
  
  medcharge <- plotData %>% filter(type == "Median Charges") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, orientation = "h", type = "bar", name = "Median Charges",
                                                                                 text = "Median Charges for",
                                                                                 hovertemplate = paste(
                                                                                   "<b>%{text}<br>",
                                                                                   "<b>%{y}:</b><br>",
                                                                                   "%{x}<br>",
                                                                                   "<extra></extra>"
                                                                                 ))
  
  medcharge_per_day <- plotData %>% filter(type == "Median Charges per Day") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, orientation = "h", type = "bar", name = "Median Charges per Day",
                                                                                                 text = "Median Charges per Day for",
                                                                                                 hovertemplate = paste(
                                                                                                   "<b>%{text}<br>",
                                                                                                   "<b>%{y}:</b><br>",
                                                                                                   "%{x}<br>",
                                                                                                   "<extra></extra>"
                                                                                                 ))
  
  subplot(num_hosp, aahosp, avg_los, charges, avgcharges, avgcharge_per_day, medcharge, medcharge_per_day, shareY = TRUE) %>% 
    layout(autosize = T, yaxis = list(title = ""), showlegend = FALSE) 
  #showlegend = FALSE removes legend, which creates more room on right side of plot

  
}

  #Option 2 for creating plotly plot--subplot of ggplot2 facetgrid
  gg1 <- filter(plotData, type %in% c("Number of Hospitalizations","Age-Adjusted Hospitalization Rate", "Total Charges",
                                          "Average Charges", "Average Length of Stay (Days)")) %>%
                                          ggplot(., aes(x = nameOnly, y = measure)) +
    coord_flip() +
    geom_bar(stat = "identity", fill = "blue") +
    facet_grid(. ~ type, scales = "free_x", labeller=labeller(type = label_wrap_gen(5))) +
    theme_bw() + #need to specify theme first, before changing/removing axis titles/labels etc. If theme_bw() is put at end, it negates all of these changes
    scale_y_continuous(labels = scales::comma) + #numbers shown with commas rather than scientific notation
    scale_x_discrete(labels = scales::wrap_format(10)) + #x-axis is condition label--wrapping text so it stacks on top of each other
    #within theme--x and y axis refer to the way it looks, with coord_flip(), so y refers to vertical label (which technically is really x axis) and vice versa with x axis
    theme(axis.title.y = element_blank(), #removes nameOnly label
          axis.title.x = element_blank(), #removes measure label
          axis.text.y = element_text(size = 8), #increases size of disease condition labels
          axis.text.x = element_text(size = 8, face="bold",angle = 90, hjust = 1), #controls size of measure labels
          strip.text.x = element_text(size = 8)) #increases the size of the facet labels
  
  gg2 <- filter(plotData, type %in% c("Average Charges per Day", "Median Charges", "Median Charges per Day")) %>%
    ggplot(., aes(x = nameOnly, y = measure)) +
    coord_flip() +
    geom_bar(stat = "identity", fill = "blue") +
    facet_grid(. ~ type, scales = "free_x", labeller=labeller(type = label_wrap_gen(5))) +
    theme_bw() + #need to specify theme first, before changing/removing axis titles/labels etc. If theme_bw() is put at end, it negates all of these changes
    scale_y_continuous(labels = scales::comma) + #numbers shown with commas rather than scientific notation
    scale_x_discrete(labels = scales::wrap_format(10)) + #x-axis is condition label--wrapping text so it stacks on top of each other
    #within theme--x and y axis refer to the way it looks, with coord_flip(), so y refers to vertical label (which technically is really x axis) and vice versa with x axis
    theme(axis.title.y = element_blank(), #removes nameOnly label
          axis.title.x = element_blank(), #removes measure label
          axis.text.y = element_text(size = 8), #increases size of disease condition labels
          axis.text.x = element_text(size = 8, face="bold",angle = 90, hjust = 1), #controls size of measure labels
          strip.text.x = element_text(size = 8)) #increases the size of the facet labels
  
  
  
  subplot(gg1, gg2, shareY = TRUE, nrows = 1) %>% 
   layout(autosize = T, yaxis = list(title = ""), showlegend = FALSE) 
  
  
  
}



