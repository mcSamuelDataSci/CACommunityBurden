
#OSHPD Chart 2 = plotly interactive version

# #---------------------------------------Plotly subplots option----------------------------------------------------#
oshpdPlot2 <- function(myCounty = "CALIFORNIA", myOSHPDtype = "Number of Hospitalizations", mySex = "Total", myN = 10) {
  
  

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
  
  #specifying annotation text font
  f <- list(
    size = 10,
    color = "black",
    face = "bold")
  

  num_hosp <- plotData %>% filter(type == "Number of Hospitalizations") %>% 
    plotly::plot_ly(., y = ~nameOnly, x = ~measure, type = "bar", name = "Number of Hospitalizations",
                    text = "Number of Hospitalizations for", hovertemplate = paste("<b>%{text}<br>",
                                                                                   "<b>%{y}:</b><br>",
                                                                                   "%{x}<br>",
                                                                                   "<extra></extra>")) %>% 
    
    layout(annotations = list(text = "Number of Hospitalizations", font = f, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center",
                              x = 0.5, y = 1,showarrow = FALSE)) 
  #need to round numbers
  aahosp <- plotData %>% filter(type == "Age-Adjusted Hospitalization Rate") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, orientation = "h", type = "bar", name = "Age-Adjusted Hospitalization Rate", colors = "blue",
                                                                                                 text = "Age-Adjusted Hospitalization Rate for",
                                                                                                 hovertemplate = paste(
                                                                                                   "<b>%{text}<br>",
                                                                                                   "<b>%{y}:</b><br>",
                                                                                                   "%{x}<br>",
                                                                                                   "<extra></extra>"
                                                                                                 )) %>%
    layout(annotations = list(text = "Age-Adjusted Hospitalization Rate", font = f, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center",
                              x = 0.5, y = 1,showarrow = FALSE)) 
  
  charges <- plotData %>% filter(type == "Total Charges") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, orientation = "h", type = "bar", name = "Total Charges",
                                                                              text = "Total Charges for",
                                                                              hovertemplate = paste(
                                                                                "<b>%{text}<br>",
                                                                                "<b>%{y}:</b><br>",
                                                                                "%{x}<br>",
                                                                                "<extra></extra>"
                                                                              )) %>%
    layout(annotations = list(text = "Total Charges", font = f, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center",
                              x = 0.5, y = 1,showarrow = FALSE))
  #need to round numbers
  avgcharges <- plotData %>% filter(type == "Average Charges") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, orientation = "h", type = "bar", name = "Average Charges",
                                                                                   text = "Average Charges for",
                                                                                   hovertemplate = paste(
                                                                                     "<b>%{text}<br>",
                                                                                     "<b>%{y}:</b><br>",
                                                                                     "%{x}<br>",
                                                                                     "<extra></extra>"
                                                                                   )) %>%
    layout(annotations = list(text = "Average Charges", font = f, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center",
                              x = 0.5, y = 1,showarrow = FALSE))
  #need to round numbers
  avg_los <- plotData %>% filter(type == "Average Length of Stay (Days)") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, orientation = "h", type = "bar", name = "Average Length of Stay (Days)",
                                                                                              text = "Average Length of Stay (Days) for",
                                                                                              hovertemplate = paste(
                                                                                                "<b>%{text}<br>",
                                                                                                "<b>%{y}:</b><br>",
                                                                                                "%{x}<br>",
                                                                                                "<extra></extra>"
                                                                                              )) %>%
    layout(annotations = list(text = "Average Length of Stay (Days)", font = f, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center",
                              x = 0.5, y = 1,showarrow = FALSE))
  
  #need to round numbers
  avgcharge_per_day <- plotData %>% filter(type == "Average Charges per Day") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, orientation = "h", type = "bar", name = "Average Charges Per Day",
                                                                                                  text = "Average Charges Per Day for",
                                                                                                  hovertemplate = paste(
                                                                                                    "<b>%{text}<br>",
                                                                                                    "<b>%{y}:</b><br>",
                                                                                                    "%{x}<br>",
                                                                                                    "<extra></extra>"
                                                                                                  )) %>%
    layout(annotations = list(text = "Average Charges per Day", font = f, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center",
                              x = 0.5, y = 1,showarrow = FALSE))
  
  medcharge <- plotData %>% filter(type == "Median Charges") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, orientation = "h", type = "bar", name = "Median Charges",
                                                                                 text = "Median Charges for",
                                                                                 hovertemplate = paste(
                                                                                   "<b>%{text}<br>",
                                                                                   "<b>%{y}:</b><br>",
                                                                                   "%{x}<br>",
                                                                                   "<extra></extra>"
                                                                                 )) %>%
    layout(annotations = list(text = "Median Charges", font = f, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center",
                              x = 0.5, y = 1,showarrow = FALSE))
  
  medcharge_per_day <- plotData %>% filter(type == "Median Charges per Day") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, orientation = "h", type = "bar", name = "Median Charges per Day",
                                                                                                 text = "Median Charges per Day for",
                                                                                                 hovertemplate = paste(
                                                                                                   "<b>%{text}<br>",
                                                                                                   "<b>%{y}:</b><br>",
                                                                                                   "%{x}<br>",
                                                                                                   "<extra></extra>"
                                                                                                 )) %>%
    layout(annotations = list(text = "Median Charges per Day", font = f, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center",
                              x = 0.5, y = 1,showarrow = FALSE))
  

  subplot1 <- subplot(num_hosp, aahosp, avg_los, shareY = TRUE) %>% 
    layout(autosize = T, yaxis = list(title = ""), showlegend = FALSE) 
    
  subplot2 <- subplot(charges, avgcharges, avgcharge_per_day, shareY = TRUE) %>% 
    layout(autosize = T, yaxis = list(title = ""), showlegend = FALSE) 
    
  subplot3 <- subplot(medcharge, medcharge_per_day, shareY = TRUE) %>% 
    layout(autosize = T, yaxis = list(title = ""), showlegend = FALSE) 
    
  subplot(subplot1, subplot2, subplot3, shareY = TRUE, nrows = 3) %>%
    layout(autosize = T, yaxis = list(title = ""), showlegend = FALSE) 
  #showlegend = FALSE removes legend, which creates more room on right side of plot
 
  
  }



