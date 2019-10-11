
#OSHPD Chart 2 = plotly interactive version

# #---------------------------------------Plotly subplots option----------------------------------------------------#
oshpdPlot2 <- function(myCounty = "CALIFORNIA", myOSHPDtype = "Number of Hospitalizations", mySex = "Total", myN = 10, myVar = "mdc") {
  
  

  full_CAUSE_mdcdrg_list <- full_CAUSE_mdcdrg_list %>% mutate(names = as.character(names)) #names was saved as a factor with 1 level, for some reason?
  
  #Ordering dataset, converting "type" values from short names to full names, joining with names
  full_oshpd_summary <- full_oshpd_summary %>% mutate(type = factor(type, levels = c("n_hosp", "cHospRate", "ahospRate", "avg_los", "charges", "cChargeRate", "avgcharge", "avgcharge_per_day", "medcharge", "medcharge_per_day"))) %>%
    mutate(type = plyr::revalue(type, hospMeasures_Revalue)) %>% #replaces values with full name labels
    left_join(., full_CAUSE_mdcdrg_list, by = c("CAUSE" = "LABEL")) %>%
    #filtering so that only lev2 data is used (for icd10_cm data), and all drg/mdc (since they didn't have lev1/2/0 options)
    filter(diagnosis_var == "drg" | diagnosis_var == "mdc" | Level == "lev2")
  
  
  #Selecting specified county, sex, level, selecting the top N rows for the given myOSHPDtype 
  myOSHPDtype_N_cause <- full_oshpd_summary %>%
    filter(diagnosis_var == "drg" | diagnosis_var == "mdc" | Level == "lev2") %>%
    filter(!is.na(CAUSE), county == myCounty, sex == mySex, diagnosis_var == myVar) %>%
    group_by(type) %>% arrange(desc(measure)) %>% dplyr::slice(1:myN) %>% #this selects the top N rows for myOSHPDtype
    filter(type == myOSHPDtype) %>% ungroup() %>% pull(CAUSE) 
  
  
  #creates dataframe with data only for CAUSEs from myOSHPDtype_N_cause, i.e. the top N CAUSES for the specified myOSHPDtype
  plotData <- full_oshpd_summary %>%
    filter(!is.na(CAUSE), county == myCounty, !(type %in% c("Crude Hospitalization Rate","Crude Charge Rate"))) %>% filter(., CAUSE %in% myOSHPDtype_N_cause, sex == mySex) %>%
    group_by(type) %>%
    mutate(names = forcats::fct_reorder(names, filter(., type == myOSHPDtype) %>%
                                          pull(measure)))
  #Creating a plotly plot
  #This code changes the formatting/order of the hovertext in the plotly plot (y specifies nameOnly, x is the $ amount): hovertemplate = paste("<b>%{y}:</b><br>","%{x: $}<br>","<extra></extra>")
  
  #Function for creating subplots in plotly
  
plotly_function <- function(mytype) {
  #specifying annotation text font
  f <- list(
    size = 10,
    color = "black",
    face = "bold")
  
  plotData %>% filter(type == mytype) %>% 
    plotly::plot_ly(.) %>% add_segments(y = ~names, yend = ~names, x = ~measure, xend = ~0, showlegend = FALSE, hoverinfo = "none", color = "blue") %>%
    add_markers(y = ~names, x = ~measure, hoverinfo = "text", color = "blue",
                text = ~paste(mytype, "for", names, ":", comma(round(measure)))) %>%
    layout(annotations = list(text = mytype, font = f, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center",
                              x = 0.5, y = 1,showarrow = FALSE))
}

#Num Hospitalizations
num_hosp <- plotly_function("Number of Hospitalizations")

#Age-Adjusted Hosp Rate
aahosp <- plotly_function("Age-Adjusted Hospitalization Rate")

#Avg Length of Stay
avg_los <- plotly_function("Average Length of Stay (Days)")

plotly_function_dollar <- function(mytype) {
  #specifying annotation text font
  f <- list(
    size = 10,
    color = "black",
    face = "bold")
  
  plotData %>% filter(type == mytype) %>% 
    plotly::plot_ly(.) %>% add_segments(y = ~names, yend = ~names, x = ~measure, xend = ~0, showlegend = FALSE, hoverinfo = "none", color = "blue") %>%
    add_markers(y = ~names, x = ~measure, hoverinfo = "text", color = "blue",
                text = ~paste(mytype, "for", names, ":","$",comma(round(measure)))) %>%
    layout(annotations = list(text = mytype, font = f, xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "center", align = "center",
                              x = 0.5, y = 1,showarrow = FALSE))
}
  
charges <- plotly_function_dollar("Total Charges")

avgcharges <- plotly_function_dollar("Average Charges")

avgcharge_per_day <- plotly_function_dollar("Average Charges per Day")

medcharge <- plotly_function_dollar("Median Charges")

medcharge_per_day <- plotly_function_dollar("Median Charges per Day")
  
  
if(myVar == "icd10_cm"){
  subplot1 <- subplot(num_hosp, aahosp, avg_los, shareY = TRUE) %>% 
    layout(autosize = T, yaxis = list(title = ""), showlegend = FALSE) 
    
  subplot2 <- subplot(charges, avgcharges, avgcharge_per_day, shareY = TRUE) %>% 
    layout(autosize = T, yaxis = list(title = ""), showlegend = FALSE) 
    
  subplot3 <- subplot(medcharge, medcharge_per_day, shareY = TRUE) %>% 
    layout(autosize = T, yaxis = list(title = ""), showlegend = FALSE) 
    
  subplot(subplot1, subplot2, subplot3, shareY = TRUE, nrows = 3) %>%
    layout(autosize = T, yaxis = list(title = ""), showlegend = FALSE, margin = c(l = 0.5, r = 0.5))
  #showlegend = FALSE removes legend, which creates more room on right side of plot
 
  #why does it show up as orange instead of blue? --and "blue" is listed as the legend? 
}

else if(myVar == "mdc" | myVar == "drg"){
  subplot1 <- subplot(num_hosp, charges, shareY = TRUE) %>%
    layout(autosize = T, yaxis = list(title = ""), showlegend = FALSE)
  
  subplot2 <- subplot(avgcharges, medcharge, shareY = TRUE) %>%
    layout(autosize = T, yaxis = list(title = ""), showlegend = FALSE)
    
  subplot(subplot1, subplot2, shareY = TRUE, nrows = 1) %>%
    layout(autosize = T, yaxis = list(title = ""), showlegend = FALSE, margin = c(l = 0.5, r = 0.5))  
  #
}


  }



