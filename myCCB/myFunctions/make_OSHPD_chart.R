#OSHPD Chart 1 = ggplot version

oshpdPlot1 <- function(myCounty = "CALIFORNIA", myOSHPDtype = "Number of Hospitalizations", mySex = "Total", myN = 10, myLiveborn = FALSE) {
  
  if (myCounty %in% cityLHJs) {
    stop(cityMessage)
  }

  
if (1==2) {
  myCounty = "CALIFORNIA"
  myOSHPDtype = "Number of Hospitalizations"
  mySex = "Total"
  myN = 10
}
  

#Ordering dataset, converting "type" values from short names to full names, joining with names
  oshpd_PD_work <- oshpd_PDD_primary %>% 
                      mutate(type = factor(type, levels = c("n_hosp", 
                                                            "cHospRate", 
                                                            "ahospRate", 
                                                            "avg_los", 
                                                            "charges", 
                                                            "cChargeRate", 
                                                            "avgcharge", 
                                                            "avgcharge_per_day", 
                                                            "medcharge", 
                                                            "medcharge_per_day"))) %>%
                          mutate(type = plyr::revalue(type, hospMeasures_Revalue)) %>%   
                          left_join(hospCauseLink, by =  c("causeCode") )  
    
  
 
# Selecting specified county, sex, level, selecting the top N rows for the given myOSHPDtype 
    myOSHPDtype_N_cause <- oshpd_PD_work %>%
    #filter(diagnosis_var == "drg" | diagnosis_var == "mdc" | Level == "lev2")  %>%
    filter(!is.na(causeCode), county == myCounty, sex == mySex) %>%
    filter(type == myOSHPDtype)     %>%
    group_by(type) %>% 
    arrange(desc(measure)) %>% dplyr::slice(1:myN)   %>% #this selects the top N rows for myOSHPDtype
    filter(type == myOSHPDtype) %>% ungroup() %>% pull(causeCode) 
  
    
    if(!myLiveborn) oshpd_PD_work <- filter(oshpd_PD_work, causeName != "Liveborn")
    
   
    
    

# creates dataframe with data only for CAUSEs from myOSHPDtype_N_cause, i.e. the top N CAUSES for the specified myOSHPDtype
  plotData <- oshpd_PD_work %>%
    filter(!is.na(causeCode), county == myCounty, (type %in% c("Number of Hospitalizations","Average Length of Stay (Days)", "Total Charges", "Median Charges"))) %>% filter(causeCode %in% myOSHPDtype_N_cause, sex == mySex) %>%
    group_by(type)   %>%
    mutate(names = forcats::fct_reorder(causeName, filter(.,type == myOSHPDtype)  %>%
                                             pull(measure)))

  
  
  
  
  
# Creating ggplot facet grid plot
myPlot <- ggplot(plotData, aes(x = names, y = measure)) + coord_flip() +
     geom_bar(stat = "identity", fill = "blue") +
     labs(title=paste("Rankings of Causes of Hospitalizaions, Lengths of Stay, and Associated Charges, California, ",currentYear_hosp_ed)) +
     facet_grid(. ~ type, scales = "free_x", labeller=labeller(type = label_wrap_gen(5))) +
       #theme_bw( ) + #need to specify theme first, before changing/removing axis titles/labels etc. If theme_bw() is put at end, it negates all of these changes   # base_size = 25
     scale_y_continuous(labels = comma) + #numbers shown with commas rather than scientific notation
     scale_x_discrete(labels = scales::wrap_format(50)) + #x-axis is condition label--wrapping text so it stacks on top of each other
     theme(  plot.title.position = "plot",
             plot.title = element_text( margin=margin(0,0,30,0)),
             axis.title.y = element_blank(), #removes nameOnly label
     axis.title.x = element_blank(), #removes measure label
     axis.text.y = element_text(size = myAxisSize), #increases size of disease condition labels
     axis.text.x = element_text(size = myAxisSize, face="bold",angle = 90, hjust = 1), #controls size of measure labels
     strip.text.x = element_text(size = myAxisSize)) #increases the size of the facet labels

list(plotL = myPlot, dataL = plotData)

}
