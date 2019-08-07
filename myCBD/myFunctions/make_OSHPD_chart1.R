#OSHPD Chart 1 = ggplot version




# #---------------------------------------Plotly subplots option----------------------------------------------------#
oshpdPlot1 <- function(myCounty = "CALIFORNIA", myOSHPDtype = "Number of Hospitalizations", mySex = "Total", myN = 10, myVar = "icd10_cm" ) {

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
    mutate(nameOnly = forcats::fct_reorder(names, filter(., type == myOSHPDtype) %>%
                                             pull(measure)))
#Creating ggplot facet grid plot
  if(1==2){
ggplot(plotData, aes(x = names, y = measure)) +
    coord_flip() +
    geom_bar(stat = "identity", fill = "blue") +
    facet_grid(. ~ type, scales = "free_x", labeller=labeller(type = label_wrap_gen(5))) +
    theme_bw() + #need to specify theme first, before changing/removing axis titles/labels etc. If theme_bw() is put at end, it negates all of these changes
    scale_y_continuous(labels = scales::comma) + #numbers shown with commas rather than scientific notation
    scale_x_discrete(labels = scales::wrap_format(18)) + #x-axis is condition label--wrapping text so it stacks on top of each other
    #within theme--x and y axis refer to the way it looks, with coord_flip(), so y refers to vertical label (which technically is really x axis) and vice versa with x axis
    theme(axis.title.y = element_blank(), #removes nameOnly label
    axis.title.x = element_blank(), #removes measure label
    axis.text.y = element_text(size = 10), #increases size of disease condition labels
    axis.text.x = element_text(size = 10, face="bold",angle = 90, hjust = 1), #controls size of measure labels
    strip.text.x = element_text(size = 10)) #increases the size of the facet labels
#strip.text.x = element_blank()) to remove facet labels--may be useful if converting to ggplotly and wanting to add annotations? 
}
#Lollipop plot as alternative to bar plot--will probably save space, and may be able to add value labels inside? 

ggplot(plotData, aes(x = names, y = measure, label = paste0(comma(round(measure, 0))))) +
    geom_segment(aes(x = names, xend = names, y = 0, yend = measure)) +
    geom_point(size = 1) + 
    geom_text(nudge_x = 0.5, size = 2) +
    coord_flip() +
    facet_wrap(. ~ type, scales = "free_x", labeller=labeller(type = label_wrap_gen(20))) +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank())


 
  
}


