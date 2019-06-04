#OSHPD Chart 1 = ggplot version


calculated_metrics <- readRDS(file = path(myPlace, "myData/real/countyOSHPD.rds"))


# #---------------------------------------Plotly subplots option----------------------------------------------------#
oshpdPlot1 <- function(myCounty = "CALIFORNIA", myOSHPDtype = "Number of Hospitalizations", mySex = "Total", myN = 10 ) {


#Ordering dataset, converting "type" values from short names to full names
  calculated_metrics <- calculated_metrics %>% mutate(type = factor(type, levels = c("n_hosp", "cHospRate", "ahospRate", "avg_los", "charges", "cChargeRate", "avgcharge", "avgcharge_per_day"))) %>%
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

#Creating ggplot facet grid plot
  ggplot(plotData, aes(x = nameOnly, y = measure)) +
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
    strip.text.x = element_text(size = 15)) #increases the size of the facet labels
  
}


