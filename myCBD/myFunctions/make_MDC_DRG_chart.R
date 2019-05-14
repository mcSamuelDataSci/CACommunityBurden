#make MDC/DRG plot


mdc_drg <- readRDS(file = path(myPlace, "myData/fake/mdc_drg.rds"))


# #---------------------------------------Plotly subplots option----------------------------------------------------#
mdc_drg_plot <- function(myCounty = "CALIFORNIA", myOSHPDtype = "Number of Hospitalizations", mySex = "Total", myN = 10, myVar = "drg" ) {
  
  #sorts that based on the ordering variable (eg n_hosp), it pulls the top N rows for n_hosp, and those variables are what are the corresponding conditions for all the other values
  #create a vector of CAUSE for top N of myOSHPDtype
  mdc_drg_N_cause <- mdc_drg %>%
    mutate(type = plyr::revalue(type, hospDiscMeasuresShort2)) %>% #replaces values with full name labels
    left_join(., hdCodes, by = c("mdc" = "mdc_drg_codes")) %>%
    filter(county == myCounty, sex == mySex, diagnosis_var == myVar) %>%
    group_by(type) %>% arrange(desc(measure)) %>% dplyr::slice(1:myN) %>% #this selects the top N rows for myOSHPDtype
    filter(type == myOSHPDtype) %>% ungroup() %>% pull(mdc) 
  
  #creates dataframe with data only for CAUSEs from myOSHPDtype_N_cause, i.e. the top N CAUSES for the specified myOSHPDtype
  plotData <- mdc_drg %>%
    mutate(type = factor(type, levels = c("n_hosp","charges","avgcharge"))) %>%
    mutate(type = plyr::revalue(type, hospDiscMeasuresShort2), type = as.character(type)) %>% #replaces values with full name labels
    left_join(., hdCodes, by = c("mdc" = "mdc_drg_codes")) %>%
    filter(county == myCounty, sex == mySex, diagnosis_var == myVar, mdc %in% mdc_drg_N_cause) %>% 
    group_by(type) %>%
    mutate(names = forcats::fct_reorder(names, filter(., type == myOSHPDtype) %>%
                                             pull(measure)))
  #making plot
  #req(nrow(plotData) > 0)
  
  ggplot(plotData, aes(x = names, y = measure)) +
    coord_flip() + geom_bar(stat = "identity", fill = "blue") +
    facet_grid(. ~ type, scales = "free_x", labeller=labeller(type = label_wrap_gen(5))) +
    theme_bw() + #need to specify theme first, before changing/removing axis titles/labels etc. If theme_bw() is put at end, it negates all of these changes
    scale_y_continuous(labels = scales::comma) + #numbers shown with commas rather than scientific notation
    #scale_x_discrete(labels = scales::wrap_format(10)) + #x-axis is condition label--wrapping text so it stacks on top of each other
    #within theme--x and y axis refer to the way it looks, with coord_flip(), so y refers to vertical label (which technically is really x axis) and vice versa with x axis
    theme(axis.title.y = element_blank(), #removes nameOnly label
          axis.title.x = element_blank(), #removes measure label
          axis.text.y = element_text(size = 10), #increases size of disease condition labels
          axis.text.x = element_text(size = 10, face="bold"), #controls size of measure labels
          strip.text.x = element_text(size = 15)) #increases the size of the facet labels

  
}
  
  
  



