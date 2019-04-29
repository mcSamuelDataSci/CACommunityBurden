
calculated_metrics <- readRDS(file = path(myPlace, "myData/real/countyOSHPD.rds"))

# hospDiscMeasures <-  hospDiscMeasures[-5]


#saving type as factor and specifies the order controls the order in which the facet panels are displayed in ggplot
oshpdPlot <- function(myCounty = "CALIFORNIA", myOSHPDtype = "Number of Hospitalizations", mySex = "Total" ) {
         
   plotData <-    calculated_metrics %>% 
                  mutate(type = factor(type, levels = c("n_hosp", "cHospRate", "ahospRate", "charges", "cChargeRate", "avgcharge"))) %>% 
                #    mutate(type = factor(type, levels = c("n_hosp", "cHospRate", "ahospRate", "charges",                "avgcharge"))) %>% 
                   mutate(type = plyr::revalue(type, hospDiscMeasures)) %>% #replaces values with full name labels
                   left_join(., fullCauseList, by = c("CAUSE" = "LABEL")) %>% 
                #  filter(!is.na(CAUSE), Level == "lev2", county == myCounty, type != "Crude Hosp Rate") %>% 
                   filter(!is.na(CAUSE), Level == "lev2", county == myCounty, !(type %in% c("Crude Hosp Rate","Crude Charge Rate"))) %>% 
    
                   filter(sex == mySex) %>%
                   group_by(type) %>% 
                    mutate(nameOnly = forcats::fct_reorder(nameOnly, filter(., type == myOSHPDtype) %>% 
                   pull(measure)))  
  
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



#Other option: facet_grid(labeller=labeller(type = hospDiscMeasuresShort))--label at the end, however, this prevents wrapping of strip heading text for facets (can only do one or the other)