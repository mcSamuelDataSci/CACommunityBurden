
calculated_metrics <- readRDS(file = path(myPlace, "myData/real/countyOSHPD.rds"))

if(1 == 2) {
#saving type as factor and specifies the order controls the order in which the facet panels are displayed in ggplot
oshpdPlot <- function(myCounty = "California", myOSHPDtype = "n_hosp") {
  calculated_metrics %>% mutate(type = factor(type, levels = c("n_hosp", "cHospRate", "ahospRate", "charges", "cChargeRate", "avgcharge"))) %>% mutate(type = plyr::revalue(type, hospDiscMeasures)) %>% #replaces values with full name labels
  left_join(., fullCauseList, by = c("CAUSE" = "LABEL")) %>% filter(!is.na(CAUSE), Level == "lev2", county == myCounty, type != "cHospRate") %>% filter(sex == "Total") %>%
    group_by(type) %>% mutate(nameOnly = forcats::fct_reorder(nameOnly, filter(., type == myOSHPDtype) %>% pull(measure))) %>% 
    ggplot(., aes(x = nameOnly, y = measure)) + coord_flip() + geom_bar(stat = "identity", fill = "blue") + facet_grid(. ~ type, scales = "free_x", labeller=labeller(type = label_wrap_gen(5))) + 
    theme_bw() + #need to specify theme first, before changing/removing axis titles/labels etc. If theme_bw() is put at end, it negates all of these changes
    scale_y_continuous(labels = scales::comma) + #numbers shown with commas rather than scientific notation
    scale_x_discrete(labels = wrap_format(10)) + #x-axis is condition label--wrapping text so it stacks on top of each other
    #within theme--x and y axis refer to the way it looks, with coord_flip(), so y refers to vertical label (which technically is really x axis) and vice versa with x axis
    theme(axis.title.y = element_blank(), #removes nameOnly label
          axis.title.x = element_blank(), #removes measure label
          axis.text.y = element_text(size = 15), #increases size of disease condition labels
          axis.text.x = element_text(size = 8), #controls size of measure labels
          strip.text.x = element_text(size = 15)) #increases the size of the facet labels
    
          
} }


oshpdPlot <- function(myCounty = "California", myOSHPDtype = "Number of Hospitalizations") {
calculated_metrics %>% mutate(type = factor(type, levels = c("n_hosp", "cHospRate", "ahospRate", "charges", "cChargeRate", "avgcharge"))) %>%
left_join(., fullCauseList, by = c("CAUSE" = "LABEL")) %>% filter(!is.na(CAUSE), Level == "lev2", county == myCounty, type != "cHospRate") %>% filter(sex == "Total") %>%
group_by(type) %>% mutate(nameOnly = forcats::fct_reorder(nameOnly, filter(., type == myOSHPDtype) %>% pull(measure))) %>% 
  ggplot(., aes(x = nameOnly, y = measure)) + coord_flip() + geom_bar(stat = "identity", fill = "blue") + facet_grid(. ~ type, scales = "free_x", labeller=labeller(type = hospDiscMeasuresShort)) + #labeller allows us to rename values
  theme_bw() + #need to specify theme first, before changing/removing axis titles/labels etc. If theme_bw() is put at end, it negates all of these changes
  scale_y_continuous(labels = scales::comma) + #numbers shown with commas rather than scientific notation
  scale_x_discrete(labels = wrap_format(10)) + #x-axis is condition label--wrapping text so it stacks on top of each other
  #within theme--x and y axis refer to the way it looks, with coord_flip(), so y refers to vertical label (which technically is really x axis) and vice versa with x axis
  theme(axis.title.y = element_blank(), #removes nameOnly label
        axis.title.x = element_blank(), #removes measure label
        axis.text.y = element_text(size = 15), #increases size of disease condition labels
        axis.text.x = element_text(size = 8), #controls size of measure labels
        strip.text.x = element_text(size = 15)) #increases the size of the facet labels

}

