#OSHPD Chart 1 = ggplot version


calculated_metrics <- readRDS(file = path(myPlace, "myData/real/countyOSHPD.rds"))


# #---------------------------------------Plotly subplots option----------------------------------------------------#
oshpdPlot1 <- function(myCounty = "CALIFORNIA", myOSHPDtype = "Number of Hospitalizations", mySex = "Total", myN = 10 ) {

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
  myOSHPDtype_N_cause <- calculated_metrics %>% mutate(type = factor(type, levels = c("n_hosp", "cHospRate", "ahospRate", "charges", "cChargeRate", "avgcharge"))) %>%
    mutate(type = plyr::revalue(type, hospDiscMeasures)) %>% #replaces values with full name labels
    left_join(., fullCauseList, by = c("CAUSE" = "LABEL")) %>%
    filter(!is.na(CAUSE), Level == "lev2", county == myCounty) %>%
    filter(sex == mySex) %>%
    group_by(type) %>% arrange(desc(measure)) %>% dplyr::slice(1:myN) %>% #this selects the top N rows for myOSHPDtype
    filter(type == myOSHPDtype) %>% ungroup() %>% pull(CAUSE) 
  
#creates dataframe with data only for CAUSEs from myOSHPDtype_N_cause, i.e. the top N CAUSES for the specified myOSHPDtype
  plotData <- calculated_metrics %>%
    mutate(type = factor(type, levels = c("n_hosp", "cHospRate", "ahospRate", "charges", "cChargeRate", "avgcharge"))) %>%
    mutate(type = plyr::revalue(type, hospDiscMeasures)) %>% #replaces values with full name labels
    left_join(., fullCauseList, by = c("CAUSE" = "LABEL")) %>%
    filter(!is.na(CAUSE), Level == "lev2", county == myCounty, !(type %in% c("Crude Hosp Rate","Crude Charge Rate"))) %>% filter(., CAUSE %in% myOSHPDtype_N_cause) %>% filter(sex == mySex) %>%
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
    strip.text.x = element_text(size = 10)) #increases the size of the facet labels
}







if (1 == 2){
#####ORIGINAL, NON-ranked CODE####
plotData <- calculated_metrics %>%
  mutate(type = factor(type, levels = c("n_hosp", "cHospRate", "ahospRate", "charges", "cChargeRate", "avgcharge"))) %>%
  mutate(type = plyr::revalue(type, hospDiscMeasures)) %>% #replaces values with full name labels
  left_join(., fullCauseList, by = c("CAUSE" = "LABEL")) %>%
  filter(!is.na(CAUSE), Level == "lev2", county == myCounty, !(type %in% c("Crude Hosp Rate","Crude Charge Rate"))) %>%
  filter(sex == mySex) %>%
  group_by(type) %>%
  mutate(nameOnly = forcats::fct_reorder(nameOnly, filter(., type == myOSHPDtype) %>%
                                           pull(measure))) %>% dplyr::arrange(type == myOSHPDtype, desc(measure)) %>% 
  dplyr::slice(1:myN) 
#########
#Other option: facet_grid(labeller=labeller(type = hospDiscMeasuresShort))--label at the end, however, this prevents wrapping of strip heading text for facets (can only do one or the other)

}