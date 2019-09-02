#Plotting options for visualizing any vs primary OSHPD data

#Stacked bar plot:
anyprimary1 <- function(myCounty = "CALIFORNIA", mySex = "Total", myprimetype = "any"){
  
any_primary_diff %>% mutate(diag_type = case_when(diag_type == "any_diff" ~ "any",
                                                  diag_type == "primary" ~ diag_type)) %>%
filter(.,county == myCounty, sex == mySex) %>% group_by(diag_type) %>%
    mutate(nameOnly = forcats::fct_reorder(nameOnly, filter(., diag_type == myprimetype) %>%
                                             pull(n_hosp))) %>%
    ggplot(., aes(fill = diag_type, x = nameOnly, y = n_hosp)) + 
    geom_bar(stat = "identity") + coord_flip() +
    theme_bw() +
    theme(plot.title = element_text(face = "bold", size = 15),
      axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 15),
          axis.text.x = element_text(size = 15)) +
    labs(title = "Any and Primary \nHospitalization Diagnoses")
  
  #this orders by "any" condition
}



#Grouped bar plot for showing which "any" diagnoses are associated with primary diagnoses

# group_any_primary <- readRDS(file = path(myPlace, "myData/real/group_any_primary.rds"))
# 
# anyprimary2 <- function(myCounty = "CALIFORNIA", mySex = "Total") {
#   
#   #grouped, not including the primary/any LABEL pairs
#   group_any_primary %>% filter(primary != any, county == myCounty, sex == mySex) %>% 
#     ggplot(., aes(fill = any_name, x = primary_name, y = n_hosp_any)) + 
#     geom_bar(position = "dodge", stat = "identity")  +
#     theme(axis.text.x = element_text(angle = 90, hjust = 1))
#   
#   
#   
#   #grid plot
#   group_any_primary %>% filter(primary != any, county == myCounty, sex == mySex) %>%
#     ggplot(., aes(fill = any_name, x = any_name, y = n_hosp_any)) + 
#     coord_flip() +
#     geom_bar(position = "dodge", stat = "identity") + 
#     facet_grid(. ~ primary_name, scales = "free_x", labeller=labeller(primary_name = label_wrap_gen(5))) +
#     scale_x_discrete(name = "Any diagnosis") + #changes the label of the x axis (y axis when the coordinates flip and the plot comes out)
#     theme(
#       axis.title.x = element_blank(), #removes measure label
#       axis.text.y = element_text(size = 10), #increases size of disease condition labels
#       axis.text.x = element_text(size = 10, face="bold"), #controls size of measure labels
#       strip.text.x = element_text(size = 10)) + #increases the size of the facet labels
#     labs(fill = "Diagnosis") #changes legend title 
#   
#   
#  
#   #or facet wrap?
#   group_any_primary %>% filter(primary != any, county == myCounty, sex == mySex) %>%
#     ggplot(., aes(fill = any_name, x = any_name, y = n_hosp_any)) + 
#     coord_flip() +
#     geom_bar(position = "dodge", stat = "identity") + 
#     facet_wrap(. ~ primary_name, scales = "free_x", labeller=labeller(primary_name = label_wrap_gen(20))) +
#     scale_x_discrete(name = "Any diagnosis") + #changes the label of the x axis (y axis when the coordinates flip and the plot comes out)
#     theme(
#         axis.title.x = element_blank(), #removes measure label
#         axis.text.y = element_text(size = 10), #increases size of disease condition labels
#         axis.text.x = element_text(size = 10, face="bold"), #controls size of measure labels
#         strip.text.x = element_text(size = 10)) + #increases the size of the facet labels
#     labs(fill = "Diagnosis") #changes legend title 
#     
#   
  
#}



