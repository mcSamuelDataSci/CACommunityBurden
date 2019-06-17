#Plotting options for visualizing any vs primary OSHPD data

#Stacked bar plot:
any_primary_diff <- readRDS(file = path(myPlace, "myData/fake/any_primary_stackedbar.rds"))


anyprimary1 <- function(myCounty = "CALIFORNIA", mySex = "Total"){
  
  any_primary_diff %>% filter(county == myCounty, sex == mySex) %>%
    ggplot(., aes(fill = diag_type, x = nameOnly, y = n_hosp)) + 
    geom_bar(stat = "identity") + coord_flip()
}



#Grouped bar plot for showing which "any" diagnoses are associated with primary diagnoses

group_any_primary <- readRDS(file = path(myPlace, "myData/fake/group_any_primary.rds"))

anyprimary2 <- function(myCounty = "CALIFORNIA", mySex = "Total") {
  
  #grouped, not including the primary/any LABEL pairs
  group_any_primary %>% filter(primary != any, county == myCounty, sex == mySex) %>% 
    ggplot(., aes(fill = any_name, x = primary_name, y = n_hosp_any)) + 
    geom_bar(position = "dodge", stat = "identity")  +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
}



#Circular barplot? https://jokergoo.github.io/circlize_book/book/the-chorddiagram-function.html#directional-relations
# Charge the circlize library
library(circlize)
#Note: If you use it in published research, please cite:
#Gu, Z. circlize implements and enhances circular visualization 
#in R. Bioinformatics 2014.


anyprimary3 <- function(myCounty = "CALIFORNIA", mySex = "Total") {
  
  chord_any_primary <- group_any_primary %>%  filter(county == myCounty, sex == mySex) %>% 
    select(primary_name, any_name, n_hosp_any) %>% group_by(primary_name) #Direction "from" primary_name (the grouping variable) "to" the any_name
  
  chordDiagram(chord_any_primary, transparency = 0.1, directional = 1) #directional = 1 means from col1 to col2
  
  #This isn't great either......
  
  
}

