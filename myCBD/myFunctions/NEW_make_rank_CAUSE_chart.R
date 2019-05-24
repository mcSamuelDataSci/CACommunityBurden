
rankCause2 <- function(myCounty = "Los Angeles", myMeasure = "Number of deaths", mySex = "Total", myLev = "lev2", myN = 10, myYear = 2017){ 
  

xMeasures <- lMeasuresShort[c(1,3,2,4,5)]

library(tidyr)
temp <- datCounty %>% gather(key = "type", value = "measure", Ndeaths,YLLper,aRate,mean.age,SMR)  %>% 
                      select(year,sex,Level,CAUSE,county,type,measure) %>%
                      mutate(type = factor(type,levels=xMeasures ))    %>%
                      mutate(type = plyr::revalue(type, xJunk))    %>% #replaces values with full name labels
                      left_join(., fullCauseList, by = c("CAUSE" = "LABEL"))  

#create a vector of CAUSE for top N 
temp_N_cause <- temp %>%
  filter(sex == mySex, Level == myLev, year == myYear) %>%
  group_by(type) %>% arrange(desc(measure)) %>% dplyr::slice(1:myN) %>% #this selects the top N rows for myOSHPDtype
  filter(type == myMeasure) %>% ungroup() %>% pull(CAUSE)

#creates dataframe with data only for CAUSEs from temp_N_cause, i.e. the top N CAUSES for the specified temp_N_cause        
   plotData <-     temp %>%
                   filter(!is.na(CAUSE), Level == myLev, county == myCounty, CAUSE %in% temp_N_cause) %>% 
                   filter(sex == mySex, year == myYear) %>%
                   group_by(type)    %>% 
                   mutate(nameOnly = forcats::fct_reorder(nameOnly, filter(., type == myMeasure)  %>% 
                   pull(measure)))  

   if (myCounty != "CALIFORNIA") {
#Notes about adding line to single facet area: https://stackoverflow.com/questions/34686217/how-can-i-add-a-line-to-one-of-the-facets
 SMR <- 1
 
 xtemp <-     ggplot(plotData, aes(x = nameOnly, y = measure)) + 
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
          strip.text.x = element_text(size = 15)) + #increases the size of the facet labels 
          geom_hline(data = data.frame(yint = SMR, type = "Standard Mortality Ratio"), aes(yintercept = yint), color = "red") #adds SMR line only to SMR facet
# 
xtemp 

#ggplotly(xtemp) #SMR ratio gets cut off/only shows at the bottom of the plot when it is put in plotly
   }
   
   if (myCounty == "CALIFORNIA") {
     
     plotData <- plotData %>% filter(type != "Standard Mortality Ratio")
     
     xtemp <-     ggplot(plotData, aes(x = nameOnly, y = measure)) + 
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
             strip.text.x = element_text(size = 15))  #increases the size of the facet labels 
     xtemp
   }
 
}
  
 #plotly version??


  if (myCounty != "CALIFORNIA") {
  #Notes about adding line to single facet area: https://stackoverflow.com/questions/34686217/how-can-i-add-a-line-to-one-of-the-facets
  SMR <- 1
  
  xtemp <- plotData %>% filter(type == "Standard Mortality Ratio") %>% plotly::plot_ly(., y = ~nameOnly, x = ~measure, type = "bar", name = "Standard Mortality Ratio")  
  
   xtemp <- layout(xtemp, shapes = list(type = "line", fillcolor = "red", opacity = 1, x0 = 0, x1 = 0, xref = 'measure', y0 = 0, y1 = 1, yref = 'y'))

  xtemp 
  #this doesn't really do what we want either
}


