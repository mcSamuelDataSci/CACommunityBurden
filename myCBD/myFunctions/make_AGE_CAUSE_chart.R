

#(for testing code outside of the app) 
if(1==2) {
  myCounty = "CALIFORNIA"
  mySex = "Total"
  myLev = "lev2"
  myN = 10
  myYearG3 = "2016-2018"
  myAgeG = "75 - 84"
  myMeasure = "cDeathRate"
}



rankCause <- function(myCounty = "Los Angeles", myMeasure = "Number of deaths", mySex = "Total", myLev = "lev2", myN = 10, myYear = 2017){ 
  

  if(myLev=="lev3") myLev <- c("lev2","lev3")
  
  

  temp <- datCounty_AGE_3year %>% select(yearG3,sex,ageG,Level,CAUSE,county,measure=myMeasure) %>%
         filter(sex == mySex, Level %in% myLev, yearG3 == myYearG3) %>%
         select(-yearG3,-sex,-Level)
  
  
  fullCauseList     <- gbdMap0[!is.na(gbdMap0$causeList),] %>% arrange(LABEL)  %>%
                          filter(!is.na(PH),is.na(DETAIL)) %>%
                          select(LABEL,causeList,nameOnly)
  library(sqldf)
  causeTemp <- data.frame(CAUSE = fullCauseList$LABEL)
  ageTemp <- data.frame(ageG = unique(temp$ageG))
  county   <- data.frame(county   = unique(temp$county))        
  
  fullMat <- sqldf(" select * from  county cross join causeTemp cross join ageTemp") 
  
  
  
  temp <- full_join(temp,fullMat,by=c("county","CAUSE","ageG")) %>% 
              arrange(county,CAUSE, ageG) %>%
              mutate(measure = ifelse(is.na(measure),0,measure)) %>%
  left_join(., fullCauseList, by = c("CAUSE" = "LABEL")) 
  
  
  
#ordering dataset, replaces short names with full names
# temp <- datCounty %>% gather(key = "type", value = "measure", Ndeaths,YLLper,aRate,mean.age,SMR)  %>% 
#                       select(year,sex,Level,CAUSE,county,type,measure) %>%
#                       mutate(type = factor(type,levels= dM_short))    %>% #orders factor
#                       mutate(type = plyr::revalue(type, dMRevalue_short))    %>% #replaces values with full name labels
#                       left_join(., fullCauseList, by = c("CAUSE" = "LABEL"))  

  
temp <- filter(temp,county==myCounty)
  


#creates dataframe with data only for CAUSEs from temp_N_cause, i.e. the top N CAUSES for the specified temp_N_cause        
plotData <-     temp %>%
  filter(!is.na(CAUSE), Level %in% myLev, county == myCounty, CAUSE %in% temp_N_cause) %>% 
  filter(sex == mySex, year == myYear) %>%
  group_by(type)    %>% 
  mutate(nameOnly = forcats::fct_reorder(nameOnly, filter(., type == myMeasure)  %>% 
                                           pull(measure)))  


#create a vector of CAUSE for top N 
temp_N_cause <- temp %>%
  group_by(ageG) %>% arrange(desc(measure)) %>% 
  dplyr::slice(1:myN) %>% #this selects the top N rows for myOSHPDtype
  filter(ageG == myAgeG) %>%ungroup() %>% pull(CAUSE)




#creates dataframe with data only for CAUSEs from temp_N_cause, i.e. the top N CAUSES for the specified temp_N_cause        
   plotData <-     temp %>%
                   filter(!is.na(CAUSE), county == myCounty, CAUSE %in% temp_N_cause) %>% 
                   #filter(sex == mySex, yearG3 == myYearG3) %>%
                   group_by(ageG)     %>% 
                #   mutate(nameOnly = forcats::fct_reorder(nameOnly, filter(., measure == myMeasure)  %>% 
                  mutate(nameOnly = forcats::fct_reorder(nameOnly, filter(.,ageG == myAgeG)  %>% 
                                                       pull(measure)))  

   
   sexLab <- ""
   if (mySex != "Total") sexLab <- paste0(", among ",mySex,"s")
   tit <- paste0("Measures by Cause in ",myYear," in ",myCounty,sexLab)
   
   

##Creating ggplot facet grid plots
   

     
#Notes about adding line to single facet area: https://stackoverflow.com/questions/34686217/how-can-i-add-a-line-to-one-of-the-facets
 
 xtemp <-     ggplot(plotData, aes(x = nameOnly, y = measure)) + 
      coord_flip() + geom_bar(stat = "identity", fill = "blue") + 
      facet_grid(. ~ ageG, scales = "free_x", labeller=labeller(type = label_wrap_gen(5))) + 
      
      theme_bw() + #need to specify theme first, before changing/removing axis titles/labels etc. If theme_bw() is put at end, it negates all of these changes
      scale_y_continuous(labels = scales::comma) + #numbers shown with commas rather than scientific notation
      scale_x_discrete(labels = scales::wrap_format(50)) + #x-axis is condition label--wrapping text so it stacks on top of each other
    labs(title = tit) +
       #within theme--x and y axis refer to the way it looks, with coord_flip(), so y refers to vertical label (which technically is really x axis) and vice versa with x axis  
    theme(plot.title = element_text(size=myTitleSize, color=myTitleColor),
          axis.title.y = element_blank(), #removes nameOnly label
          axis.title.x = element_blank(), #removes measure label
          axis.text.y = element_text(size = myAxisSize), #increases size of disease condition labels
          axis.text.x = element_text(size = myAxisSize, face="bold",angle = 90, hjust = 1), #controls size of measure labels
          
          strip.text.x = element_text(size = myAxisSize))  #increases the size of the facet labels 
 
 
   
   xtemp
 
}

