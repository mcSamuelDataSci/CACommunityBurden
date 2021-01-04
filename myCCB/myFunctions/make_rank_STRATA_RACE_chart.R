rankStrataRace <- function(myRace = "Black-NH",
                           myCounty = "Los Angeles",
                           myData = "Deaths",
                         myAgeG = "75 - 84",
                         myMeasure = "aRate",
                         mySex = "Total",
                         myN = 10,
                         myYearG3 = "2016-2018"
                            ){
  
  myTextSize <- 18
  myData = "Deaths"
  
  
  if (myData == "Deaths") {
      myMeasure = "aRate"
    
      tLab  <- "Deaths"
     
      t.dataSet  <- datCounty_RE %>% 
        filter(yearG3 == myYearG3, Level == "lev2")   %>%
        select(sex,raceCode,causeCode,county,measure=myMeasure) %>%
        filter(sex == mySex,!is.na(raceCode)) 
        
      causeTemp <- data.frame(causeCode = fullCauseList$causeCode ,stringsAsFactors = FALSE)
      raceTemp   <- data.frame(raceCode = unique(t.dataSet$raceCode),stringsAsFactors = FALSE)
      county    <- data.frame(county   = unique(t.dataSet$county),stringsAsFactors = FALSE)
      fullMat   <- sqldf(" select * from  county cross join causeTemp cross join raceTemp")
      
      t.dataSet <- full_join(t.dataSet,fullMat,by=c("county","causeCode","raceCode")) %>%
        arrange(county,causeCode, raceCode) %>%
        mutate(measure = ifelse(is.na(measure),0,measure))    %>%
        left_join(., fullCauseList, by = "causeCode") %>%
        rename(ccsName = causeName)
      
  }
  
  # create a vector of CAUSE for top N
  theseCodes <- t.dataSet %>%
                filter(county == myCounty) %>%
                group_by(raceCode) %>% arrange(desc(measure)) %>% 
                dplyr::slice(1:myN) %>% #this selects the top N rows for myOSHPDtype
                filter(raceCode == myRace) %>% 
                ungroup() %>% pull(causeCode)
  
  
  # creates dataframe with data only for CAUSEs from theseCodes, i.e. the top N CAUSES for the specified theseCodes
  #  and sorts by frequency of measure for plotting
  plot_data.2 <-  t.dataSet %>%
                  filter(!is.na(causeCode), county == myCounty, causeCode %in% theseCodes) %>%
                  group_by(raceCode)    %>%
                  mutate(ccsName = forcats::fct_reorder(ccsName, filter(., raceCode == myRace)  %>%
                  pull(measure)))
  
  sexLab <- ""
  if (mySex != "Total") sexLab <- paste0(", among ",mySex,"s")
  myTitle <- paste0("Number of ", tLab," by Cause by Age Group, Age Group ", 
                    myRace, " ORDER, in ", myYearG3, " in ", myCounty, sexLab)
  
  
  
  raceCausePlot  <-   ggplot(plot_data.2, aes(x = ccsName, y = measure)) +
                     geom_bar(stat = "identity", fill = "blue") +
                     coord_flip() + 
                     facet_grid(. ~ raceCode, labeller=labeller(type = label_wrap_gen(5))) +
                     theme_bw() + 
                      scale_y_continuous(labels = scales::comma) + # numbers shown with commas
                      scale_x_discrete(labels = scales::wrap_format(50)) + #x-axis is condition label--wrapping text so it stacks on top of each other
                      labs(title = myTitle,x ="condition",y = paste("Age-Adjusted Death Rate")) +
                     theme(plot.title = element_text(size=22, color="blue"),
                     axis.text.x = element_text(angle = 90, hjust = 1,size = myTextSize-6),
                     axis.text.y = element_text(size = myTextSize), #increases size of disease condition labels
                     strip.text.x = element_text(size = myTextSize),
                     axis.title=element_text(size=myTextSize,face="bold"))
  
  raceCausePlot
  
}

