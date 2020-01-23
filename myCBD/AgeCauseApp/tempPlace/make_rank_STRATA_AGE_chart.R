rankStrataAge <- function(myAgeG = "75 - 84",
                         myCounty = "Los Angeles",
                         myData = "Deaths",
                         myMeasure = "n_hosp",
                         mySex = "Total",
                         myN = 10,
                         myYearG3 = "2016-2018"
                            ){
  
  myTextSize <- 18
  
  if (myData == "Deaths") {
      myMeasure = "Ndeaths"
    
      tLab  <- "Deaths"
     
      t.dataSet  <- datCounty_AGE_3year %>% 
        filter(yearG3 == myYearG3, Level == "lev2") %>%
        select(sex,ageG,CAUSE,county,measure=myMeasure) %>%
        mutate(ageG = ifelse(ageG == "5 - 14"," 5 - 14",ageG),
               ageG = ifelse(ageG == "0 - 4"," 0 - 4",ageG),
               ageG = ifelse(ageG == "85 - 999","85+",ageG),) %>%
        filter(sex == mySex,!is.na(ageG)) 
        
      causeTemp <- data.frame(CAUSE = fullCauseList$LABEL ,stringsAsFactors = FALSE)
      ageTemp   <- data.frame(ageG = unique(t.dataSet$ageG),stringsAsFactors = FALSE)
      county    <- data.frame(county   = unique(t.dataSet$county),stringsAsFactors = FALSE)
      fullMat   <- sqldf(" select * from  county cross join causeTemp cross join ageTemp")
      
      t.dataSet <- full_join(t.dataSet,fullMat,by=c("county","CAUSE","ageG")) %>%
        arrange(county,CAUSE, ageG) %>%
        mutate(measure = ifelse(is.na(measure),0,measure))    %>%
        left_join(., fullCauseList, by = c("CAUSE" = "LABEL")) %>%
        rename(ccsName = nameOnly)
      
  }
  
  
  if (myData == "PDD") { t.dataSet  <- oshpd_PDD_AGE 
                         tLab       <- "Hospitalizations"}
  if (myData == "ED")  { t.dataSet  <- oshpd_ED_AGE
                         tLab       <- "ER Visits"} 
  
  if (myData %in% c("PDD","ED")) {
  
  t.dataSet <- t.dataSet %>%      
    select(ageG, county, ccsCode, measure = n_hosp,sex,year) %>%  #measure rename is a quick fix for now
    filter(sex == mySex,!is.na(ageG))   %>% select(-sex,-year)    ## ALL sex = "Total"  and year is only = 2016 for now
  
  
  causeTemp <- data.frame(ccsCode = unique(t.dataSet$ccsCode))
  ageTemp   <- data.frame(ageG = unique(t.dataSet$ageG))
  county    <- data.frame(county   = unique(t.dataSet$county))
  fullMat   <- sqldf(" select * from  county cross join causeTemp cross join ageTemp")
  
  t.dataSet <- full_join(t.dataSet,fullMat,by=c("county","ccsCode","ageG")) %>%
    arrange(county,ccsCode, ageG) %>%
    mutate(measure = ifelse(is.na(measure),0,measure))  %>%
    left_join(ccsMap,by="ccsCode")    %>%
    filter(!birth) %>% select(-birth) %>%
    rename(CAUSE = ccsCode)
  
  }
  
  # create a vector of CAUSE for top N
  theseCodes <- t.dataSet %>%
                filter(county == myCounty) %>%
                group_by(ageG) %>% arrange(desc(measure)) %>% 
                dplyr::slice(1:myN) %>% #this selects the top N rows for myOSHPDtype
                filter(ageG == myAgeG) %>% 
                ungroup() %>% pull(CAUSE)
  
  
  # creates dataframe with data only for CAUSEs from theseCodes, i.e. the top N CAUSES for the specified theseCodes
  #  and sorts by frequency of measure for plotting
  plot_data.2 <-  t.dataSet %>%
                  filter(!is.na(CAUSE), county == myCounty, CAUSE %in% theseCodes) %>%
                  group_by(ageG)    %>%
                  mutate(ccsName = forcats::fct_reorder(ccsName, filter(., ageG == myAgeG)  %>%
                  pull(measure)))
  
  sexLab <- ""
  if (mySex != "Total") sexLab <- paste0(", among ",mySex,"s")
  myTitle <- paste0("Number of ", tLab," by Cause by Age Group, Age Group ", 
                    myAgeG, " ORDER, in ", myYearG3, " in ", myCounty, sexLab)
  
  
  
  ageCausePlot  <-   ggplot(plot_data.2, aes(x = ccsName, y = measure)) +
                     geom_bar(stat = "identity", fill = "blue") +
                     coord_flip() + 
                     facet_grid(. ~ ageG, labeller=labeller(type = label_wrap_gen(5))) +
                  #  facet_grid(. ~ ageG, scales = "free_x"
                     theme_bw() + 
                      scale_y_continuous(labels = scales::comma) + # numbers shown with commas
                      scale_x_discrete(labels = scales::wrap_format(50)) + #x-axis is condition label--wrapping text so it stacks on top of each other
                      labs(title = myTitle,x ="condition",y = paste("Number of",tLab)) +
                     theme(plot.title = element_text(size=22, color="blue"),
                     axis.text.x = element_text(angle = 90, hjust = 1,size = myTextSize-6),
                     axis.text.y = element_text(size = myTextSize), #increases size of disease condition labels
                     strip.text.x = element_text(size = myTextSize),
                     axis.title=element_text(size=myTextSize,face="bold"))
  
  ageCausePlot
  
}

