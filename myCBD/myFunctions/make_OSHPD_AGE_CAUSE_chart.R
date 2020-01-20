

rankOSHPDAgeCause <- function(myCounty = "Los Angeles",
                         myMeasure = "n_hosp",
                         mySex = "Total",
                         myN = 10,
                         myYearG3 = "2016-2018",
                         myAgeG = "75 - 84",
                         myData = "PDD"){

  
 if (myData == "PDD") { t.dataSet  <- pddCounty_AGE %>% mutate(ccsCode = ccs_diagP)
                             tLab  <- "Hospitalizations"}
 if (myData == "ED")  { t.dataSet  <- ED.age 
                             tLab  <- "ER Visits"} 
        
        
 t.dataSet <- t.dataSet %>%      
                select(ageG, county, ccsCode, measure = n_hosp,sex,year) %>%  #measure rename is a quick fix for now
                mutate(ageG = ifelse(ageG == "5 - 14"," 5 - 14",ageG),
                       ageG = ifelse(ageG == "0 - 4"," 0 - 4",ageG)) %>%
                filter(sex == mySex)   %>% select(-sex,-year)    ## ALL sex = "Total"  and year is only = 2016 for now
  
 library(sqldf)
 causeTemp <- data.frame(ccsCode = unique(t.dataSet$ccsCode))
 ageTemp   <- data.frame(ageG = unique(t.dataSet$ageG))
 county    <- data.frame(county   = unique(t.dataSet$county))
 fullMat   <- sqldf(" select * from  county cross join causeTemp cross join ageTemp")

  
 t.dataSet <- full_join(t.dataSet,fullMat,by=c("county","ccsCode","ageG")) %>%
               arrange(county,ccsCode, ageG) %>%
               mutate(measure = ifelse(is.na(measure),0,measure))  %>%
               left_join(ccsLinker(),by="ccsCode")   %>%
               filter(is.na(birth)) %>% select(-birth) %>%
               rename(CAUSE = ccsCode)
  

# create a vector of CAUSE for top N
 theseCodes <- t.dataSet %>%
   filter(county == myCounty) %>%
   group_by(ageG) %>% arrange(desc(measure)) %>% dplyr::slice(1:myN) %>% #this selects the top N rows for myOSHPDtype
   filter(ageG == myAgeG) %>% ungroup() %>% pull(CAUSE)

  
  
# creates dataframe with data only for CAUSEs from theseCodes, i.e. the top N CAUSES for the specified theseCodes
 plot_data.2 <-     t.dataSet %>%
   filter(!is.na(CAUSE), county == myCounty, CAUSE %in% theseCodes) %>%
   #filter(sex == mySex, year == myYear) %>%
   group_by(ageG)    %>%
   mutate(ccsName = forcats::fct_reorder(ccsName, filter(., ageG == myAgeG)  %>%
                                           pull(measure)))

   sexLab <- ""
   if (mySex != "Total") sexLab <- paste0(", among ",mySex,"s")
   myTitle <- paste0("Number of ", tLab,
                     " by Cause by Age Group, Age Group ", 
                     myAgeG, " ORDER, in ", myYear, " in ", myCounty, sexLab)

 ageCausePlot  <-     ggplot(plot_data.2, aes(x = ccsName, y = measure)) +
      coord_flip() + geom_bar(stat = "identity", fill = "blue") +
      facet_grid(. ~ ageG,                    labeller=labeller(type = label_wrap_gen(5))) +
   #  facet_grid(. ~ ageG, scales = "free_x", labeller=labeller(type = label_wrap_gen(5))) +
      theme_bw() + 
      scale_y_continuous(labels = scales::comma) + # numbers shown with commas
      scale_x_discrete(labels = scales::wrap_format(50)) + #x-axis is condition label--wrapping text so it stacks on top of each other
      labs(title = myTitle,x ="condition",y = paste("Number of",tLab)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

 ageCausePlot

   }



#(for testing code outside of the app)
if(1==2) {
  myCounty = "CALIFORNIA"
  mySex = "Total"
  myN = 10
  myYearG3 = "2016-2018"
  myAgeG = "75 - 84"
  myAgeG = "0 - 4"
  myAgeG = "25 - 34"
  myAgeG = "15 - 24"
  myMeasure = "n_hosp"
  myData = "PDD"
  myData = "ED"
  #cDeathRate"
}


# rankOSHPDAgeCause(myCounty = "Los Angeles",
#                               myMeasure = "n_hosp",
#                               mySex = "Total",
#                               myN = 10,
#                               myYearG3 = "2016-2018",
#                               myAgeG = "75 - 84",
#                               myData = "PDD")



