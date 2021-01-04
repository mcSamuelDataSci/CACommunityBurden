# -- This chunk is here for loading, and testing the function, but will be removed after finalizing the function ---------------------------
# library(dplyr)
# library(ggplot2)

# ------------------------------------------------------------------------------------------------------------------

if(1==2) {
  myDataSet     = "hosp_race" # leave it in for now
  myData        = "Hospitalizations"   # "Hospitalizations", "Emergency Department".. will be part of  title
  myStrata  = "Race/Ethnicity" # "Race/Ethnicity" , "sex".. for title
  mySort        = "Black"
  myCounty      = "CALIFORNIA"
  myMeasure     = "cRate"           # n_hosp
  mySex         = "Total"
  myN           = 10
  myYearG3      = "2016-2018" # no year in dataset
  myScale       = "fixed"
  myFillManual  = T
  
  myData = "Deaths"
  myStrata = "Age Group"
  mySort        = "85+"
  
}


raceSort <- raceLink$raceCode
ageSort  <- ageLink$ageName

makePlotRank <- function(myDataSet     = NA,
                         myData        = "Deaths",   # "Hospitalizations", "Emergency Department".. will be part of  title
                         myStrata      = "Race/Ethnicity", # "Race/Ethnicity", # "Race/Ethnicity" , "sex".. for title
                         mySort        = "White",
                         myCounty      = "Los Angeles",
                         myMeasure     = "N", 
                         mySex         = "Total",
                         myN           = 10,
                         myYearG3      = "2016-2018", # no year in dataset
                         myScale       = "free",
                         myFillManual  = T # Set true if the bars should be filled by topLev (which will then have a legend). Setting to F defaults to blue bars
){
  
   if(1==200) {
  # Looking at myMeasure to: 1) filter type, 2) create titles for plots
  if (grepl("Rate", myMeasure, fixed = T)) {
    filterType <- "cRate"
    measureTitle <- paste(substr(myData, 1, nchar(myData)-1), myMeasure)
  } else {
    filterType <- "measure" # To change soon, but this refers to number
   # measureType <- paste(myMeasure, "of", myData) # not used....
  }
  }
  
  if (myData == "Deaths"               & myStrata == "Age Group") myDataSet <- death_age
  if (myData == "Hospitalizations"     & myStrata == "Age Group") myDataSet <- hosp_age
  if (myData == "Emergency Department" & myStrata == "Age Group") myDataSet <- ed_age
  
  if (myData == "Deaths"               & myStrata == "Race/Ethnicity") myDataSet <- death_race
  if (myData == "Hospitalizations"     & myStrata == "Race/Ethnicity") myDataSet <- hosp_race
  if (myData == "Emergency Department" & myStrata == "Race/Ethnicity") myDataSet <- ed_race
 
  
   tDat <- myDataSet %>%
    filter(county == myCounty, !MAINSTRATA %in% c("Unknown", "Other"), causeName != "Liveborn") %>%
      mutate(measure = get(myMeasure))
   
   
   if(myStrata == "Age Group")  tDat <- mutate(tDat, MAINSTRATA = factor(MAINSTRATA, levels = ageSort)) 
   
   
   #....one way we might do this...
   if (is.na(myStrata)) { 
        filterType <- "measure"
       tDat <- tDat    %>%
               pivot_longer(cols = c("measure", "cRate"), names_to = "measureType", values_to = "measure") %>%
               filter(measureType == filterType)
      }
   
  
  # Sorting ----------------------------------------------------------------------------------------
  
  pullTopN <- tDat %>%
    filter(MAINSTRATA == mySort) %>%
    arrange(desc(measure)) %>%
    slice(1:myN) %>%
    pull(causeName)
  
  pullTopN <- factor(pullTopN, levels = pullTopN) # Setting that vector to a factor with levels = desc(topN)
  
  tDat_topN <- tDat %>%
    filter(causeName %in% pullTopN) %>% # filtering our conditions
    mutate(causeName = factor(causeName, levels = rev(pullTopN))) # setting conditions to a factor in REVERSE order of the original levels
  
  # Plot set up ------------------------------------------------------------------------------------
  
  myTitle <- paste0(myData," by Cause by ", myStrata, ", ", myYearG3, " in ", myCounty)
  #myTitle <- wrap.labels(myTitle, 50)
  
  mySubTitle <- paste0("Ordered by: ", mySort) 
  myXTitle <- "Cause"
  myYTitle <- "Hmmmm"
  
 # if(!myFillManual) tDat_topN$topLevName <- NULL
  
  # Plot --------------------------------------------------------------------------------------------
  
 
  plotX  <-   ggplot(data=tDat_topN, aes(x = causeName, y = measure, fill=topLevName)) +   #
    geom_bar(stat = "identity")   +
    coord_flip() + 
    facet_grid(~ MAINSTRATA, scales = myScale, labeller=labeller(type = label_wrap_gen(5)))  +
    theme_bw() + 
    scale_y_continuous(labels = scales::comma) + # numbers shown with commas
    scale_x_discrete(labels = scales::wrap_format(40))   + #x-axis is condition label--wrapping text so it stacks on top of each other
    labs(title = myTitle, subtitle = mySubTitle, x = myXTitle,y = myYTitle) +
    theme(plot.title = element_text(size=myTextSize, color="blue"),
          plot.subtitle = element_text(size=myTextSize, color="blue"),
          axis.text.x = element_text(angle = 90, hjust = 1,size = myTextSize-6),
          axis.text.y = element_text(size = myTextSize), #increases size of disease condition labels
          strip.text.x = element_text(size = myTextSize),
          axis.title=element_text(size=myTextSize,face="bold"),
          legend.text = element_text(size=myTextSize),
          legend.title = element_blank() ) 
  
  # 
  if(myFillManual) {
    topLevColors <- topLevColors[names(topLevColors) %in% unique(tDat$topLevName)]
    plotX <- plotX + scale_fill_manual(values = topLevColors)
  } else {
    plotX <- plotX + geom_bar(stat = "identity", fill = "blue")
  }
  
  #ggplotly(plotX, height = 750) %>% layout(autosize = TRUE, margin = list(l = 0, r = 0, b = 0, t = 100, pad = 0))
 
  
  list(plot= plotX)
  
  
}

