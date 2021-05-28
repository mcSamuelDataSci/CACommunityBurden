# -- This chunk is here for loading, and testing the function, but will be removed after finalizing the function ---------------------------
# library(dplyr)
# library(ggplot2)

# ------------------------------------------------------------------------------------------------------------------

## FIX yearG3

raceSort <- raceLink %>%
  filter(!raceCode %in% c("Other", "Unknown", "Total")) %>%
  pull(raceCode)

ageSort  <- ageLink$ageName

makePlotRank <- function(myDataSet     = NA,
                         myData        = "Deaths",   # "Hospitalizations", "Emergency Department".. will be part of  title
                         myStrata      = "Race/Ethnicity", # "Race/Ethnicity", # "Race/Ethnicity" , "sex".. for title
                         mySort        = "White",
                         myCounty      = "Los Angeles",
                         myMeasure     = "N", 
                         mySex         = "Total",
                         myN           = 10,
                         myOlderFocus    = TRUE,
                       #  myYearG3      = "2017-2019", # no year in dataset
                         myScale       = "free",
                         myLiveborn    = TRUE,
                         myFillManual  = T # Set true if the bars should be filled by topLev (which will then have a legend). Setting to F defaults to blue bars
){
  
  
  
  if  ( myData %in% c("Hospitalizations","Emergency Department") & 
        (myStrata == "Race/Ethnicity") & 
        (mySort %in% c("NHPI", "Multi") )
        ) stop("Sorry friend, there is no NHPI or Multirace in Hospitalization or ED data -- kindly select another race/ethnic group")
  
  
  
  
  
  
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
 
  
  if (myStrata == "Age Group" & myOlderFocus) myDataSet <- filter(myDataSet, MAINSTRATA >= "55 - 64")
  
  # if (myStrata == "Race/Ethnicity") {
  #   myDataSet <- myDataSet %>% 
  #     left_join(select(raceLink, raceCode, raceNameShort), by = c("MAINSTRATA" = "raceCode")) %>%
  #     select(-MAINSTRATA, MAINSTRATA = raceNameShort)
  # }
  
  # mutate(myDataSet,aRate = ifelse(N==0,0,aRate))
  
  
   tDat <- myDataSet %>%
    filter(county == myCounty, !MAINSTRATA %in% c("Unknown", "Other")) %>%
      mutate(measure = get(myMeasure))
   
   # Return message if no rows exist
   if (nrow(tDat)==0) stop("Sorry friend, but thank goodness there are none of those OR all data are suppressed because of small numbers")
   
   if(!myLiveborn) tDat <- filter(tDat, causeName != "Liveborn")
   
   
   
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
    pull(causeNameShort)
  
  pullTopN <- factor(pullTopN, levels = pullTopN) # Setting that vector to a factor with levels = desc(topN)
  
  tDat_topN <- tDat %>%
    filter(causeNameShort %in% pullTopN) %>% # filtering our conditions
    mutate(causeNameShort = factor(causeNameShort, levels = rev(pullTopN))) # setting conditions to a factor in REVERSE order of the original levels
  
  
  
  
  # Plot set up ------------------------------------------------------------------------------------
  
  myTitle <- paste0(myData," by Cause by ", myStrata, ", ", yearGrp_hosp_ed_deaths)
  #myTitle <- wrap.labels(myTitle, 50)
  
  mySubTitle <- paste0("In ", myCounty, ", Ordered by: ", mySort) 
  myXTitle <- "Cause"
  if (myMeasure == "cRate") myYTitle <- "Crude Rate"
  if (myMeasure != "cRate") myYTitle <- myMeasure
  if (myScale == "free") myYTitle <- paste(myMeasure, "(NOTE axis scales differ)")
  
 # if(!myFillManual) tDat_topN$topLevName <- NULL
  
  # Plot --------------------------------------------------------------------------------------------
  
 plotX  <-   ggplot(data=tDat_topN, aes(x = causeNameShort, y = measure, fill=topLevName)) +   #
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
    #topLevColors <- topLevColors[names(topLevColors) %in% unique(tDat$topLevName)]
    plotX <- plotX + scale_fill_manual(values = topLevColors)
  } else {
    plotX <- plotX + geom_bar(stat = "identity", fill = "blue")
  }
  
  #ggplotly(plotX, height = 750) %>% layout(autosize = TRUE, margin = list(l = 0, r = 0, b = 0, t = 100, pad = 0))
 
  
  
   tDat_topN <- tDat_topN %>% filter(N > 0)  %>% mutate(MAINSTRATA = paste(" ",MAINSTRATA),
                                                        cRate = round(cRate,1),
                                                        measure = round(measure,1))  %>% 
                               select(county, MAINSTRATA, causeName, causeNameShort, measure) %>%
     rename(!!myMeasure:=measure)
  
   #list(plotL= plotX, dataL=dataTable, loadData = tDat_topN)
   list(plotL= plotX, dataL=tDat_topN)
  
  
}






# ========================================================================================================
# ========================================================================================================

# t1 <- 5
# t2 <- 10




deathHospEDchart <- function(myStrata = "Age Group", mySort = "85+", myCounty = "Los Angeles", myMeasure = "cRate") {
  
  t.chart <- function(dataSet,source, legend =FALSE, myTopN = 10) {
    
    t.dat   <-  dataSet %>% 
      mutate(measure = get(myMeasure)) %>%
      filter(MAINSTRATA %in% mySort, county == myCounty) %>% 
      arrange(-measure) %>% 
      slice(1:myTopN)
    
    if (myMeasure == "cRate") myYTitle <- "Crude Rate"
    if (myMeasure != "cRate") myYTitle <- myMeasure
    
    tPlot <- ggplot(data=t.dat, aes(x=reorder(causeNameShort,measure), y=measure, fill=topLevName)) + 
      geom_bar(stat="identity") + coord_flip() + 
      labs(title=source,x="",y = str_wrap(myYTitle, width = 15)) +  
      scale_x_discrete(labels = wrap_format(20)) +
      # scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
      theme(legend.position="bottom", 
            legend.title = element_blank(),
            plot.title = element_text(size=myTextSize, color="blue"),
            axis.title   = element_text(size = myAxisTitleSize, face="bold"),
            axis.text.y  = element_text(size = myAxisTextSize),
            axis.text.x  = element_text(size = myAxisTextSize, angle = 90, vjust = 0.5, hjust=1)) + 
      scale_fill_manual(values = topLevColors) 
    
    if(!legend) tPlot <- tPlot + theme(legend.position = "none")
    
    # tPlot
    
   list(plotL = tPlot, dataL = t.dat)
  }
  
  if (myStrata == "Age Group") {
    d1 <- death_age
    d2 <- hosp_age
    d3 <- ed_age }
  
  
  if (myStrata == "Race/Ethnicity") {
    d1 <- death_race
    d2 <- hosp_race
    d3 <- ed_race }
  
  # Call functions to get plot and data
  c.1 <- t.chart(d1,"Deaths")
  c.2 <- t.chart(d2,"Hospitalizations")
  c.3 <- t.chart(d3,"ED Visits")
  
  # For download data
  d.1 <- c.1$dataL %>% mutate(dataType = "Deaths")
  d.2 <- c.2$dataL %>% mutate(dataType = "Hospitalizations")
  d.3 <- c.3$dataL %>% mutate(dataType = "ED Visits")
  
  df <- bind_rows(d.1, d.2, d.3) %>%
    select(year = yearG3, county, MAINSTRATA, causeName, dataType, !!myMeasure) %>%
    mutate(year = ifelse(is.na(year), "2019", "2017-2019")) # FIX THIS upstream
  
  
  # https://wilkelab.org/cowplot/articles/drawing_with_on_plots.html
  # https://wilkelab.org/cowplot/articles/plot_grid.html
  
  r1       <- plot_grid(c.1$plotL,c.2$plotL,c.3$plotL,nrow=1)
  c.legend <- get_legend(t.chart(d2,"Hospitalizations", myTopN = 100, legend = TRUE)$plotL)
  # c.legend <- get_legend(t.chart(hosp_age,"Deaths",myTopN = 100,legend=TRUE))
  title    <- ggdraw() + draw_label(paste0("Leading Causes of Death, Hospitalization, and ED Visits for ",mySort," ",myStrata," in ",myCounty,": ",yearGrp_hosp_ed_deaths),size=myTextSize, color="blue") 
  
  pPlot <- plot_grid(title,r1,c.legend,nrow=3,rel_heights = c(.2,1,.25))
  
  
  
  list(plotL= pPlot, dataL = df) 
  
}







if(1==2) {
  myDataSet     = "hosp_race" # leave it in for now
  myData        = "Deaths"   # "Hospitalizations", "Emergency Department".. will be part of  title
  myStrata  = "Race/Ethnicity" # "Race/Ethnicity" , "sex".. for title
  mySort        = "Black"
  myCounty      = "CALIFORNIA"
  myMeasure     = "cRate"           # n_hosp
  mySex         = "Total"
  myN           = 10
  myYearG3      = "2016-2018" # no year in dataset
  myScale       = "fixed"
  myFillManual  = T
  
  myData = "Hospitalizations"
  myStrata = "Age Group"
  mySort        = "15 - 24"
  myOlderFocus = FALSE
  myLiveborn   = TRUE
}

