# -- This chunk is here for loading, and testing the function, but will be removed after finalizing the function ---------------------------
# library(sqldf)
# library(dplyr)
# library(readxl)
# library(shiny)
# library(fs)
# library(stringr)
# library(ggplot2)
# library(sqldf)
# library(RColorBrewer)
# library(plotly)
# 
# myTextSize <- 18
# 
# myPlace <- "/mnt/projects/CCB/CCB Project/0.CCB/myCBD/"
# 
# topLev <- c("Communicable","Cancer","Cardiovascular","Other Chronic","Injury","Ill-Defined","Birth","Other" )
# topLevColors        <- brewer.pal(n = length(topLev), name = "Dark2")
# names(topLevColors) <- topLev
# 
# deaths_age_stn <- readRDS(paste0(myPlace, "myData/real/age_race_focus_data/deaths_age_stn.RDS"))
# deaths_race_stn <- readRDS(paste0(myPlace, "myData/real/age_race_focus_data/deaths_race_stn.RDS"))
# ed_age_stn <- readRDS(paste0(myPlace, "myData/real/age_race_focus_data/ed_age_stn.RDS"))
# ed_race_stn <- readRDS(paste0(myPlace, "myData/real/age_race_focus_data/ed_race_stn.RDS"))
# hospital_age_stn <- readRDS(paste0(myPlace, "myData/real/age_race_focus_data/hospital_age_stn.RDS"))
# hospital_race_stn <- readRDS(paste0(myPlace, "myData/real/age_race_focus_data/hospital_race_stn.RDS"))

# ------------------------------------------------------------------------------------------------------------------


if(1==2) {
  myDataSet     = "deaths_race_stn" # leave it in for now
  myData        = "Deaths"   # "Hospitalizations", "Emergency Department".. will be part of  title
  myMAINSTRATA  = "raceCode" # "raceCode" , "sex".. for title
  mySort        = "White"
  myCounty      = "Los Angeles"
  myMeasure     = "Ndeaths"           # n_hosp... not in dataset yet
  mySex         = "Total"
  myN           = 15
  myYearG3      = "2016-2018" # no year in dataset
  myScale       = "fixed"
  myFillManual  = T
}

makePlotRank <- function(myDataSet     = "hospital_age_stn", # leave it in for now
                         myData        = "Hospitalizations",   # "Hospitalizations", "Emergency Department".. will be part of  title
                         myMAINSTRATA  = "raceCode", # "raceCode" , "sex".. for title
                         mySort        = "Black",
                         myCounty      = "Los Angeles",
                         myMeasure     = "Ndeaths",           # n_hosp
                         mySex         = "Total",
                         myN           = 10,
                         myYearG3      = "2016-2018", # no year in dataset
                         myScale       = "fixed",
                         myFillManual  = T # Set true if the bars should be filled by topLev (which will then have a legend). Setting to F defaults to blue bars
){
  
  tDat <- get(myDataSet) %>%
    filter(county == myCounty)
  
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
  
  myTitle <- paste0(myData, " by Cause by ", myMAINSTRATA, ", ", myYearG3, " in ", myCounty, " county")
  mySubTitle <- paste0("Ordered by: ", mySort, " ", myMAINSTRATA) 
  myXTitle <- "Cause"
  myYTitle <- myData
  
  if(!myFillManual) tDat_topN$topLev <- NULL
  
  # Plot --------------------------------------------------------------------------------------------
  
  plotX  <-   ggplot(tDat_topN, aes(x = causeName, y = measure, fill=tDat_topN$topLev)) +
    geom_bar(stat = "identity") +
    coord_flip() + 
    facet_grid(~ MAINSTRATA, scales = myScale, labeller=labeller(type = label_wrap_gen(5))) +
    theme_bw() + 
    scale_y_continuous(labels = scales::comma) + # numbers shown with commas
    scale_x_discrete(labels = scales::wrap_format(30)) + #x-axis is condition label--wrapping text so it stacks on top of each other
    labs(title = myTitle, subtitle = mySubTitle, x = myXTitle,y = myYTitle) +
    theme(plot.title = element_text(size=myTextSize, color="blue"),
          plot.subtitle = element_text(size=myTextSize, color="blue"),
          axis.text.x = element_text(angle = 90, hjust = 1,size = myTextSize-6),
          axis.text.y = element_text(size = myTextSize), #increases size of disease condition labels
          strip.text.x = element_text(size = myTextSize),
          axis.title=element_text(size=myTextSize,face="bold"),
          legend.text = element_text(size=myTextSize),
          legend.title = element_blank() ) 
  if(myFillManual) {
    #topLevColors <- topLevColors[names(topLevColors) %in% unique(tDat$topLev)]
    plotX <- plotX + scale_fill_manual(values = topLevColors)
  } else {
    plotX <- plotX + geom_bar(stat = "identity", fill = "blue")
  }
  
  #ggplotly(plotX, height = 750) %>% layout(autosize = TRUE, margin = list(l = 0, r = 0, b = 0, t = 100, pad = 0))
  plotX
}

# makePlotRank(myDataSet     = "hospital_race_stn", # leave it in for now
#              myData        = "Hospitalizations",   # "Hospitalizations", "Emergency Department".. will be part of  title
#              myMAINSTRATA  = "raceCode", # "raceCode" , "sex".. for title
#              mySort        = "Black",
#              myCounty      = "Los Angeles",
#              myMeasure     = "Ndeaths",           # n_hosp
#              mySex         = "Total",
#              myN           = 10,
#              myYearG3      = "2016-2018", # no year in dataset
#              myScale       = "fixed",
#              myFillManual  = T)

