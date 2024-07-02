# Documentation ===========================================================================================================

makeIHME_chart <- function(
  myFacet, # Options: Sex, Age Group, Year
  myMeasure, 
  myMetric,
  myYear,
  myType, # Options: "Cause" or "Risk"
  myLevel,
  myN,
  myScales
) {
  
  if (F) {
    myFacet <- "Year"
    myMeasure <- "YLDs (Years Lived with Disability)" 
    myMetric <- "Number"
    myYear <- 2019
    myType <- "Cause"
    myLevel <- 2
    myN <- 10
    myScales <- "fixed"
  }
  
  myN <- as.numeric(myN)
  
  # Cause or Risk Data ====================
  if (myType == "Cause") {
    ihmeData <- ihmeCause
  } else if (myType == "Risk") {
    ihmeData <- ihmeRisk
  }
  
  # Facets =======================
  if (myFacet == "Sex") {
    facetCol <- "sex_name"
    mySex <- c("Female", "Male")
    myAge <- "All ages"
  } else if (myFacet == "Age Group") {
    facetCol <- "age_name"
    mySex <- "Both"
    myAge <- ihmeLink$age$age_name
    myAge <- myAge[myAge != "All ages"]
  } else if (myFacet == "Year") {
    facetCol <- "year"
    mySex <- "Both"
    myAge <- "All ages"
    myYear <- ihmeYears
  }
  
  
  
  # Filter data ====================
  tDat <- ihmeData %>% 
    filter(measure_name == myMeasure, metric_name == myMetric, level == paste0("lev", myLevel),
           sex_name %in% mySex, age_name %in% myAge, year %in% myYear)%>%
    rename(facet = !!as.symbol(facetCol)) %>% 
    slice_max(val, by = facet, n = myN)
  
  # Color palette ====================
  uniqueCauses <- unique(tDat$cause_name)
  
  tColors <- c(
    brewer.pal(n = 12, "Set3"),
    brewer.pal(n = 8, "Set2")
  )
  tColors <- rep(tColors, 20)

  tColors <- setNames(tColors[1:length(uniqueCauses)], uniqueCauses)
  
  
  if (myFacet == "Age Group") {
    tDat <- tDat %>% 
      mutate(facet = factor(facet, levels = myAge))
  }
  
  # Create chart =====================
  
  # Configure x-axis scale
  myScales <- ifelse(myScales == "fixed", "free_y", "free")
  
  # Title
  tMeasure <- case_when(myMeasure == "YLDs (Years Lived with Disability)" ~ "Years Lived with Disability", 
                        myMeasure == "DALYs (Disability-Adjusted Life Years)" ~ "Disability-Adjusted Life Years", 
                        myMeasure == "Prevalence" ~ "Prevalence")
  
  title_main <- paste0("Ranking of Causes based on Associated ", tMeasure, " by ", myFacet)
  
  title_main <- ifelse(myFacet == "Year", title_main, paste0(title_main, ", ", myYear))
  
  # X-Axis Title
  tMeasure <- case_when(myMeasure == "YLDs (Years Lived with Disability)" ~ "YLD", 
                        myMeasure == "DALYs (Disability-Adjusted Life Years)" ~ "DALY", 
                        myMeasure == "Prevalence" ~ "Prevalence")
  
  title_xaxis <- case_when(myMetric == "Number" ~ paste0("Number of ", tMeasure, "s"), 
                           myMetric == "Rate" ~ paste0(tMeasure, " Rate"))
  
  # Label size
  if (myN == 5) {
    myConditionSize <- 7
  } else if (myN == 10) {
    myConditionSize <- ifelse(myFacet == "Sex", 7, 6)
  } else if (myN == 15) {
    myConditionSize <- ifelse(myFacet == "Sex", 7, 5.5)
  }
  
  tPlot <- ggplot(tDat, aes(x = val, y = reorder_within(cause_name, val, facet), label = cause_name)) +
    geom_col(aes(fill = cause_name)) +
    scale_y_reordered() +
    scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    labs(x = title_xaxis, y = myType, title = title_main) +
    geom_text(x = 0, hjust = 0, size = myConditionSize) +
    scale_fill_manual(values = tColors) +
    ggh4x::facet_nested_wrap(facets = vars(facet), scales = myScales) +
    theme(axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          legend.position = "none")
  
  list(plotL = tPlot)
  
  
}

# Either recent year or select year when comparing sex, age, total

# makeIHME_chart(
#   myFacet = "Sex",
#   myMeasure = "YLDs (Years Lived with Disability)",
#   myMetric = "Rate",
#   myYear = c(2021),
#   myLevel = 2,
#   myN = 10,
#   myScales = "fixed"
# )

