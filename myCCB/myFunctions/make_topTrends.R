if (1 == 2) {
  myLHJ = "CALIFORNIA"
  myMeasure = "aRate"
  myLogTrans = FALSE
  myN = 5
  myLev = "lev2" 
  myBroad = c("0")
}


topCauses_trends <- function(
  myLHJ = "CALIFORNIA", 
  myMeasure = "aRate", 
  myLogTrans = FALSE,
  myN = 5,
  myLev = "lev2", 
  myBroad = c("0"), 
  myYearRange = c(2000, 2020), 
  myYearRank = 2020
) {
  
  if (myLev == "lev2" & is.null(myBroad)) stop("Please select at least one Broad Condition Group")
  
  
  # PREPARE DATASET ------------------------------------------------------------------------------------
  plot_df <- datCounty %>%
    filter(county == myLHJ, Level == myLev, sex == "Total") %>%
    left_join(select(deathCauseLink, causeCode, causeName, causeNameShort, topLevCode, topLevName), by = "causeCode") %>%
    filter(topLevCode != "Z") %>%
    mutate(measure = !!sym(myMeasure), 
           causeNameShort_left_dl = ifelse(causeNameShort == "COVID-19", NA, causeNameShort)) %>%
    select(year, Level, county, causeNameShort, causeNameShort_left_dl, topLevCode, topLevName, measure)
  
  
  # Year Range ==========================================================================================
  yearRange <- min(myYearRange):max(myYearRange)
  
  # Expand (for Top PH trends - one group selected) =====================================================
  myExpand <- min(6, length(yearRange) / 2)
  
  # Y-Axis Measure Labels -------------------------------------------------------------------------------
  y_title <- names(deathMeasures_Dropdown[deathMeasures_Dropdown == myMeasure])
  
  deathMeasures_Dropdown_short <- setNames(
    c("Ndeaths", "cDeathRate", "aRate", "YLL", "YLLper", "YLL.adj.rate", "mean.age"), 
    c("N deaths", "Crude Rate", "Adj. Rate", "YLL", "YLL Rate", "Adj. YLL Rate", "Mean Age")
  )
  
  y_title_short <- names(deathMeasures_Dropdown_short[deathMeasures_Dropdown_short == myMeasure])

  # If top level is chosen - one trend chart
  if (myLev == "lev1") {
    
    if (nrow(plot_df)==0) stop("Sorry friend, data are suppressed per the California Health and Human Services Agency Data De-Identification Guidelines, or there are no cases that meet this criteria.")
    
    tPlot <- ggplot(filter(plot_df, year %in% yearRange), aes(x = year, y = measure, color = topLevName)) +
      scale_color_manual(values = topLevColors, drop = T, limits = force) +
      scale_x_continuous(minor_breaks = yearRange, breaks = yearRange, labels = yearRange) +
      geom_line(size = 1) +
      labs(x = "Year", y = y_title, color = "Cause", title = paste0("Trends in Broad Condition Groups in ", myLHJ, " - ", y_title, ", ", min(myYearRange), "-", max(myYearRange))) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
    if (myLogTrans) tPlot <- tPlot + scale_y_continuous(trans = 'log2')
    
  } else if (myLev == "lev2" & length(myBroad) > 1) {
    
    
    plotList <- lapply(myBroad, function(x) {
      
      if (x == "0") tDat <- plot_df else tDat <- plot_df %>% filter(topLevCode == x)
      
      topCauses <- tDat %>%
        filter(year == myYearRank) %>%
        arrange( desc(measure) ) %>%
        dplyr::slice(1:myN) %>%
        pull(causeNameShort)
      
      tDat <- tDat %>% filter(causeNameShort %in% topCauses, year %in% yearRange)
      
      if (x == "0") plot_title <- "All Groups" else plot_title <- unique(tDat$topLevName)
      
      
      tPlot0 <- ggplot(tDat, aes(x = year, y = measure, color = causeNameShort)) +
        geom_line(size = 1) +
        geom_point() +
        labs(x = "Year", y = y_title_short, color = "Cause", title = plot_title) +
        scale_colour_discrete(labels = function(x) str_wrap(x, width = 20)) +
        theme(axis.text = element_text(size = myTextSize3),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              axis.title = element_text(size = myAxisTitleSize - 4, face = "bold"),
              legend.text = element_text(size = 12), 
              legend.title = element_blank())
      
      if (myLogTrans) tPlot0 <- tPlot0 + scale_y_continuous(trans='log2')
      
      tPlot0
      
    })
    
    tPlot <- cowplot::plot_grid(plotlist = plotList, ncol = 2)
    
    myLHJ_title <- ifelse(myLHJ == "CALIFORNIA", myLHJ, paste0(myLHJ, " County"))
    myTitle <- paste0("Trends In Top Public Health Conditions (by Broad Condition Groups) in ",
                      myLHJ_title, ", ", myYearRange[1], "-", myYearRange[2])
    
    tPlot_title <- ggdraw() + 
      draw_label(
        myTitle,
        fontface = 'bold',
        x = 0.5,
        #hjust = 0, 
        size = 20
      ) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 7)
      )
    
    tPlot <- cowplot::plot_grid(
      tPlot_title, tPlot,
      ncol = 1,
      # rel_heights values control vertical title margins
      rel_heights = c(0.1, 1)
    )
    
    
  } else {
    
    if (myBroad == "0") tDat <- plot_df else tDat <- plot_df %>% filter(topLevCode == myBroad)
    
    topCauses <- tDat %>%
      filter(year == myYearRank) %>%
      arrange( desc(measure) ) %>%
      dplyr::slice(1:myN) %>%
      pull(causeNameShort)
    
    tDat <- tDat %>% filter(causeNameShort %in% topCauses, year %in% yearRange)
    
    if (nrow(tDat)==0) stop("Sorry friend, data are suppressed per the California Health and Human Services Agency Data De-Identification Guidelines, or there are no cases that meet this criteria.")

    if (myBroad == "0") plot_title <- paste0(min(myYearRange), "-", max(myYearRange), " Trends in Top ", myN, " Public Health Level Conditions (based on ", y_title, " in ", myYearRank, "), ", myLHJ)
    if (myBroad != "0") plot_title <- paste0(min(myYearRange), "-", max(myYearRange), " Trends in Top ", myN, " ", unique(tDat$topLevName), " Conditions (based on ", y_title, " in ", myYearRank, "), ", myLHJ)
    
    tPlot <- ggplot(tDat, aes(x = year, y = measure, color = causeNameShort) )  +
      geom_line(size = 1) +
      geom_point() +
      labs(x = "Year", y = y_title, color = "Cause", title = plot_title) +
      scale_x_continuous(minor_breaks = yearRange, breaks = yearRange, expand = c(0, myExpand), labels = yearRange) +
      scale_colour_discrete(guide = 'none') +   # removed legend
      geom_dl(aes(label = causeNameShort), method = list(dl.trans(x = x + .1), "last.points", cex = 1.3, 'last.bumpup')) +   # , 'last.bumpup'
      geom_dl(aes(label = causeNameShort_left_dl), method = list(dl.trans(x = x - .1), "first.points", cex = 1.3, 'first.bumpup', hjust = 1)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
    if (myLogTrans) tPlot <- tPlot + scale_y_continuous(trans='log2')
    
    
  }
  


  # PREPARE PLOT FEATURES ------------------------------------------------
  # level_title <- ifelse(myLev == "lev2", "Top Public Health Level Conditions", "Broad Condition Groups")
  # measure_title <- deathMeasuresNames[deathMeasures == myMeasure]
  # if (myLogTrans) measure_title <- paste0(measure_title, " (log-scaled) ")
  # 
  # plot_title <- paste0( "Trends in ", level_title, " - ", 
  #                       measure_title, 
  #                       ", 2000-", maxYear)
  
  

  
  
  
  list(plotL = tPlot)
  
  
}


# topCauses_trends(
#   myLHJ = "CALIFORNIA", 
#   myMeasure = "aRate", 
#   myLogTrans = FALSE,
#   myN = 15,
#   myLev = "lev1", 
#   myBroad = c("0")
# )

