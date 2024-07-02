## mono space font to keep the same label width
font_add(family = "roboto mono", regular = paste0(ccbInfo, "/RobotoMono/static/RobotoMono-Regular.ttf"))



## Prepare data - datLifeCourse ----
if (FALSE) {

  myN = 10  
  
  ### ----- 1-year data -----
  lifeCourse_data <- left_join(datLifeCourse, select(raceLink, raceCode, raceNameShort), by = "raceCode")
  lifeCourse_data <- left_join(lifeCourse_data, select(deathCauseLink, causeCode, causeNameShort, topLevName), by = "causeCode")

  regions <- c("RURAL NORTH", "BAY AREA", "CENTRAL CALIFORNIA", "GREATER SIERRA SACRAMENTO", "LOS ANGELES COUNTY", "SOUTHERN CALIFORNIA (No LA)")
  
  lifeCourse_data <- lifeCourse_data %>%
    filter(!grepl("Z", causeCode), !county %in% regions) %>% 
    mutate(ageGroup = factor(ageGroup, levels = ageLink$ageName), 
           causeNameShort = ifelse(causeNameShort == "Alzheimer’s disease", "Alzheimer's disease", causeNameShort),
           causeNameShort_rate = case_when(causeNameShort == "Other neurological conditions" ~ "Other neurological",
                                           causeNameShort == "Other Infections/Nutritional" ~ "Other Infections or Nutrition",
                                           causeNameShort == "Endocrine, blood, immune disorders" ~ "Endo., blood, immune dis.",
                                           causeNameShort == "Alzheimer’s disease" ~ "Alzheimer's",  # shortened for chart labels
                                           causeNameShort == "Diabetes mellitus" ~ "Diabetes",  # shortened for chart labels
                                           causeNameShort == "Congestive heart failure" ~ "Congestive HF",  # shortened for chart labels
                                           causeNameShort == "Parkinson's disease" ~ "Parkinson's",  # shortened for chart labels
                                           causeNameShort == "Other malignant neoplasms" ~ "Other cancers",  # shortened for chart labels
                                           causeNameShort == "Other unintentional injuries" ~ "Other injuries", # shortened for chart labels
                                           TRUE ~ causeNameShort),
           causeNameShort_rate = gsub("and", "&", causeNameShort_rate),
           causeNameShort_rate = gsub("heart disease", "HD", causeNameShort_rate), # shortened for chart labels
           causeNameShort_rate = gsub("system", "sys.", causeNameShort_rate),  # shortened for chart labels
           causeTrunc = str_trunc(causeNameShort_rate, width = 15, side = "right"), # Truncate longer cause names
           causePadRate = str_pad(causeTrunc, width = 15, side = "right", pad = " ") # Pad cause names to be the same width
           ) %>% 
    select(year, county, raceNameShort, sex, ageGroup, causeNameShort, causePadRate, Ndeaths, cDeathRate, topLevName) %>%
    group_by(ageGroup, sex, raceNameShort, year, county) %>%
    mutate(Ranking = rank(-cDeathRate, ties.method = "first")) %>%
    ungroup() %>%
    filter(Ranking %in% 1:myN) %>%
    arrange(year, county, raceNameShort, sex, ageGroup, Ranking)
  
  
  # Replicate each row 4 times for ranking chart's polygon coordinates
  lifeCourse_data4 <- cbind(lifeCourse_data, temp = rep(row.names(lifeCourse_data), each = 4)) %>% 
    arrange(year, county, raceNameShort, sex, ageGroup, Ranking) %>% 
    select(-temp)
  
  lifeCourse_data4 <- tibble::rowid_to_column(lifeCourse_data4, "row_n")
  
  polygon_margin <- 0.02
  
  ## Create x and Y coordinates
  lifeCourse_data4 <- lifeCourse_data4 %>% 
    mutate(row_n4 = ifelse(row_n%%4 == 0, 4, row_n%%4), 
           ageRank = paste(ageGroup, " ", Ranking),
           polygonXpre = case_when(
             ageGroup == "0 - 4" ~ 1,
             ageGroup == "5 - 14" ~ 2, 
             ageGroup == "15 - 24" ~ 3,
             ageGroup == "25 - 34" ~ 4,
             ageGroup == "35 - 44" ~ 5,
             ageGroup == "45 - 54" ~ 6,
             ageGroup == "55 - 64" ~ 7, 
             ageGroup == "65 - 74" ~ 8,
             ageGroup == "75 - 84" ~ 9, 
             ageGroup == "85+" ~ 10
           ), 
           polygonX = ifelse(row_n4 < 3, polygonXpre - (1 - polygon_margin), polygonXpre), 
           polygonY = ifelse(row_n4 == 1 | row_n4 == 4, Ranking - (1 - polygon_margin), Ranking))
  
  
  saveRDS(lifeCourse_data4, path(ccbData, whichData, "datLifeCourse1y.RDS"))


  ### ----- 5-year data -----
  lifeCourse5y_data <- left_join(datLifeCourse_5y, select(raceLink, raceCode, raceNameShort), by = "raceCode")
  lifeCourse5y_data <- left_join(lifeCourse5y_data, select(deathCauseLink, causeCode, causeNameShort, topLevName), by = "causeCode")
  
  
  lifeCourse5y_data <- lifeCourse5y_data %>%
    rename(year = yearG5) %>% 
    filter(!grepl("Z", causeCode), !county %in% regions) %>% 
    mutate(ageGroup = factor(ageGroup, levels = ageLink$ageName), 
           causeNameShort = ifelse(causeNameShort == "Alzheimer’s disease", "Alzheimer's disease", causeNameShort),
           causeNameShort_rate = case_when(causeNameShort == "Other neurological conditions" ~ "Other neurological",
                                           causeNameShort == "Other Infections/Nutritional" ~ "Other Infections or Nutrition",
                                           causeNameShort == "Endocrine, blood, immune disorders" ~ "Endo., blood, immune dis.",
                                           causeNameShort == "Alzheimer’s disease" ~ "Alzheimer's",  # shortened for chart labels
                                           causeNameShort == "Diabetes mellitus" ~ "Diabetes",  # shortened for chart labels
                                           causeNameShort == "Congestive heart failure" ~ "Congestive HF",  # shortened for chart labels
                                           causeNameShort == "Parkinson's disease" ~ "Parkinson's",  # shortened for chart labels
                                           causeNameShort == "Other malignant neoplasms" ~ "Other cancers",  # shortened for chart labels
                                           causeNameShort == "Other unintentional injuries" ~ "Other injuries", # shortened for chart labels
                                           TRUE ~ causeNameShort),
           causeNameShort_rate = gsub("and", "&", causeNameShort_rate),
           causeNameShort_rate = gsub("heart disease", "HD", causeNameShort_rate), # shortened for chart labels
           causeNameShort_rate = gsub("system", "sys.", causeNameShort_rate),  # shortened for chart labels
           causeTrunc = str_trunc(causeNameShort_rate, width = 15, side = "right"), # Truncate longer cause names
           causePadRate = str_pad(causeTrunc, width = 15, side = "right", pad = " ") # Pad cause names to be the same width
           ) %>% 
    select(year, county, raceNameShort, sex, ageGroup, causeNameShort, causePadRate, Ndeaths, cDeathRate, topLevName) %>%
    group_by(ageGroup, sex, raceNameShort, year, county) %>%
    mutate(Ranking = rank(-cDeathRate, ties.method = "first")) %>%
    ungroup() %>%
    filter(Ranking %in% 1:myN) %>%
    arrange(year, county, raceNameShort, sex, ageGroup, Ranking)
  
  
  # Replicate each row 4 times for ranking chart's polygon coordinates
  lifeCourse5y_data4 <- cbind(lifeCourse5y_data, temp = rep(row.names(lifeCourse5y_data), each = 4)) %>% 
    arrange(year, county, raceNameShort, sex, ageGroup, Ranking) %>% 
    select(-temp)
  
  lifeCourse5y_data4 <- tibble::rowid_to_column(lifeCourse5y_data4, "row_n")
  
  polygon_margin <- 0.02
  
  ## Create x and Y coordinates
  lifeCourse5y_data4 <- lifeCourse5y_data4 %>% 
    mutate(row_n4 = ifelse(row_n%%4 == 0, 4, row_n%%4), 
           ageRank = paste(ageGroup, " ", Ranking),
           polygonXpre = case_when(
             ageGroup == "0 - 4" ~ 1,
             ageGroup == "5 - 14" ~ 2, 
             ageGroup == "15 - 24" ~ 3,
             ageGroup == "25 - 34" ~ 4,
             ageGroup == "35 - 44" ~ 5,
             ageGroup == "45 - 54" ~ 6,
             ageGroup == "55 - 64" ~ 7, 
             ageGroup == "65 - 74" ~ 8,
             ageGroup == "75 - 84" ~ 9, 
             ageGroup == "85+" ~ 10
           ), 
           polygonX = ifelse(row_n4 < 3, polygonXpre - (1 - polygon_margin), polygonXpre), 
           polygonY = ifelse(row_n4 == 1 | row_n4 == 4, Ranking - (1 - polygon_margin), Ranking))

  
  saveRDS(lifeCourse5y_data4, path(ccbData, whichData, "datLifeCourse5y.RDS"))

}
## ---- datLifeCourse


## Function to create the lifecourse rate chart
makelifeCourseRate_chart <- function(myYear, myYearGrouping, myLHJ, myRace, bySex, myAge, myN, logScale, repel, tab) {
  

  ## select dataset
  if (myYearGrouping == 1) 
    lc_data <- datLifeCourse1y
  else {
    lc_data <- datLifeCourse5y
    myYear <- max(datLifeCourse5y$year) ## 5-yr data only has 2018-2022
  }
  
  
  mainTab <- (tab == "main")
  disparityTab <- (tab == "disparity")
  

  varX <- ifelse(mainTab, "ageGroup", "raceNameShort")

  myforce <- ifelse(repel, 0.1, 0)
  
  facetRace <- length(myRace) > 1
  facetSex <- bySex == "Yes" 
  
  
  myRace_mod <- ifelse(myRace == "Total", "", paste0(" among ", myRace, "s"))
  
  chart_title <- ifelse(mainTab, paste0("Leading Causes of Death Across the Life Course in ", myLHJ, myRace_mod, ", ", myYear), 
                        paste0("Leading Causes of Death by Race/Ethnicity and Age in ", myLHJ, ", ", myYear))
  
  
  y_title <- ifelse(logScale, "Crude Death Rate per 100,000 (log scale)", "Crude Death Rate per 100,000")
  
  
  ## Fill missing age groups that have all causes of death suppressed with NAs
  lc_data <- lc_data %>% 
    filter(row_n4 == 1) %>% 
    complete(year, county, raceNameShort, sex, ageGroup, Ranking, fill = list(causePadRate = NA, causePadRank1 = NA, causePadRank2 = NA, causePadRank3 = NA))
  
  
  lifeCourse_chart_data <- lc_data %>% 
    filter(county == myLHJ, year == myYear, ageGroup %in% myAge, raceNameShort %in% myRace, Ranking <= myN) %>% 
    filter(case_when(
      bySex == "Yes" ~ sex %in% c("Female", "Male"),
      bySex == "No" ~ sex == "Total",
      TRUE ~ sex == "Total"
    )) %>% 
    arrange(ageGroup, sex, -cDeathRate)
  
  #if (nrow(lifeCourse_chart_data)==0) stop("Sorry friend, data are suppressed per the California Health and Human Services Agency Data De-Identification Guidelines, or there are no cases that meet this criteria.")

  
  ## Lifecourse
  lc_chart <- lifeCourse_chart_data %>% 
    ggplot(aes(x = !!as.symbol(varX), y = cDeathRate, fill = topLevName, color = topLevName)) +
    geom_label_repel(aes(label = causePadRate), size = 4.5, force = myforce, max.overlaps = Inf, direction = "y", 
                                 seed = 72, family = "roboto mono", segment.color = "white", label.size = NA) +  # label.size = NA removes label border
    #)), size = 3.5, lineheight = 2, label.size = NA, family = "roboto mono", label.padding = unit(0.5, "lines"))} +
    {if (logScale) scale_y_continuous(trans='log2')} +
    scale_fill_manual(values = topLevColors, breaks = c("Cancer", "Cardiovascular", "Communicable", "Injury", "Other Chronic", "Perinatal")) +
    scale_color_manual(values = c("Other Chronic" = "white", "Communicable" = "white", "Cancer" = "white",
                                  "Perinatal" = "black", "Cardiovascular" = "black", "Injury" = "black"), guide = "none") +
    labs(x = ifelse(mainTab, "Age Group", "Race/Ethnicity"), y = y_title, fill = "", color = "", title = chart_title) +
    {if (mainTab & facetSex) facet_wrap(vars(sex), nrow = 2)} +
    {if (disparityTab & !facetSex) facet_wrap(vars(ageGroup), nrow = 1)} +
    {if (disparityTab & facetSex) facet_grid(rows = vars(sex), cols = vars(ageGroup))} +
    theme_bw() +
    theme(legend.position = "bottom", 
          legend.text = element_text(size = 15),
          legend.key.size = unit(1, 'cm'),
          axis.text = element_text(face = "bold", size = 16, color = "black"), 
          axis.title = element_text(size = 17), 
          plot.title = element_text(hjust = 0.5, size = 17), 
          strip.text = element_text(face = "bold", size = 16)) +
    guides(fill = guide_legend(override.aes = aes(label = ""), nrow = 1))
  
  
  ## chart for download - need to reduce text size to match the size that appear in the app
  lc_download_chart <- lifeCourse_chart_data %>% 
    ggplot(aes(x = !!as.symbol(varX), y = cDeathRate, fill = topLevName, color = topLevName)) +
    geom_label_repel(aes(label = causePadRate), size = 3.5, force = myforce, max.overlaps = Inf, direction = "y", # size = 3.5
                     seed = 72, family = "roboto mono", segment.color = "white", label.size = NA) +  # label.size = NA removes label border
    {if (logScale) scale_y_continuous(trans='log2')} +
    scale_fill_manual(values = topLevColors, breaks = c("Cancer", "Cardiovascular", "Communicable", "Injury", "Other Chronic", "Perinatal")) +
    scale_color_manual(values = c("Other Chronic" = "white", "Communicable" = "white", "Cancer" = "white",
                                  "Perinatal" = "black", "Cardiovascular" = "black", "Injury" = "black"), guide = "none") +
    labs(x = ifelse(mainTab, "Age Group", "Race/Ethnicity"), y = y_title, fill = "", color = "", title = chart_title) +
    {if (mainTab & facetSex) facet_wrap(vars(sex), nrow = 2)} +
    {if (disparityTab & !facetSex) facet_wrap(vars(ageGroup), nrow = 1)} +
    {if (disparityTab & facetSex) facet_grid(rows = vars(sex), cols = vars(ageGroup))} +
    theme_bw() +
    theme(legend.position = "bottom", 
          legend.text = element_text(size = 12),
          legend.key.size = unit(1, 'cm'),
          axis.text = element_text(face = "bold", size = 13, color = "black"), 
          axis.title = element_text(size = 14), 
          plot.title = element_text(hjust = 0.5, size = 14), 
          strip.text = element_text(face = "bold", size = 13)) +
    guides(fill = guide_legend(override.aes = aes(label = ""), nrow = 1))
  
  # Clean up data for download
  lc_data <- lifeCourse_chart_data %>% 
    filter(!is.na(causeNameShort)) %>% 
    arrange(year, county, raceNameShort, sex, ageGroup, Ranking) %>% 
    mutate(ageGroup = paste0(" ", ageGroup)) %>%  # pad space to prevent Excel from automatically converting age group to date
    select(year, county, raceNameShort, sex, ageGroup, causeNameShort, topLevName, Ndeaths, cDeathRate, Ranking)
  
  list(plotL = lc_chart, dataL = lc_data, plotLD = lc_download_chart)
}



## Function to create ranking charts for the life course
makelifeCourseRank_chart <- function(myYear, myYearGrouping, myLHJ, myRace, bySex, myAge, myN, tab) {

  
  ## select dataset
  if (myYearGrouping == 1) 
    lc_data <- datLifeCourse1y
  else {
    lc_data <- datLifeCourse5y
    myYear <- max(datLifeCourse5y$year) ## 5-yr data only has 2018-2022
  }
  
  
  mainTab <- (tab == "main")
  disparityTab <- (tab == "disparity")
  
  varX <- ifelse(mainTab, "ageGroup", "raceNameShort")
  
  facetRace <- length(myRace) > 1
  facetSex <- bySex == "Yes" 
  
  myRace_mod <- ifelse(myRace == "Total", "", paste0(" among ", myRace, "s"))
  
  chart_title <- ifelse(mainTab, paste0("Leading Causes of Death Across the Life Course in ", myLHJ, myRace_mod, ", ", myYear), 
                        paste0("Leading Causes of Death by Race/Ethnicity and Age in ", myLHJ, ", ", myYear))
  
  
  
  lifeCourseRank_chart_data <- lc_data %>% 
    filter(county == myLHJ, year == myYear, ageGroup %in% myAge, raceNameShort %in% myRace, Ranking <= myN) %>% 
    filter(case_when(
      bySex == "Yes" ~ sex %in% c("Female", "Male"),
      bySex == "No" ~ sex == "Total",
      TRUE ~ sex == "Total"
    )) %>% 
    arrange(ageGroup, sex, -cDeathRate)
  
  axis_margin_x <- 0.01
  axis_margin_y <- 0.02
  
  # lower/upper limits for x-axis (ageGroup)
  x_lower_limit <- ifelse(mainTab, 0, min(lifeCourseRank_chart_data$polygonXpre) - 1)
  x_upper_limit <- ifelse(mainTab, 10, max(lifeCourseRank_chart_data$polygonXpre))
  
  #if (nrow(lifeCourseRank_chart_data)==0) stop("Sorry friend, data are suppressed per the California Health and Human Services Agency Data De-Identification Guidelines, or there are no cases that meet this criteria.")
  
  lcRank_plot <- lifeCourseRank_chart_data %>% 
    ggplot() +
    geom_polygon(mapping = aes(x = polygonX, y = polygonY, group = ageRank, fill = topLevName)) +
    geom_text(mapping = aes(x = polygonXpre - 0.5, y = Ranking - 0.5, label = str_wrap(causeNameShort, width = 15), color = topLevName), size = 5, lineheight = 1) +
    scale_x_continuous(breaks = seq(0.5, 9.5, 1), limits = c(x_lower_limit, x_upper_limit), 
                       expand = c(axis_margin_x, axis_margin_x), labels = ageSort, position = "top", sec.axis = dup_axis()) +
    scale_y_reverse(breaks = seq(0.5, myN-0.5, 1), 
                    expand = c(axis_margin_y, axis_margin_y), labels = seq(1, myN, 1)) +
    labs(x = "", y = "Ranking", fill = "", title = chart_title) +
    scale_fill_manual(values = topLevColors, breaks = c("Cancer", "Cardiovascular", "Communicable", "Injury", "Other Chronic", "Perinatal")) +
    scale_color_manual(values = c("Other Chronic" = "white", "Communicable" = "white", "Cancer" = "white",
                                  "Perinatal" = "black", "Cardiovascular" = "black", "Injury" = "black"), guide = "none") +
    {if (mainTab & facetSex) facet_wrap(vars(sex), nrow = 2)} +
    {if (disparityTab & !facetSex) facet_wrap(vars(raceNameShort), nrow = 1)} +
    {if (disparityTab & facetSex) facet_grid(rows = vars(sex), cols = vars(raceNameShort))} +
    theme_bw() +
    theme(legend.position = "bottom", 
          legend.text = element_text(size = 15),
          legend.key.size = unit(1, 'cm'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_text(face = "bold", size = 17, color = "black"), 
          axis.ticks = element_blank(),
          axis.title = element_text(size = 17), 
          plot.title = element_text(hjust = 0.5, size = 17), 
          strip.text = element_text(face = "bold", size = 16)) +
    guides(fill = guide_legend(nrow = 1))
  
  ## chart for download - need to reduce text size to match the size that appear in the app
  lcRank_download_plot <- lifeCourseRank_chart_data %>% 
    ggplot() +
    geom_polygon(mapping = aes(x = polygonX, y = polygonY, group = ageRank, fill = topLevName)) +
    geom_text(mapping = aes(x = polygonXpre - 0.5, y = Ranking - 0.5, label = str_wrap(causeNameShort, width = 15), color = topLevName), size = 4, lineheight = 1) +
    scale_x_continuous(breaks = seq(0.5, 9.5, 1), limits = c(x_lower_limit, x_upper_limit), 
                       expand = c(axis_margin_x, axis_margin_x), labels = ageSort, position = "top", sec.axis = dup_axis()) +
    scale_y_reverse(breaks = seq(0.5, myN-0.5, 1), 
                    expand = c(axis_margin_y, axis_margin_y), labels = seq(1, myN, 1)) +
    labs(x = "", y = "Ranking", fill = "", title = chart_title) +
    scale_fill_manual(values = topLevColors, breaks = c("Cancer", "Cardiovascular", "Communicable", "Injury", "Other Chronic", "Perinatal")) +
    scale_color_manual(values = c("Other Chronic" = "white", "Communicable" = "white", "Cancer" = "white",
                                  "Perinatal" = "black", "Cardiovascular" = "black", "Injury" = "black"), guide = "none") +
    {if (mainTab & facetSex) facet_wrap(vars(sex), nrow = 2)} +
    {if (disparityTab & !facetSex) facet_wrap(vars(raceNameShort), nrow = 1)} +
    {if (disparityTab & facetSex) facet_grid(rows = vars(sex), cols = vars(raceNameShort))} +
    theme_bw() +
    theme(legend.position = "bottom", 
          legend.text = element_text(size = 12),
          legend.key.size = unit(1, 'cm'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text = element_text(face = "bold", size = 14, color = "black"), 
          axis.ticks = element_blank(),
          axis.title = element_text(size = 14), 
          plot.title = element_text(hjust = 0.5, size = 14), 
          strip.text = element_text(face = "bold", size = 13)) +
    guides(fill = guide_legend(nrow = 1))
  
  
  # Clean up data for download
  lcRank_data <- lifeCourseRank_chart_data %>% 
    filter(row_n4 == 1) %>% 
    arrange(year, county, raceNameShort, sex, ageGroup, Ranking) %>% 
    mutate(ageGroup = paste0(" ", ageGroup)) %>%  # pad space to prevent Excel from automatically converting age group to date
    select(year, county, raceNameShort, sex, ageGroup, causeNameShort, topLevName, Ndeaths, cDeathRate, Ranking)
  
  list(plotL = lcRank_plot, dataL = lcRank_data, plotLD = lcRank_download_plot)
}



## Test
if (FALSE) {
  makelifeCourseRate_chart(myYear = "2018-2022", myYearGrouping = 5, myLHJ = "CALIFORNIA", bySex = "No", 
                       myRace = "Total", myAge = ageSort, myN = 5, logScale = TRUE, repel = TRUE, tab = "main")
  
  makelifeCourseRate_chart(myYear = "2022", myYearGrouping = 1, myLHJ = "Los Angeles", bySex = "No", 
                       myRace = "White", myAge = ageSort, myN = 6, logScale = TRUE, repel = TRUE, tab = "main")
  
  makelifeCourseRate_chart(myYear = "2018-2022", myYearGrouping = 5, myLHJ = "CALIFORNIA", bySex = "No", 
                       myRace = c("Black", "White"), myAge = c("45 - 54", "55 - 64", "65 - 74", "75 - 84"), 
                       myN = 5, logScale = TRUE, repel = TRUE, tab = "disparity")
  
  
  makelifeCourseRank_chart(myYear = "2022", myYearGrouping = 1, myLHJ = "Los Angeles", bySex = "Yes", 
                           myRace = "Latino", myAge = ageSort, myN = 5, tab = "main")
  
  makelifeCourseRank_chart(myYear = "2018-2022", myYearGrouping = 5, myLHJ = "Pasadena", bySex = "No", 
                           myRace = "Total", myAge = ageSort, myN = 5, tab = "main")
  
  makelifeCourseRank_chart(myYear = "2022", myYearGrouping = 1, myLHJ = "Santa Clara", bySex = "No", 
                           myRace = c("White", "Black"), myAge = c("45 - 54", "55 - 64", "65 - 74"), myN = 5, tab = "disparity")
  
  makelifeCourseRank_chart(myYear = "2018-2022", myYearGrouping = 5, myLHJ = "Orange", bySex = "Yes", 
                           myRace = c("White", "Asian"), myAge = c("45 - 54", "55 - 64", "65 - 74"), myN = 5, tab = "disparity")
}
