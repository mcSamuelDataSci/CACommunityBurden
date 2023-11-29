# Documentation ================================================================================

# About: This script creates a life course table for the CCB application
# Life course: Leading N causes of death by age group

# Packages required:
# - dplyr
# - reactable
# - reactablefmtr

# Datasets used:
# - datState_AGE.RDS - For statewide 1-year deaths
# - datCounty_AGE_3year.RDS - For statewide and county-level 3-year deaths.

# Other requirements:
# - cause of death mapping object (deathCauseLink)
# - broad condition group colors (topLevColors)
# - Age linkage file (ageLink)
 
makeLifeCourse <- function(
  myLHJ, # character: inputID = myYear 
  myYearGrouping, # numeric: inputID = myYearGrouping_race_age; choices = 1, 3
  myYear, # character: inputID = myYear_lifeCourse
  myN = 5 # numeric: input ID = myN_lifeCourse; choices = 5, 10, 15
  ) {
  
  # ONLY RUN FOR TESTING PURPOSES
  if (FALSE) {
    myLHJ <- "CALIFORNIA"
    myYearGrouping <- 3
    myYear <- "2020-2022"
    myN <- 5
  }
  
  # Assign colors to broad condition groups ----------------------------
  topLevColorsDF <- data.frame(topLevName = names(topLevColors), topLevColor = unname(topLevColors), stringsAsFactors = F) %>% 
    filter(topLevName %in% c("Communicable", "Cancer", "Cardiovascular", "Other Chronic", "Injury"))
  
  # Set data ------------------------------------
  if (myLHJ == STATE & myYearGrouping == 1) {
    tDat <- datState_AGE %>% mutate(year = as.character(year))
  } else {
    tDat <- datCounty_AGE_3year %>% 
      rename(year = yearG3)
    myYearGrouping <- 3
  }
  
  # Prepare data ----------------------------------------
  
  topEachAge <- tDat %>% 
    filter(sex == "Total", Level == "lev2", !grepl("Z", causeCode), county == myLHJ, year == myYear, !is.na(ageGroup)) %>% 
    left_join(select(deathCauseLink, causeCode, causeNameShort, topLevName)) %>%
    left_join(topLevColorsDF) %>% 
    select(causeNameShort, ageGroup, Ndeaths, cDeathRate, topLevName, topLevColor) %>%
    group_by(ageGroup) %>% 
    mutate(Ranking = order(order(cDeathRate, decreasing=TRUE))) %>% 
    ungroup() %>% 
    filter(Ranking %in% 1:myN) 
  
  ## Stop code if no data exists ------
  if (nrow(topEachAge) == 0) stop("Sorry friend, data are suppressed per the California Health and Human Services Agency Data De-Identification Guidelines, or there are no cases that meet this criteria.")
  
  ## Get max number of causes within an age group
  # nRows <- max(topEachAge$Ranking, na.rm = TRUE)
    
  
  ## Ensure all age groups-Rankings are in data -----
  tAgeLink <- data.frame(ageName = rep(ageLink$ageName, myN), stringsAsFactors = F) %>% 
    group_by(ageName) %>% 
    mutate(Ranking = row_number()) %>% 
    ungroup()
  
  topEachAge <- topEachAge %>% 
    full_join(tAgeLink, by = c("ageGroup" = "ageName", "Ranking")) %>% 
    mutate(topLevColor = case_when(is.na(Ndeaths) ~ "white", 
                                   TRUE ~ topLevColor)
           )
  
  # Table setup ----------------------------------------------
  
  ## Cell values -----------------------
  tableValues <- topEachAge %>% 
    mutate(cellValue = ifelse(is.na(Ndeaths), NA_character_, 
                              paste0("<span class = 'tableCause'>", causeNameShort, "<br></span>", 
                                     "<span class = 'tableN'>", scales::comma(Ndeaths, accuracy = 1), "</span>")
                              )) %>% 
    select(Ranking, ageGroup, cellValue) %>% 
    pivot_wider(names_from = ageGroup, values_from = cellValue) %>% 
    arrange(Ranking) %>%
    mutate(Ranking = as.character(Ranking)) %>% 
    select(sort(peek_vars())) %>% 
    relocate(Ranking, .before = `0 - 4`) %>% 
    relocate(`5 - 14`, .after = `0 - 4`) %>%
    replace(is.na(.), "<center>-</center>") 
  #%>% 
   # tibble::add_row() %>% 
  #  replace(is.na(.), "<span class = 'emptyRow'></span>")
    
  
  # Add legend row
  # tableValues[nrow(tableValues) + 1, ] <- as.list(c(rep("", 3), "Broad Condition Group", topLevColorsDF$topLevName, rep("", 2)))
  
  
  ## Cell background colors -----------------------
  tableBackgroundColors <- topEachAge %>% 
    select(Ranking, ageGroup, topLevColor) %>% 
    pivot_wider(names_from = "ageGroup", values_from = "topLevColor") %>%
    arrange(Ranking) %>%
    mutate(Ranking = "white") %>% 
    select(sort(peek_vars())) %>% 
    relocate(Ranking, .before = `0 - 4`) %>% 
    relocate(`5 - 14`, .after = `0 - 4`) 
  #%>% 
   # tibble::add_row() %>% # Add a blank row
    #replace(is.na(.), "white")
  
  # Add legend row
  #tableBackgroundColors[nrow(tableBackgroundColors) + 1, ] <- as.list(c(rep("white", 4), topLevColorsDF$topLevColor, rep("white", 2))) 
  
  ## Cell text colors -----------------------------
  # tableTextColors <- topEachAge %>% 
  #   mutate(topLevName = ifelse(topLevName == "Injury", "black", "white")) %>% 
  #   select(Ranking, ageGroup, topLevName) %>% 
  #   pivot_wider(names_from = "ageGroup", values_from = "topLevName") %>% 
  #   arrange(Ranking) %>%
  #   mutate(Ranking = "black") %>%
  #   select(sort(peek_vars())) %>% 
  #   relocate(Ranking, .before = `0 - 4`) %>% 
  #   relocate(`5 - 14`, .after = `0 - 4`) 
  #%>% 
   # tibble::add_row() %>% 
  #  replace(is.na(.), "black")
  
  # Add legend row
 # tableTextColors[nrow(tableTextColors) + 1, ] <- as.list(c(rep("black", 4), rep("white", 4), rep("black", 3)))
  
  
  # Create table --------------------------------------------------------------------------------------------------
  colStyle <- function(myCol) {
    colDef(html = T, style = color_scales(tableBackgroundColors, color_ref = myCol))
  }
  
  # lhjTitle <- ifelse(myLHJ == STATE, STATE, paste0(myLHJ, " County"))
  # tableTitle <- paste0("Leading Causes of Death Across the Life Course in ", lhjTitle, ", ", myYear)
  # tableSubtitle <- "Note: Small cells are suppressed in order to adhere to the California Health and Human Services Agency Data De-Identification Guidelines."
  
  
  lifeCourseTable <- reactable(
    tableValues, sortable = F, wrap = T, fullWidth = T, pagination = F, bordered = T, outlined = F,
    highlight = TRUE, defaultPageSize = 15,
    theme = reactableTheme(
      headerStyle = list(borderWidth = "3px 0px", paddingTop = "12px", verticalAlign = "bottom", textAlign = "bottom", 
                         borderColor = "#222222", color = "black", fontSize = 20),
      borderWidth = "1px", borderColor = "#dddddd", cellPadding = 5, 
      tableStyle = list(borderBottom = "3px solid #222222")
    ),
    defaultColDef = colDef(vAlign = "center", align = "center"),
    # columnGroups = list(
    #   colGroup(name = "Age Group", columns = tAgeLink$ageName, headerStyle = list(fontSize = 20, borderWidth = "0"))
    # ),
    columns = list(
      Ranking = colStyle("Ranking"),
     `0 - 4` = colStyle("0 - 4"),
     `5 - 14` = colStyle("5 - 14"),
     `15 - 24` = colStyle("15 - 24"),
     `25 - 34` = colStyle("25 - 34"),
     `35 - 44` = colStyle("35 - 44"),
     `45 - 54` = colStyle("45 - 54"),
     `55 - 64` = colStyle("55 - 64"),
     `65 - 74` = colStyle("65 - 74"),
     `75 - 84` = colStyle("75 - 84"),
     `85+` = colStyle("85+")
     )) 
  
  return(lifeCourseTable)
  
  
}

# Series of tests ---------------
if (FALSE) {
  makeLifeCourse("CALIFORNIA", myYearGrouping = 1, myYear = "2022", myN = 15)
  makeLifeCourse("CALIFORNIA", myYearGrouping = 3, myYear = "2020-2022", myN = 5)
  makeLifeCourse("Fresno", myYearGrouping = 3, myYear = "2020-2022", myN = 15)
}

