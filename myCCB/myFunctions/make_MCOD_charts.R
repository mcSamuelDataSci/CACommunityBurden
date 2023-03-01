if (FALSE) {
  myCounty <- "Alameda"
  mySex <- "Total"
  myYear <- 2021
  mySort <- "Ndeaths_primary"
  mySort <- "Ndeaths_other"
  mySort <- "Ndeaths_total"
  mySort <- "pPrimary"
  mySort <- "pOther"
  topN <- 10
}


mcodRankMeasure <- function(myCounty = "CALIFORNIA", 
                        myYear = 2021,
                        mySort, 
                        topN = 15, 
                        myCause) {
  
  if (grepl("Ndeaths", mySort)) { measureCol1 <- "Ndeaths_primary"; measureCol2 <- "Ndeaths_other"; titleX <- "Number of Deaths" }
  if (mySort %in% c("pPrimary", "pOther")) { measureCol1 <- "pPrimary"; measureCol2 <- "pOther"; titleX <- "Percent"}
  
  # Title
  titleCounty <- ifelse(myCounty == "CALIFORNIA", myCounty, paste(myCounty, "County"))
  titleSort <- case_when(
    mySort == "Ndeaths_primary" ~ "Number of Primary Deaths",
    mySort == "Ndeaths_other" ~ "Number of Secondary Deaths",
    mySort == "Ndeaths_total" ~ "Number of Total (Primary + Secondary) Deaths",
    mySort == "pPrimary" ~ "Percent Primary Deaths",
    mySort == "pOther" ~ "Percent Secondary Deaths"
  )
  
  myTitle <- paste0("Rankings of Primary and Secondary Causes of Death in ", titleCounty, ", ", myYear)
  mySubTitle <- paste0("Sorted by ", titleSort)
  
  # Label placement
  if (mySort %in% c("Ndeaths_primary", "pPrimary")) {labelX <- 0; label_hjust <- 0}
  if (mySort == "pOther") { labelX <- 1; label_hjust <- 1}
  myLineHeight <- 0.7
  myLabelSize <- 4
  
  # CauseNameShort
  myCauseNameShort <- deathCauseLink %>% filter(causeCode == myCause) %>% pull(causeNameShort)
  
  
  tDat_download <- datCounty_mcod %>%
    filter(year == myYear, county == myCounty, sex == "Total") 
  
  if (mySort %in% c("pPrimary", "pOther")) {
    tDat_download <- tDat_download %>% arrange(-!!as.symbol(mySort), desc(Ndeaths_total))
  } else {
    tDat_download <- tDat_download %>% arrange(-!!as.symbol(mySort))
  }
  
  tDat_download <- tDat_download %>%
    slice(1:topN) %>%
    left_join(select(deathCauseLink, causeCode, causeNameShort), by = "causeCode") %>%
    select(-causeCode, -starts_with("data"))
  
  
  tDat <- tDat_download %>% 
    mutate(causeNameShort = ifelse(causeNameShort == myCauseNameShort, paste0(causeNameShort, "*"), causeNameShort)) %>% 
    mutate(causeNameShort = factor(causeNameShort, levels = rev(.$causeNameShort)), 
           myLabel = case_when(mySort == "Ndeaths_primary" ~ paste0("% Primary: ", scales::percent(pPrimary, accuracy = .1)), 
                               mySort == "Ndeaths_other" ~ paste0("% Secondary: ", scales::percent(pOther, accuracy = .1)),
                               mySort == "Ndeaths_total" ~ paste0("% Secondary: ", scales::percent(pOther, accuracy = .1)),
                               mySort == "pPrimary" ~ paste0("Total Number of Deaths: ", scales::comma(Ndeaths_total, accuracy = 1)),
                               mySort == "pOther" ~ paste0("Total Number of Deaths: ", scales::comma(Ndeaths_total, accuracy = 1)))) %>% 
    rename(measure1 = !!as.symbol(measureCol1),
           measure2 = !!as.symbol(measureCol2)) %>%
    select(year, county, causeNameShort, measure1, measure2, myLabel) %>%
    pivot_longer(-c("year", "county", "causeNameShort", "myLabel"), names_to = "measure", values_to = "value") %>% 
    mutate(measure = ifelse(measure == "measure1", "Primary", "Secondary"), 
           measure = factor(measure, levels = rev(c("Primary", "Secondary"))), 
           myLabel = ifelse(row_number() %in% 1:2, myLabel, sub(".*[: ]", "", myLabel)), 
           myLabel = ifelse(measure == "Secondary", "", myLabel))
  
  tPlot <- ggplot(tDat, aes(x = value, y = causeNameShort, fill = measure)) +
    geom_bar(stat = "identity", color = "black") +
    scale_fill_manual(values = c("Primary" = "#AED6F1", "Secondary" = "#ABB2B9")) +
    scale_x_continuous(labels = if (grepl("Ndeaths", mySort)) scales::comma_format(accuracy=1) else scales::percent) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
    labs(x = titleX, y = "Cause of Death", title = str_wrap(myTitle, 35), subtitle = mySubTitle) +
    theme(legend.title = element_blank(), 
          axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 14), 
          legend.text = element_text(size = 12), 
          legend.position = "bottom", 
          plot.caption = element_text(size = 12))
  
  if (mySort %in% c("Ndeaths_primary", "pPrimary", "pOther")) {
    tPlot <- tPlot + geom_text(aes(label = myLabel), color = "black", lineheight = myLineHeight, size = myLabelSize, x = labelX, hjust = label_hjust) 
  } else {
    tPlot <- tPlot + geom_text(aes(label = myLabel, x = value), color = "black", lineheight = myLineHeight, size = myLabelSize, hjust = 0)
  }
  
  if (paste0(myCauseNameShort, "*") %in% tDat$causeNameShort) { 
    tPlot <- tPlot + 
      labs(caption = "*Indicates cause of death is selected in the right panel.")
    }
    
  list(dataL = tDat_download, plotL = tPlot)
  

}


mcodRankCause <- function(myCounty = "CALIFORNIA", 
                          myYear = 2021,
                          myCause = "C01", 
                          leadingPrimary = TRUE) {
  
  # CauseNameShort
  myCauseNameShort <- deathCauseLink %>% filter(causeCode == myCause) %>% pull(causeNameShort)
  titleCounty <- ifelse(myCounty == "CALIFORNIA", myCounty, paste(myCounty, "County"))
  
  tDat <- datCounty_mcod %>% 
    filter(sex == "Total", year == myYear, county == myCounty, causeCode == myCause)
  
  # Error messages
  if (leadingPrimary) {
    if (nrow(tDat)==0) stop(paste0("There are no cases where ", myCauseNameShort, " appears as a primary or secondary cause of death in ", titleCounty, " in ", myYear, ". Please select a new cause."))
    if (is.null(tDat$dataPrimary[[1]])) stop(paste0("There are no cases where ", myCauseNameShort, " appears as a secondary cause of death in ", titleCounty, " in ", myYear))
    
  } else {
    if (nrow(tDat)==0) stop(paste0("There are no cases where ", myCauseNameShort, " appears as a primary or secondary cause of death in ", titleCounty, " in ", myYear, ". Please select a new cause."))
    if (is.null(tDat$dataOther[[1]]) & tDat$Ndeaths_primary == 0) stop(paste0("There are no cases where ", myCauseNameShort, " appears as a primary cause of death in ", titleCounty, " in ", myYear))
    if (is.null(tDat$dataOther[[1]]) & tDat$Ndeaths_primary > 0) stop(paste0("There are no secondary causes of death where ", myCauseNameShort, " is listed as a primary cause of death in ", titleCounty, " in ", myYear))
  }
  
  tDat <- if(leadingPrimary) tDat$dataPrimary[[1]] else tDat$dataOther[[1]]
  
  # Titles
  mySubTitle <- paste0(titleCounty, ", ", myYear)
  myYTitle <- ifelse(leadingPrimary, "Primary Causes of Death", "Secondary Causes of Death")
  myTitle <- ifelse(leadingPrimary, 
                    paste0("Leading Primary Causes of Death where ", myCauseNameShort, " was the Secondary Cause"),
                    paste0("Leading Secondary Causes of Death where ", myCauseNameShort, " was the Primary Cause")
  )
  
  # Other setup
  myNdeathsCol <- ifelse(leadingPrimary, "Ndeaths_primary", "Ndeaths_other")
  myBarColor <- ifelse(leadingPrimary, "#ABB2B9", "#AED6F1")
  
  # Data setup
  tDat <- tDat %>% 
    rename(Ndeaths = !!as.symbol(myNdeathsCol)) %>% 
    arrange(desc(Ndeaths)) %>% 
    slice(1:20) %>% 
    left_join(select(deathCauseLink, causeCode, causeNameShort), by = "causeCode") %>% 
    select(-causeCode)
  
  # Plot
  tPlot <- ggplot(tDat, aes(x = Ndeaths, y = reorder(causeNameShort, Ndeaths))) +
    geom_bar(stat = "identity", color = "black", fill = myBarColor, width = 1) +
    scale_x_continuous(labels = scales::comma_format(accuracy=1)) +
    # scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
    labs(x = "Number of Deaths", y = myYTitle, title = str_wrap(myTitle, 35), subtitle = mySubTitle) +
    theme(legend.title = element_blank(), 
          axis.text.x = element_text(size = 12, angle = 90, hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 14), 
          legend.text = element_text(size = 12), 
          legend.position = "bottom")
  
  list(plotL = tPlot, dataL = tDat)
    
}





