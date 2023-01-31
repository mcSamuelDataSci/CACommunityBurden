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


mcodRanking <- function(myCounty = "CALIFORNIA", 
                        mySex = "Total", 
                        myYear = 2021,
                        mySort, 
                        topN = 15) {
  
  if (grepl("Ndeaths", mySort)) { measureCol1 <- "Ndeaths_primary"; measureCol2 <- "Ndeaths_other"; titleX <- "Number of Deaths" }
  if (mySort %in% c("pPrimary", "pOther")) { measureCol1 <- "pPrimary"; measureCol2 <- "pOther"; titleX <- "Percent"}
  
  # Title
  titleSex <- ifelse(mySex == "Total", "Total Sex", mySex)
  titleCounty <- ifelse(myCounty == "CALIFORNIA", myCounty, paste(myCounty, "County"))
  titleSort <- case_when(
    mySort == "Ndeaths_primary" ~ "Number of Primary Deaths",
    mySort == "Ndeaths_other" ~ "Number of Secondary Deaths",
    mySort == "Ndeaths_total" ~ "Number of Total (Primary + Secondary) Deaths",
    mySort == "pPrimary" ~ "Percent Primary Deaths",
    mySort == "pOther" ~ "Percent Secondary Deaths"
  )
  
  myTitle <- paste0("Rankings of Primary and Secondary Causes of Death in ", titleCounty, ", ", titleSex, ", ", myYear)
  mySubTitle <- paste0("Sorted by ", titleSort)
  
  # Label placement
  if (mySort %in% c("Ndeaths_primary", "pPrimary")) {labelX <- 0; label_hjust <- 0}
  if (mySort == "pOther") { labelX <- 1; label_hjust <- 1}
  myLineHeight <- 0.7
  myLabelSize <- 4
  
  
  tDat_download <- datCounty_mcod %>%
    filter(year == myYear, county == myCounty, sex == mySex) %>%
    arrange(-!!as.symbol(mySort)) %>%
    slice(1:topN) %>%
    left_join(select(deathCauseLink, causeCode, causeNameShort)) %>%
    select(-causeCode, !starts_with("data"))
  
  
  tDat <- tDat_download %>% 
    mutate(causeNameShort = factor(causeNameShort, levels = rev(.$causeNameShort)), 
           myLabel = case_when(mySort == "Ndeaths_primary" ~ paste0("% Primary: ", scales::percent(pPrimary, accuracy = .1)), 
                               mySort == "Ndeaths_other" ~ paste0("% Secondary: ", scales::percent(pOther, accuracy = .1)),
                               mySort == "Ndeaths_total" ~ paste0("% Secondary: ", scales::percent(pOther, accuracy = .1)),
                               mySort == "pPrimary" ~ paste0("Total Number of Deaths: ", scales::comma(Ndeaths_total, accuracy = 1)),
                               mySort == "pOther" ~ paste0("Total Number of Deaths: ", scales::comma(Ndeaths_total, accuracy = 1)))) %>% 
    rename(measure1 = !!as.symbol(measureCol1),
           measure2 = !!as.symbol(measureCol2)) %>%
    select(year, county, sex, causeNameShort, measure1, measure2, myLabel) %>%
    pivot_longer(-c("year", "county", "sex", "causeNameShort", "myLabel"), names_to = "measure", values_to = "value") %>% 
    mutate(measure = ifelse(measure == "measure1", "Primary", "Secondary"), 
           measure = factor(measure, levels = rev(c("Primary", "Secondary"))), 
           myLabel = ifelse(row_number() %in% 1:2, myLabel, sub(".*[: ]", "", myLabel)), 
           myLabel = ifelse(measure == "Secondary", NA, myLabel))
  
  
  tPlot <- ggplot(tDat, aes(x = value, y = reorder(causeNameShort, causeNameShort), fill = measure)) +
    geom_bar(stat = "identity", color = "black") +
    scale_fill_manual(values = c("Primary" = "#AED6F1", "Secondary" = "#ABB2B9")) +
    scale_x_continuous(labels = if (grepl("Ndeaths", mySort)) scales::comma else scales::percent) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
    labs(x = titleX, y = "Cause of Death", title = myTitle, subtitle = mySubTitle) +
    theme(legend.title = element_blank(), 
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
          plot.subtitle = element_text(size = myTextSize2))
  
  if (mySort %in% c("Ndeaths_primary", "pPrimary", "pOther")) {
    tPlot <- tPlot + geom_text(aes(label = myLabel), color = "black", lineheight = myLineHeight, size = myLabelSize, x = labelX, hjust = label_hjust) 
  } else {
    tPlot <- tPlot + geom_text(aes(label = myLabel, x = value), color = "black", lineheight = myLineHeight, size = myLabelSize, hjust = 0)
  }
    
  list(data = tDat_download, plot = tPlot)
  

}