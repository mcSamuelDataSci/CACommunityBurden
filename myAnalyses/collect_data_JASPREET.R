# Finding specific code within every single file

fileNames <- list.files(".", recursive = TRUE, pattern = "\\.R$")


for (fileName in fileNames) {
  if (length(grep("dottedSelectInput", readLines(fileName))) > 0) { print(fileName)}
}


# make_cause_TABLE.R -> datCounty data frame: --------------------------------------------------------

  # Collect data (from years 2000-2018) for each County on specified conditons

collect_county <- function(year, mySex, conditions_grep) { # conditions_grep = "Diabetes|diabetes|heart|pulmonary"
  
  # year <- 2017
  # mySex <- "Total"
  inDat <- datCounty
  
  dat.2 <- filter(inDat, year==myYear, sex==mySex, CAUSE != 0)
  dat.2$causeList <- fullCauseList[match(dat.2$CAUSE, fullCauseList[,"LABEL"]), "causeList"]  
  dat.2 <- dat.2[,c("county", "causeList","Ndeaths","cDeathRate","aRate","aLCI","aUCI","YLL","YLLper","YLL.adj.rate","SMR")]
  names(dat.2) <- c("County", "Condition","Number of deaths","Crude Death Rate","Age-Adjusted Death Rate (AADR)","Lower 95% CI AADR","Upper 95% CI AADR","Years of Life Lost (YLL)","YLL per 100,000 population","Age-Adjusted YLL Rate","Standard Mortality Ratio")
  
  data <- dat.2[grep(conditions_grep, dat.2$Condition), ] %>% arrange(Condition, County)
  
  return(data)

}

collect_county_df <- collect_county(year=2017:2018, mySex = "Total", conditions_grep = "Diabetes|diabetes|heart|pulmonary")

# -----------------------------------------------------------------------------------------------------


# datComm data frame (using make_MAPS.R code): --------------------------------------------------------------

  # Collect data (from years 2014-2018) for each community on specified conditions

collect_community <- function(mySex, conditions_grep) {
  
  dat.3 <- filter(datComm, sex==mySex)
  dat.3 <- dat.3[,c("yearG5", "county", "comName", "CAUSE","Ndeaths","cDeathRate","aRate","aLCI","aUCI","YLL","YLLper","YLL.adj.rate")]
  names(dat.3) <- c("Year", "County", "Community", "Condition","Number of deaths","Crude Death Rate","Age-Adjusted Death Rate (AADR)","Lower 95% CI AADR","Upper 95% CI AADR","Years of Life Lost (YLL)","YLL per 100,000 population","Age-Adjusted YLL Rate")
  
  data <- dat.3[grep(conditions_grep, dat.3$Condition), ] %>% arrange(Condition, County)
  # data <- data[order(data$Condition), ]
  
  return(data)

}

collect_community_df <- collect_community("Total", "C01|C02|C05|D01|D66")

# ----------------------------------------------------------------------------------------------------------



# datCounty_AGE_3year data frame (using make_ANY_TREND_chart.R code) ----------------------------------------

  # Collect data (in 3 year increments as early back as 2000) for each County on specified conditions)


collect_age_county <- function(mySex, conditions_grep) {
  
  dat.4 <- filter(datCounty_AGE_3year, sex==mySex)
  dat.4 <- dat.4[, c("yearG3", "county", "ageG", "CAUSE", "Ndeaths", "cDeathRate", "YLL", "YLLper")]
  names(dat.4) <- c("Year", "County", "Age Group", "Condition", "Number of Deaths", "Crude Death Rate", "Years of Life Lost (YLL)","YLL per 100,000 population")
  
  data <- dat.4[grep(conditions_grep, dat.4$Condition), ] %>% arrange(Condition, County, `Age Group`)
  
  return(data)
}

collect_age_county_df <- collect_age_county("Total", "C01|C02|C05|D01|D66")

# ----------------------------------------------------------------------------------------------------------

