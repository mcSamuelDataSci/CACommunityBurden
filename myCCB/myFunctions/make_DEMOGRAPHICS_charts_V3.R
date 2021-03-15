make_demoPop_Pyramid <- function(myCounty) {
  
    tDat <- filter(popData_AgePyramid, county == myCounty) %>%
    mutate(population_notAbs = ifelse(sex == "Male", -1 * population, population), 
           plotText = paste0("Sex: ", sex, "\nAge Group: ", ageGroup, "\nPopulation: ", scales::comma(population))) %>%
    arrange(desc(ageGroup))
  
    if (myCounty == "CALIFORNIA") plotTitle <- paste0("Population Pyramid in ", myCounty)
    if (myCounty != "CALIFORNIA") plotTitle <- paste0("Population Pyramid in ", myCounty, " County")
    
  # Plot
  myPlot <- tDat %>%
    hchart('bar', hcaes(x = ageGroup, y = population_notAbs, group = sex), stacking = 'normal') %>%
    hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator", "downloadPNG", "downloadJPEG", "downloadPDF")))) %>%
    hc_colors(c("#FF706E", "#00BEC6")) %>%
    hc_yAxis(title = list(text = "Population")) %>%
    hc_xAxis(title = list(text = "Age Group")) %>%
    hc_title(text = plotTitle, align = "left")
  
  # Table
  
  tDat <- tDat %>%
    select(year, county, raceName, ageGroup, sex, population) %>%
    mutate(county = str_to_title(county)) %>%
    arrange(ageGroup)
  
  myTable <- datatable(tDat, rownames = FALSE, extensions = 'Buttons',
                       colnames = c("Year","County","Race/Ethnicity","Age Group","Sex","Population"), 
                       options = list(
                         dom = 'Bfrtip',
                         columnDefs = list(list(className = 'dt-left', targets = "_all")), 
                         scrollX = TRUE, # horizontal scrollbar
                         buttons = list('copy', 'print', list(extend = 'collection', buttons = c('csv', 'excel'), text = 'Download'))
                         )) %>%
    formatCurrency('population', currency = "", interval = 3, mark = ",")
  
  list(plotL = myPlot, tableL = myTable)
}



# Pie chart - Race Pop

demoPop_RacePie <- function(myCounty) {
  
  plotColors <- raceNamesShortColors[!names(raceNamesShortColors) %in% c("Other", "Unknown", "Total", NA)]
  raceColors_df <- data.frame(raceNameShort = names(plotColors), raceColor = unname(plotColors))
  
  tDat <- filter(popData_RacePie, county == myCounty) %>%
    left_join(raceColors_df, by = "raceNameShort")
  
  if (myCounty == "CALIFORNIA") plotTitle <- paste0("Population by Race/Ethnicity in ", myCounty)
  if (myCounty != "CALIFORNIA") plotTitle <- paste0("Population by Race/Ethnicity in ", myCounty, " County")
  
  # Plot
  myPlot <- tDat %>%
    hchart('pie', hcaes(x = raceNameShort, label = raceNameShort, y = population, color = raceColor)) %>%
    hc_title(text = plotTitle, align = 'center') %>%
    hc_tooltip(formatter = JS("function(){
                             return  '<b>' + this.point.label + ': </b>(Population:' +this.y+', Percentage: '+Highcharts.numberFormat(this.percentage)+'%)'
  }"),useHTML = FALSE) %>%
    hc_plotOptions(pie = list(dataLabels = list(enabled = TRUE,format="{point.label}<br>{point.percentage:.2f} %"))) %>%
    hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator", "downloadPNG", "downloadJPEG", "downloadPDF"))))
  
  # Table
  tDat <- tDat %>%
    select(year, county, raceName, population) %>%
    mutate(county = str_to_title(county)) %>%
    arrange(raceName)
  
  myTable <- datatable(tDat, rownames = FALSE, extensions = 'Buttons',
                       colnames = c("Year","County","Race/Ethnicity", "Population"), 
                       options = list(
                         dom = 'Bfrtip',
                         columnDefs = list(list(className = 'dt-left', targets = "_all")), 
                         scrollX = TRUE, # horizontal scrollbar
                         buttons = list('copy', 'print', list(extend = 'collection', buttons = c('csv', 'excel'), text = 'Download'))
                       )) %>%
    formatCurrency('population', currency = "", interval = 3, mark = ",")
  
  list(plotL = myPlot, tableL = myTable)
  
  
}


# Race Age Pop

demoPop_RaceAge <- function(myCounty) {
  
  tDat <- filter(popData_RaceAge, county == myCounty) %>%
    group_by(raceName) %>%
    mutate(percent = round(100*population/sum(population), 1)) %>%
    ungroup() %>%
    mutate(plotText = paste0("Race/Ethnicity: ", raceName, "\nAge Group: ", ageGroup, "\nPercent: ", scales::percent(percent/100, accuracy = 0.1), "\nPopulation: ", scales::comma(population)), 
           ageGroup = factor(ageGroup, levels = rev(levels(popData_RaceAge$ageGroup))))
  
  if (myCounty == "CALIFORNIA") plotTitle <- paste0("Population by Race/Ethnicity & Age Group in ", myCounty)
  if (myCounty != "CALIFORNIA") plotTitle <- paste0("Population by Race/Ethnicity & Age Group in ", myCounty, " County")
  
  # Plot
  myPlot <- tDat %>%
    hchart('bar', hcaes(x = raceNameShort, y = population, label = raceNameShort, group = ageGroup), stacking = 'percent') %>%
    hc_yAxis(title = list(text = "Percent")) %>%
    hc_xAxis(title = list(text = "Race/Ethnicity")) %>%
    hc_title(text = plotTitle, align = "left") %>%
    hc_tooltip(formatter = JS("function(){
                             return  '<b>' + this.point.label + ' (' +this.point.ageGroup+ ')</b><br>' + 'Population: ' +this.y+ '<br>Percentage: ' + Highcharts.numberFormat(this.percentage)+'%'
  }"),useHTML = FALSE) %>%
    hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator", "downloadPNG", "downloadJPEG", "downloadPDF"))))
  
  # Table
  tDat <- tDat %>%
    select(year, county, raceName, ageGroup, sex, population) %>%
    mutate(county = str_to_title(county)) %>%
    arrange(raceName)
  
  myTable <- datatable(tDat, rownames = FALSE, extensions = 'Buttons',
                       colnames = c("Year","County","Race/Ethnicity", "Age Group", "Sex", "Population"), 
                       options = list(
                         dom = 'Bfrtip',
                         columnDefs = list(list(className = 'dt-left', targets = "_all")), 
                         scrollX = TRUE, # horizontal scrollbar
                         buttons = list('copy', 'print', list(extend = 'collection', buttons = c('csv', 'excel'), text = 'Download'))
                       )) %>%
    formatCurrency('population', currency = "", interval = 3, mark = ",")
  
  list(plotL = myPlot, tableL = myTable)
  
  
}


# Trends function
make_demoPop_trend <- function(myCounty, trendType = "Total") {
  
  
  if (myCounty == "CALIFORNIA") plotTitle <- paste0(trendType, " Population Trend in ", myCounty)
  if (myCounty != "CALIFORNIA") plotTitle <- paste0(trendType, " Population Trend in ", myCounty, " County")
  
  # Total Trend
  if (trendType == "Total") {
    tDat_Total <- popData_trends %>%
      filter(county == myCounty, sex == "Total", raceName == "Total", ageGroup == "Total")
    
    myPlot <- tDat_Total %>% 
      hchart(., type = "line", hcaes(x = year, y = population)) %>%
      hc_yAxis(title = list(text = "Population")) %>%
      hc_xAxis(title = list(text = "Year")) %>%
      hc_tooltip(formatter = JS("function(){
                               return  '<b>' + this.x + '</b><br>Population:  ' + this.y.toLocaleString() }"),useHTML = FALSE) %>%
      hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator", "downloadPNG", "downloadJPEG", "downloadPDF"))))
  }
  
  # Race trend
  if (trendType == "Race/Ethnicity") {
    plotColors <- raceNamesShortColors[!names(raceNamesShortColors) %in% c("Other", "Unknown", "Total", NA)]
    raceColors_df <- data.frame(raceNameShort = names(plotColors), raceColor = unname(plotColors))
    
    tDat_Race <- popData_trends %>%
      filter(county == myCounty, sex == "Total", raceName != "Total", ageGroup == "Total") %>%
      left_join(raceColors_df, by = "raceNameShort")
    
    myPlot <- tDat_Race %>% 
      hchart(., type = "line", hcaes(x = year, y = population, group = raceNameShort, color = raceColor), 
             color = plotColors) %>%
      hc_yAxis(title = list(text = "Population")) %>%
      hc_xAxis(title = list(text = "Year")) %>%
      hc_tooltip(formatter = JS("function(){
                               return  '<b>' + this.x + '</b><br>' + this.point.raceNameShort + ' Population:  ' + this.y.toLocaleString() }"),useHTML = FALSE) %>%
      hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator", "downloadPNG", "downloadJPEG", "downloadPDF"))))
  }
  
  # Sex
  if (trendType == "Sex") {
    plotColors <- genderColors[names(genderColors) != "Total"]
    sexColors_df <- data.frame(sex = names(plotColors), sexColor = unname(plotColors))
    
    tDat_Sex <- popData_trends %>%
      filter(county == myCounty, sex != "Total", raceName == "Total", ageGroup == "Total") %>%
      left_join(sexColors_df, by = "sex")
    
    myPlot <- tDat_Sex %>% 
      hchart(., type = "line", hcaes(x = year, y = population, group = sex, color = sexColor), 
             color = plotColors) %>%
      hc_yAxis(title = list(text = "Population")) %>%
      hc_xAxis(title = list(text = "Year")) %>%
      hc_tooltip(formatter = JS("function(){
                               return  '<b>' + this.x + '</b><br>' + this.point.sex + ' Population:  ' + this.y.toLocaleString() }"),useHTML = FALSE) %>%
      hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator", "downloadPNG", "downloadJPEG", "downloadPDF"))))
  }
  
  # Age Group
  if (trendType == "Age Group") {
    tDat_Age <- popData_trends %>%
      filter(county == myCounty, sex == "Total", raceName == "Total", ageGroup != "Total")
    
    myPlot <- tDat_Age %>% 
      hchart(., type = "line", hcaes(x = year, y = population, group = ageGroup)) %>%
      hc_yAxis(title = list(text = "Population")) %>%
      hc_xAxis(title = list(text = "Year")) %>%
      hc_tooltip(formatter = JS("function(){
                               return  '<b>' + this.x + '</b><br>' + this.point.ageGroup + ' Age Group Population:  ' + this.y.toLocaleString() }"),useHTML = FALSE) %>%
      hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator", "downloadPNG", "downloadJPEG", "downloadPDF"))))
  }
  
  myPlot <- myPlot %>%
    hc_title(text = plotTitle, 
             align = "left")
  
  
  # Table
  tDat <- popData_trends %>%
    filter(county == myCounty) %>%
    select(year, county, raceName, ageGroup, sex, population) %>%
    mutate(county = str_to_title(county)) %>%
    arrange(year, county, raceName, ageGroup, sex)
  
  myTable <- datatable(tDat, rownames = FALSE, extensions = 'Buttons',
                       colnames = c("Year","County","Race/Ethnicity", "Age Group", "Sex", "Population"), 
                       options = list(
                         dom = 'Bfrtip',
                         columnDefs = list(list(className = 'dt-left', targets = "_all")), 
                         scrollX = TRUE, # horizontal scrollbar
                         buttons = list('copy', 'print', list(extend = 'collection', buttons = c('csv', 'excel'), text = 'Download'))
                       )) %>%
    formatCurrency('population', currency = "", interval = 3, mark = ",")
  
  list(plotL = myPlot, 
       tableL = myTable)
  
}
