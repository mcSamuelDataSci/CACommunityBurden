make_demoPop_Pyramid <- function(myCounty) {
  
    tDat <- filter(popData_AgePyramid, countyName == myCounty) %>%
    mutate(population_notAbs = ifelse(sex == "Male", -1 * population, population), 
           plotText = paste0("Sex: ", sex, "\nAge Group: ", ageGroup, "\nPopulation: ", scales::comma(population))) %>%
    arrange(desc(ageGroup))
  
  # Plot
  myPlot <- tDat %>%
    hchart('bar', hcaes(x = ageGroup, y = population_notAbs, group = sex), stacking = 'normal') %>%
    hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator", "downloadPNG", "downloadJPEG", "downloadPDF")))) %>%
    hc_colors(c("#FF706E", "#00BEC6")) %>%
    hc_yAxis(title = list(text = "Population")) %>%
    hc_xAxis(title = list(text = "Age Group"))
  
  # Table
  
  tDat <- tDat %>%
    select(year, countyName, raceName, ageGroup, sex, population) %>%
    mutate(countyName = str_to_title(countyName)) %>%
    arrange(ageGroup)
  
  myTable <- datatable(tDat, rownames = FALSE, extensions = 'Buttons',
                       colnames = c("Year","County","Race/Ethnicity","Age Group","Sex","Population"), 
                       options = list(
                         dom = 'Bfrtip',
                         columnDefs = list(list(className = 'dt-left', targets = "_all")), 
                         buttons = list('copy', 'print', list(extend = 'collection', buttons = c('csv', 'excel'), text = 'Download'))
                         )) %>%
    formatCurrency('population', currency = "", interval = 3, mark = ",")
  
  list(plotL = myPlot, tableL = myTable)
}



# Pie chart - Race Pop

demoPop_RacePie <- function(myCounty) {
  
  plotColors <- raceNamesShortColors[!names(raceNamesShortColors) %in% c("Other", "Unknown", "Total", NA)]
  raceColors_df <- data.frame(raceNameShort = names(plotColors), raceColor = unname(plotColors))
  
  tDat <- filter(popData_RacePie, countyName == myCounty) %>%
    left_join(raceColors_df, by = "raceNameShort")
  
  # Plot
  myPlot <- tDat %>%
    hchart('pie', hcaes(x = raceNameShort, label = raceNameShort, y = population, color = raceColor)) %>%
    hc_tooltip(formatter = JS("function(){
                             return  '<b>' + this.point.label + ': </b>(Population:' +this.y+', Percentage: '+Highcharts.numberFormat(this.percentage)+'%)'
  }"),useHTML = FALSE) %>%
    hc_plotOptions(pie = list(dataLabels = list(enabled = TRUE,format="{point.label}<br>{point.percentage:.2f} %"))) %>%
    hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator", "downloadPNG", "downloadJPEG", "downloadPDF"))))
  
  # Table
  tDat <- tDat %>%
    select(year, countyName, raceName, population) %>%
    mutate(countyName = str_to_title(countyName)) %>%
    arrange(raceName)
  
  myTable <- datatable(tDat, rownames = FALSE, extensions = 'Buttons',
                       colnames = c("Year","County","Race/Ethnicity", "Population"), 
                       options = list(
                         dom = 'Bfrtip',
                         columnDefs = list(list(className = 'dt-left', targets = "_all")), 
                         buttons = list('copy', 'print', list(extend = 'collection', buttons = c('csv', 'excel'), text = 'Download'))
                       )) %>%
    formatCurrency('population', currency = "", interval = 3, mark = ",")
  
  list(plotL = myPlot, tableL = myTable)
  
  
}


# Race Age Pop

demoPop_RaceAge <- function(myCounty) {
  
  tDat <- filter(popData_RaceAge, countyName == myCounty) %>%
    group_by(raceName) %>%
    mutate(percent = round(100*population/sum(population), 1)) %>%
    ungroup() %>%
    mutate(plotText = paste0("Race/Ethnicity: ", raceName, "\nAge Group: ", ageGroup, "\nPercent: ", scales::percent(percent/100, accuracy = 0.1), "\nPopulation: ", scales::comma(population)), 
           ageGroup = factor(ageGroup, levels = rev(levels(popData_RaceAge$ageGroup))))
  
  # Plot
  myPlot <- tDat %>%
    hchart('bar', hcaes(x = raceNameShort, y = population, label = raceNameShort, group = ageGroup), stacking = 'percent') %>%
    hc_yAxis(title = list(text = "Percent")) %>%
    hc_xAxis(title = list(text = "Race/Ethnicity")) %>%
    hc_tooltip(formatter = JS("function(){
                             return  '<b>' + this.point.label + ' (' +this.point.ageGroup+ ')</b><br>' + 'Population: ' +this.y+ '<br>Percentage: ' + Highcharts.numberFormat(this.percentage)+'%'
  }"),useHTML = FALSE) %>%
    hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator", "downloadPNG", "downloadJPEG", "downloadPDF"))))
  
  # Table
  tDat <- tDat %>%
    select(year, countyName, raceName, ageGroup, sex, population) %>%
    mutate(countyName = str_to_title(countyName)) %>%
    arrange(raceName)
  
  myTable <- datatable(tDat, rownames = FALSE, extensions = 'Buttons',
                       colnames = c("Year","County","Race/Ethnicity", "Age Group", "Sex", "Population"), 
                       options = list(
                         dom = 'Bfrtip',
                         columnDefs = list(list(className = 'dt-left', targets = "_all")), 
                         buttons = list('copy', 'print', list(extend = 'collection', buttons = c('csv', 'excel'), text = 'Download'))
                       )) %>%
    formatCurrency('population', currency = "", interval = 3, mark = ",")
  
  list(plotL = myPlot, tableL = myTable)
  
  
}


demoPop_All <- function(myCounty) {
  
  if (myCounty == "CALIFORNIA") {
    plotTitle1 <- "Population in California by Race/Ethnicity"
    plotTitle2 <- "Population Pyramid in California"
    plotTitle3 <- "Population Distribution in California by Race/Ethnicity and Age Group"
  } else {
    plotTitle1 <- paste0("Population in ", myCounty, " County by Race/Ethnicity")
    plotTitle2 <- paste0("Population Pyramid in ", myCounty, " County" )
    plotTitle3 <- paste0("Population Distribution in ", myCounty, " County by Race/Ethnicity and Age Group")
  }
  
  # ------------ Race Pie ------------------------------
  
  tDat <- filter(popData_RacePie, countyName == myCounty)
  
  p1 <- plot_ly(tDat, labels = ~raceName, values = ~population, type = 'pie',textposition = 'outside', textinfo = 'label+percent') %>%
    layout(margin = list(l=50, r=50, b=100, t=100, pad=2), title = list(text = plotTitle1, font = list(size = 16)), showlegend = FALSE, 
           font = list(size = 10), xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
  
  # -------------- Population Pyramid ---------------------
  
  roundUpNice <- function(x, nice=c(1:10)) {
    if(length(x) != 1) stop("'x' must be of length 1")
    10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
  }
  
  tDat <- filter(popData_AgePyramid, countyName == myCounty)
  
  plotBreaks <- seq(-roundUpNice(max(tDat$population)),roundUpNice(max(tDat$population)),by=roundUpNice(max(tDat$population)/5))
  
  p2 <- ggplot(tDat, aes(x = ageGroup, y = population, fill = sex)) +
    geom_bar(data = subset(tDat, sex == "Female"), stat = "identity") + 
    geom_bar(data = subset(tDat, sex == "Male"), stat = "identity", aes(y=population*(-1))) +
    scale_y_continuous(breaks= plotBreaks, 
                       limits = max(tDat$population) * c(-1,1), 
                       labels = scales::comma(plotBreaks)) +
    coord_flip() +
    labs(title = plotTitle2) +
    theme(legend.position="none", plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, vjust = 0.5)) + # , margin=margin(0,0,30,0))) + 
    annotate("text", label = "Female", x = 20, y = -0.75*max(tDat$population), size = 8, colour = "#FF706E") + 
    annotate("text", label = "Male", x = 20, y = 0.75*max(tDat$population), size = 8, colour = "#00BEC6")
  
  
  # -------------- Race Age Bar Chart -------------------------
  
  tDat <- filter(popData_RaceAge, countyName == myCounty) %>%
    group_by(raceName) %>%
    mutate(percent = round(100*population/sum(population), 1))
  
  
  p3 <- ggplot(tDat, aes(x=raceName, y=percent, fill=ageGroup)) + 
    geom_bar(position=position_stack(reverse = TRUE), stat="identity") +
    scale_y_continuous(labels=function(x) scales::percent(x/100)) +
    coord_flip() +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 25)) + # str_wrap to wrap text
    labs(title = plotTitle3)
  
  fig <- subplot(p1, ggplotly(p2), ggplotly(p3), nrows = 2, margin = 0.04)
  
  fig
  
  
}