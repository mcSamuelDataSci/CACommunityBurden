make_demoPop_Pyramid <- function(myCounty, myYear) {
  

  
  # print(myYear)
  roundUpNice <- function(x, nice=c(1:10)) {
    if(length(x) != 1) stop("'x' must be of length 1")
    10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
  }
  
  plotTitle <- ifelse(myCounty == "CALIFORNIA", paste0("Population Pyramid \nin ", myCounty, ", ", myYear), 
                      paste0("Population Pyramid \nin ", myCounty, " County, ", myYear))
  
  tDat <- filter(popData_AgePyramid, county == myCounty, year == myYear) %>%
    mutate(population_notAbs = ifelse(sex == "Male", -1 * population, population), 
           plotText = paste0("Sex: ", sex, "\nAge Group: ", ageGroup, "\nPopulation: ", scales::comma(population, accuracy = 1)))
  
  plotBreaks <- seq(-roundUpNice(max(tDat$population)),roundUpNice(max(tDat$population)),by=roundUpNice(max(tDat$population)/5))
  
  myPlot <- tDat %>%
    plot_ly(x = ~population_notAbs, y = ~ageGroup, color = ~sex, colors = genderColors) %>% # marker = list(color = c("red", "blue"))) %>%
    add_bars(orientation = 'h', hoverinfo = 'text', text = ~plotText) %>%
    layout(title = list(text = plotTitle, font = list(size = myTitleSize, color = "blue")), 
           margin = list(l=0, r=0, b=0, t=100, pad=2),
           bargap = 0.1, barmode = 'overlay',
           xaxis = list(title = "Population", tickmode = 'array', tickvals = plotBreaks,
                        ticktext = scales::comma(abs(plotBreaks))), 
           yaxis = list(title = list(text = "Age Group", standoff = 30L)))
  
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
    formatCurrency('population', currency = "", interval = 3, mark = ",", digits = 0)
  
  list(plotL = myPlot, tableL = myTable)
  
  
}



# Pie chart - Race Pop

make_demoPop_RacePie <- function(myCounty, myYear) {
  
  tDat <- filter(popData_RacePie, county == myCounty, year == myYear) 
  
  plotTitle <- ifelse(myCounty == "CALIFORNIA", paste0("Population by Race/Ethnicity \nin ", myCounty, ", ", myYear),
                      paste0("Population by Race/Ethnicity \nin ", myCounty, " County, ", myYear))
  
  raceColors <- raceNameShortColors[names(raceNameShortColors) %in% unique(tDat$raceNameShort)]
  
  # Place text inside
  
  # plot_ly(tDat, labels = ~raceNameShort, values = ~population, type = 'pie',textposition = 'inside', textinfo = 'label+percent', 
  #         marker = list(colors = raceNamesShortColors), showlegend = T) %>%
  #   layout(margin = list(l=0, r=0, b=0, t=100, pad=0),
  #     title = list(text = plotTitle, font = list(size = myTitleSize, color = "blue")),
  #     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
  #     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
  #     legend = list(orientation = "h",   # show entries horizontally
  #                   xanchor = "center",  # use center of legend as anchor
  #                   x = 0.5))
  
  # Place text outside
  
  myPlot <- plot_ly(tDat, labels = ~raceNameShort, values = ~population, type = 'pie',textposition = 'outside', textinfo = 'label+percent', 
          marker = list(colors = raceColors, 
                        line = list(width = 2, color = 'rgb(0, 0, 0)')), showlegend = F) %>%
    layout(margin = list(l=50, r=50, b=100, t=100, pad=2), title = list(text = plotTitle, font = list(size = myTitleSize, color = "blue")),
           font = list(size = 10), xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
  
  
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
    formatCurrency('population', currency = "", interval = 3, mark = ",", digits = 0)
  
  list(plotL = myPlot, tableL = myTable)
  
  
}


# Race Age Pop

make_demoPop_RaceAge <- function(myCounty, myYear) {
  
  plotTitle <- ifelse(myCounty == "CALIFORNIA", 
                      paste0("Population by Race/Ethnicity & Age Group \nin ", myCounty, ", ", myYear), 
                      paste0("Population by Race/Ethnicity & Age Group \nin ", myCounty, " County, ", myYear))

  
  tDat <- filter(popData_RaceAge, county == myCounty, year == myYear) %>%
    group_by(raceName) %>%
    mutate(percent = round(100*population/sum(population), 1)) %>%
    ungroup() %>%
    mutate(plotText = paste0("Race/Ethnicity: ", raceName, "\nAge Group: ", ageGroup, "\nPercent: ", scales::percent(percent/100, accuracy = 0.1), "\nPopulation: ", scales::comma(population)))
  
  races <- unique(tDat$raceNameShort)
  
  tDat <- tDat %>%
    mutate(raceNameShort = factor(raceNameShort, levels = rev(races)))
  
  ageColors <- data.frame(ageGroup = unique(tDat$ageGroup), 
                          ageColor = paletteCB[1:length(unique(tDat$ageGroup))])
  
  tDat <- tDat %>%
    left_join(ageColors, by = "ageGroup") %>%
    mutate(ageColor = as.character(ageColor))
    
  
  myPlot <- plot_ly(tDat, y = ~raceNameShort, x = ~percent, type = 'bar', color = ~ ageGroup, colors = ~ageColor, 
          hoverinfo = 'text', text = ~plotText, 
          marker = list(line = list(width = 2,
                                    color = 'rgb(0, 0, 0)'))) %>%
    layout(barmode = 'stack',
           margin = list(l=0, r=0, b=0, t=100, pad=2),
           title = list(text = plotTitle, font = list(size = myTitleSize, color = "blue")),
           xaxis = list(title = "Percent"),
           yaxis = list(title = list(text = "Race/Ethnicity", standoff = 40L)))
  
  
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
    formatCurrency('population', currency = "", interval = 3, mark = ",", digits = 0)
  
  list(plotL = myPlot, tableL = myTable)
  
  
}

# Trends function
make_demoPop_trend <- function(myCounty, trendType = "Total") {
  
  
  if (myCounty == "CALIFORNIA") plotTitle <- paste0(trendType, " Population Trend \nin ", myCounty)
  if (myCounty != "CALIFORNIA") plotTitle <- paste0(trendType, " Population Trend \nin ", myCounty, " County")
  
  # Total Trend
  if (trendType == "Total") {
    
    tDat_Total <- popData_trends %>%
      filter(county == myCounty, sex == "Total", raceName == "Total", ageGroup == "Total") %>%
      mutate(plotText = paste0(year, "\nPopulation: ", scales::comma(population, accuracy = 1)))
    
    
    myPlot <- plot_ly(tDat_Total, y = ~population, x = ~year, type = 'scatter', mode = 'lines+markers', color = I(paletteCB[1]),
                      hoverinfo = 'text', text = ~plotText) %>% 
      layout(title = list(text = plotTitle, font = list(size = myTitleSize, color = "blue")),
             xaxis = list(title = "Year"),
             yaxis = list(title = list(text = "Population", standoff = 30L)), 
             margin = list(t=50, pad=2))
    
  }
  
  # Race trend
  if (trendType == "Race/Ethnicity") {
    
    plotColors <- data.frame(raceNameShort = names(raceNameShortColors), plotColor = unname(raceNameShortColors))
    
    tDat_Race <- popData_trends %>%
      filter(county == myCounty, sex == "Total", raceName != "Total", ageGroup == "Total") %>%
      left_join(plotColors, by = "raceNameShort") %>%
      mutate(plotText = paste0(year, "\n", raceName, " Population: ", scales::comma(population, accuracy = 1)), 
             plotColor = as.character(plotColor))
    
    
    myPlot <- plot_ly(tDat_Race, y = ~population, x = ~year, color = ~raceNameShort, colors = raceNameShortColors,  
                      type = 'scatter', mode = 'lines+markers',
                      hoverinfo = 'text', text = ~plotText) %>% 
      layout(title = list(text = plotTitle, font = list(size = myTitleSize, color = "blue")),
             xaxis = list(title = "Year"),
             yaxis = list(title = list(text = "Population", standoff = 30L)), 
             margin = list(t=50, pad=2))
  
  }
  
  # Sex
  if (trendType == "Sex") {
    
    plotColors <- data.frame(sex = names(genderColors), plotColor = unname(genderColors))
    
    tDat_Sex <- popData_trends %>%
      filter(county == myCounty, sex != "Total", raceName == "Total", ageGroup == "Total") %>%
      mutate(plotText = paste0(year, "\n", sex, " Population: ", scales::comma(population, accuracy = 1))) %>%
      left_join(plotColors, by = "sex") %>%
      mutate(plotColor = as.character(plotColor))
    
    
    myPlot <- plot_ly(tDat_Sex, y = ~population, x = ~year, color = ~sex, colors = ~plotColor,  
                      type = 'scatter', mode = 'lines+markers',
                      hoverinfo = 'text', text = ~plotText) %>% 
      layout(title = list(text = plotTitle, font = list(size = myTitleSize, color = "blue")),
             xaxis = list(title = "Year"),
             yaxis = list(title = list(text = "Population", standoff = 30L)), 
             margin = list(t=50, pad=2))
  }
  
  # Age Group
  if (trendType == "Age Group") {
    
    tDat_Age <- popData_trends %>%
      filter(county == myCounty, sex == "Total", raceName == "Total", ageGroup != "Total") %>%
      mutate(plotText = paste0(year, "\n", ageGroup, " Population: ", scales::comma(population, accuracy = 1)))
    
    ageColors <- paletteCB[1:length(unique(tDat_Age$ageGroup))]
    names(ageColors) <- unique(tDat_Age$ageGroup)
    
    myPlot <- plot_ly(tDat_Age, y = ~population, x = ~year, color = ~ageGroup, colors = ageColors,  
                      type = 'scatter', mode = 'lines+markers',
                      hoverinfo = 'text', text = ~plotText) %>% 
      layout(title = list(text = plotTitle, font = list(size = myTitleSize, color = "blue")),
             xaxis = list(title = "Year"),
             yaxis = list(title = list(text = "Population", standoff = 30L)), 
             margin = list(t=50, pad=2))
  }

  
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
    formatCurrency('population', currency = "", interval = 3, mark = ",", digits = 0)
  
  list(plotL = myPlot, 
       tableL = myTable)
  
}


# demoPop_All <- function(myCounty) {
#   
#   if (myCounty == "CALIFORNIA") {
#     plotTitle1 <- "Population in California by Race/Ethnicity"
#     plotTitle2 <- "Population Pyramid in California"
#     plotTitle3 <- "Population Distribution in California by Race/Ethnicity and Age Group"
#   } else {
#     plotTitle1 <- paste0("Population in ", myCounty, " County by Race/Ethnicity")
#     plotTitle2 <- paste0("Population Pyramid in ", myCounty, " County" )
#     plotTitle3 <- paste0("Population Distribution in ", myCounty, " County by Race/Ethnicity and Age Group")
#   }
#   
#   # ------------ Race Pie ------------------------------
#   
#   tDat <- filter(popData_RacePie, countyName == myCounty)
#   
#   p1 <- plot_ly(tDat, labels = ~raceName, values = ~population, type = 'pie',textposition = 'outside', textinfo = 'label+percent') %>%
#     layout(margin = list(l=50, r=50, b=100, t=100, pad=2), title = list(text = plotTitle1, font = list(size = 16)), showlegend = FALSE, 
#            font = list(size = 10), xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
#            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
#   
#   # -------------- Population Pyramid ---------------------
#   
#   roundUpNice <- function(x, nice=c(1:10)) {
#     if(length(x) != 1) stop("'x' must be of length 1")
#     10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
#   }
#   
#   tDat <- filter(popData_AgePyramid, countyName == myCounty)
#   
#   plotBreaks <- seq(-roundUpNice(max(tDat$population)),roundUpNice(max(tDat$population)),by=roundUpNice(max(tDat$population)/5))
#   
#   p2 <- ggplot(tDat, aes(x = ageGroup, y = population, fill = sex)) +
#     geom_bar(data = subset(tDat, sex == "Female"), stat = "identity") + 
#     geom_bar(data = subset(tDat, sex == "Male"), stat = "identity", aes(y=population*(-1))) +
#     scale_y_continuous(breaks= plotBreaks, 
#                        limits = max(tDat$population) * c(-1,1), 
#                        labels = scales::comma(plotBreaks)) +
#     coord_flip() +
#     labs(title = plotTitle2) +
#     theme(legend.position="none", plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, vjust = 0.5)) + # , margin=margin(0,0,30,0))) + 
#     annotate("text", label = "Female", x = 20, y = -0.75*max(tDat$population), size = 8, colour = "#FF706E") + 
#     annotate("text", label = "Male", x = 20, y = 0.75*max(tDat$population), size = 8, colour = "#00BEC6")
#   
#   
#   # -------------- Race Age Bar Chart -------------------------
#   
#   tDat <- filter(popData_RaceAge, countyName == myCounty) %>%
#     group_by(raceName) %>%
#     mutate(percent = round(100*population/sum(population), 1))
#   
#   
#   p3 <- ggplot(tDat, aes(x=raceName, y=percent, fill=ageGroup)) + 
#     geom_bar(position=position_stack(reverse = TRUE), stat="identity") +
#     scale_y_continuous(labels=function(x) scales::percent(x/100)) +
#     coord_flip() +
#     scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 25)) + # str_wrap to wrap text
#     labs(title = plotTitle3)
#   
#   fig <- subplot(p1, ggplotly(p2), ggplotly(p3), nrows = 2, margin = 0.04)
#   
#   fig
#   
#   
# }



# make_demoPop_Pyramid("CALIFORNIA", 2020)$plotL
# make_demoPop_RaceAge("CALIFORNIA", 2020)$plotL
# make_demoPop_RacePie("CALIFORNIA", 2020)$plotL
# make_demoPop_trend("CALIFORNIA", "Total")$plotL

