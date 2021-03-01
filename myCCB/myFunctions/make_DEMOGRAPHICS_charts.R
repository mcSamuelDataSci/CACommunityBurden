

make_demoPop_Pyramid <- function(myCounty) {
  
  roundUpNice <- function(x, nice=c(1:10)) {
    if(length(x) != 1) stop("'x' must be of length 1")
    10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
  }
  
  if (myCounty == "CALIFORNIA") {
    
    plotTitle <- "Population Pyramid in California"
  } else {
    
    plotTitle <- paste0("Population Pyramid in ", myCounty, " County" )
  }
  
  tDat <- filter(popData_AgePyramid, countyName == myCounty)
  
  plotBreaks <- seq(-roundUpNice(max(tDat$population)),roundUpNice(max(tDat$population)),by=roundUpNice(max(tDat$population)/5))
  
  ggplot(tDat, aes(x = ageGroup, y = population, fill = sex)) +
    geom_bar(data = subset(tDat, sex == "Female"), stat = "identity") + 
    geom_bar(data = subset(tDat, sex == "Male"), stat = "identity", aes(y=population*(-1))) +
    scale_y_continuous(breaks= plotBreaks, 
                       limits = max(tDat$population) * c(-1,1), 
                       labels = scales::comma(abs(plotBreaks))) +
    coord_flip() +
    labs(title = plotTitle) +
    theme(legend.position="none", 
          plot.title = element_text(hjust = 0.5), 
          axis.text.x = element_text(angle = 45, vjust = 0.5), 
          axis.title=element_text(size=myAxisTitleSize - 2,face="bold")) + # , margin=margin(0,0,30,0))) + 
    annotate("text", label = "Male", x = 20, y = -0.75*max(tDat$population), size = 8, colour = "#00BEC6") + 
    annotate("text", label = "Female", x = 20, y = 0.75*max(tDat$population), size = 8, colour = "#FF706E")
}



# Pie chart - Race Pop

demoPop_RacePie <- function(myCounty) {
  
  tDat <- filter(popData_RacePie, countyName == myCounty)
  
  # Get county/state total population
  totalPop <- scales::comma(sum(tDat$population, na.rm = T))
  
  if (myCounty == "CALIFORNIA") {
    plotTitle <- paste0("Population in California by Race/Ethnicity\n", "(Total Population - ", totalPop, ")")
    # plotTitle <- "Population in California by Race/Ethnicity"
  } else {
    plotTitle <- paste0("Population in ", myCounty, " County by Race/Ethnicity\n", "(Total Population - ", totalPop, ")")
    #plotTitle <- paste0("Population in ", myCounty, " County by Race/Ethnicity")
  }
  
  plot_ly(tDat, labels = ~raceName, values = ~population, type = 'pie',textposition = 'outside', textinfo = 'label+percent') %>%
    layout(margin = list(l=50, r=50, b=100, t=100, pad=2), title = list(text = plotTitle, font = list(size = myTitleSize, color = "blue")), showlegend = FALSE, 
           font = list(size = 10), xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
  
  
  
}


# Race Age Pop

demoPop_RaceAge <- function(myCounty) {
  
  if (myCounty == "CALIFORNIA") {
    plotTitle <- "Population Distribution in California by Race/Ethnicity and Age Group"
  } else {
    plotTitle <- paste0("Population Distribution in ", myCounty, " County by Race/Ethnicity and Age Group")
  }
  
  tDat <- filter(popData_RaceAge, countyName == myCounty) %>%
    group_by(raceName) %>%
    mutate(percent = round(100*population/sum(population), 1))
  
  
  ggplot(tDat, aes(x=raceName, y=percent, fill=ageGroup)) + 
    geom_bar(position=position_stack(reverse = TRUE), stat="identity") +
    scale_y_continuous(labels=function(x) scales::percent(x/100)) +
    coord_flip() +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 25)) + # str_wrap to wrap text
    labs(title = plotTitle) +
    theme(axis.title=element_text(size=myAxisTitleSize - 2,face="bold"))
  
  
  
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



#demoPop_Pyramid("CALIFORNIA")
#demoPop_RaceAge("CALIFORNIA")
# demoPop_RacePie("Butte")
# demoPop_All("CALIFORNIA")

