### fix E4 regarding Standards


server <- T
if (!server) source("g:/FusionData/Standards/FusionStandards.R")
if (server) source("/mnt/projects/FusionData/Standards/FusionStandards.R")




makePlotRank()

makePlotRank(myData = "Deaths",               myStrata = "ageGroup", mySort        = "85+")$plot
makePlotRank(myData = "Hospitalizations",     myStrata = "raceCode", mySort        = "Black")
makePlotRank(myData = "Emergency Department", myStrata = "raceCode", mySort        = "White")
makePlotRank(myData = "Emergency Department", myCounty="Contra Costa",myStrata = "raceCode", mySort = "White", myMeasure = "N")
makePlotRank(myData = "Deaths", myCounty="Contra Costa",myStrata = "ageGroup", mySort = "85+", myMeasure = "cRate")
makePlotRank(myData = "Deaths", myCounty="Contra Costa",myStrata = "ageGroup", mySort = "35 - 44", myMeasure = "cRate")

myCounty <- "CALIFORNIA"


myAge    <- c("85+")  ####! Need Age7 Deaths....
#myTopN   <- 10






focusData <- path(ccbData,"real/age_race_focus_data")
death_age   <- readRDS(path(focusData, "deaths_age_stn.RDS"))
hosp_age    <- readRDS(path(focusData, "hospital_age_stn.RDS"))
ed_age      <- readRDS(path(focusData, "ed_age_stn.RDS"))
death_race  <- readRDS(path(focusData, "deaths_race_stn.RDS"))
hosp_race   <- readRDS(path(focusData, "hospital_race_stn.RDS"))
ed_race     <- readRDS(path(focusData, "ed_race_stn.RDS"))







deathHospEDchart <- function(myStrata = "Age Group", mySort = "85+", myCounty = "Los Angeles") {

library(cowplot)  
  
t.chart <- function(dataSet,source, legend =FALSE, myTopN = 10) {
  
  t.dat   <-  dataSet %>% 
    filter(MAINSTRATA %in% mySort, county == myCounty) %>% 
    arrange(-N) %>% 
    slice(1:myTopN)
  
  tPlot <- ggplot(data=t.dat, aes(x=reorder(causeName,N), y=N, fill=topLevName)) + 
    geom_bar(stat="identity") + coord_flip() + 
    theme(legend.position="bottom", 
          axis.text.y  = element_text(size = myAxisTextSize - 2),
          axis.text.x  = element_text(size = myAxisTextSize - 2, angle = 90, vjust = 0.5, hjust=1)) + 
    labs(title=source,x="") +  scale_x_discrete(labels = wrap_format(20)) +
    scale_fill_manual(values = topLevColors) 
  
  if(!legend) tPlot <- tPlot + theme(legend.position = "none")
  
  tPlot
}

if (myStrata == "Age Group") {
  d1 <- death_age
  d2 <- hosp_age
  d3 <- ed_age }


c.1 <- t.chart(d1,"Deaths")
c.2 <- t.chart(d2,"Hospitalizations")
c.3 <- t.chart(d3,"ED Visits")


# https://wilkelab.org/cowplot/articles/drawing_with_on_plots.html
# https://wilkelab.org/cowplot/articles/plot_grid.html

r1       <- plot_grid(c.1,c.2,c.3,nrow=1)
c.legend <- get_legend(t.chart(death_age,"Deaths",myTopN = 100,legend=TRUE))
title    <- ggdraw() + draw_label(paste("Leading Causes for",mySort)) 

plot_grid(title,r1,c.legend,nrow=3,rel_heights = c(.2,1,.25))



}


