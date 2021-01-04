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

p.chart <- function(dataSet,source,myTopN=10,legend=FALSE) {
  
  t.dat   <-  dataSet %>% 
    filter(MAINSTRATA %in% myAge, county == myCounty) %>% 
    arrange(-measure) %>% 
    slice(1:myTopN)
  
  tPlot <- ggplot(data=t.dat, aes(x=reorder(causeName,measure), y=measure, fill=topLev)) + 
    geom_bar(stat="identity") + coord_flip() + 
    theme(legend.position="bottom", 
          axis.text.y  = element_text(size = myAxisTextSize - 2),
          axis.text.x  = element_text(size = myAxisTextSize - 2, angle = 90, vjust = 0.5, hjust=1)) + 
    labs(title=source,x="") +  scale_x_discrete(labels = wrap_format(20)) +
    scale_fill_manual(values = topLevColors) 
  
  if(!legend) tPlot <- tPlot + theme(legend.position = "none")
  
  tPlot
}


c.1 <- p.chart(death_age,"Deaths")
c.2 <- p.chart(hosp_age,"Hospitalizations")
c.3 <- p.chart(ed_age,"ED Visits")

# https://wilkelab.org/cowplot/articles/drawing_with_on_plots.html
# https://wilkelab.org/cowplot/articles/plot_grid.html

r1       <- plot_grid(c.1,c.2,c.3,nrow=1)
c.legend <- get_legend(p.chart(death_age,"Deaths",myTopN = 100,legend=TRUE))
title    <- ggdraw() + draw_label(paste("Leading Causes for",myAge)) 

p <- plot_grid(title,r1,c.legend,nrow=3,rel_heights = c(.2,1,.25))






#  myData -->  mySource  
#  myStrata --> 


#  if   hosp and age   then  myDataSet <- "hosp_age"
#  if




MEASURES....

myDataSet  <- "countyDat.RDS"

if (myData == anything....)  {
  county.Dat  %>% ... measure....  pivot longer....
  myStrata <- newMeasureColumn...
}




