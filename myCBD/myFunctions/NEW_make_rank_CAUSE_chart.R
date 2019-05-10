
rankCause2  <- function(myLHJ="Amador",myMeasure = "aRate",myYear=2017,mySex="Total",myLev="lev1",myN=10) {
  
  myCex <- 1.6
  myCol <- "blue"            #mycol <- rep("blue",nrow(filtered.df))
  bLwd <- 2
  
  
  if(myLev=="lev3") myLev <- c("lev2","lev3")
  
  filtered.df <- filter(datCounty,county==myLHJ,year==myYear,sex==mySex,Level %in% myLev,CAUSE !=0)
  
  
  Nrows.df          <- nrow(filtered.df)
  Nrows.to.display  <- min(Nrows.df,myN) 
  filtered.df       <- filtered.df[((Nrows.df-Nrows.to.display+1):Nrows.df),]

  
# works below here  
# =============================================================================  
  
  
  
if (1==2) {
myCounty = "Alameda"
myMeasure = "Number of deaths"
mySex = "Total" 
myYear = 2017
}

xMeasures <- lMeasuresShort[c(1,3,2,4,5)]

library(tidyr)
temp <- datCounty %>% gather(key = "type", value = "measure", Ndeaths,YLLper,aRate,mean.age,SMR)  %>% 
                      select(year,sex,Level,CAUSE,county,type,measure) %>%
                      mutate(type = factor(type,levels=xMeasures ))    %>%
                      mutate(type = plyr::revalue(type, xJunk))    %>% #replaces values with full name labels
                      left_join(., fullCauseList, by = c("CAUSE" = "LABEL"))  



## oshpdPlot <- function(myCounty = "Alameda", myMeasure = "Number of deaths", mySex = "Total" ) {
         
   plotData <-     temp %>%
                   filter(!is.na(CAUSE), Level == "lev2", county == myCounty) %>% 
                   filter(sex == mySex, year == myYear) %>%
                   group_by(type)    %>% 
                   mutate(nameOnly = forcats::fct_reorder(nameOnly, filter(., type == myMeasure)  %>% 
                   pull(measure)))  
  
   
 xtemp <-     ggplot(plotData, aes(x = nameOnly, y = measure)) + 
      coord_flip() + geom_bar(stat = "identity", fill = "blue") + 
      facet_grid(. ~ type, scales = "free_x", labeller=labeller(type = label_wrap_gen(5))) + 
      theme_bw() + #need to specify theme first, before changing/removing axis titles/labels etc. If theme_bw() is put at end, it negates all of these changes
      scale_y_continuous(labels = scales::comma) + #numbers shown with commas rather than scientific notation
      scale_x_discrete(labels = scales::wrap_format(10)) + #x-axis is condition label--wrapping text so it stacks on top of each other
    #within theme--x and y axis refer to the way it looks, with coord_flip(), so y refers to vertical label (which technically is really x axis) and vice versa with x axis
    theme(axis.title.y = element_blank(), #removes nameOnly label
          axis.title.x = element_blank(), #removes measure label
          axis.text.y = element_text(size = 15), #increases size of disease condition labels
          axis.text.x = element_text(size = 10, face="bold"), #controls size of measure labels
          strip.text.x = element_text(size = 15)) #increases the size of the facet labels
# }

 xtemp
 
ggplotly(xtemp)


 
################################################################### 
 
}
 
 
 
#Other option: facet_grid(labeller=labeller(type = hospDiscMeasuresShort))--label at the end, however, this prevents wrapping of strip heading text for facets (can only do one or the other)