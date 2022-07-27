# take tract out of dropdown - DONE; MADE A NEW DROPDOWN TAGGED myGeo_sdoh
# add very small N to every chart - NOT NOW
# "facet" scatter plot?  and/or "fix" legend? - IN PROGRESS
# restrict data sets and dropdowns to number, crude rate, age-adjusted rate;  maybe not number - DONE, INCLUDED NUMBER  
# ???????  bring in "unsuppressed data"??????? - NOT NOW
## restrict condition list to all county death N >  XXX???? - NOT NOW
# fix warnings?



# =====================================================================================
# "scatterSDOH.R" file                                                                |
#            Social Determiants of Health (SDOH) & Outcome Scatter plot plus          |
#                                                                                     |   
# =====================================================================================

myYear <- 2021
my5Year <- "2017-2021"

f      <- list(family = "Arial", size = 18, color = "blue")  # not used          
pal    <- c("red", "blue", "green")

# mySex=c("Male","Female"),
scatterSDOH <- function(myCause="0", myMeasure = "aRate",  myGeo="Community", mySDOH="pov"){
  
  if(1==2){
    myCause="0"
    myCause="D04"
    myMeasure = "aRate"
    mySex = c("Male","Female")
    mySex="Total"
    myGeo="County"
    myGeo="Community"
    myGeo="Tract"
    mySDOH="pov"
    mySDOH="hpi3score"
  }
  
  
  mySex = c("Male","Female")  
  mySex = c("Total")  
  mySex = c("Male","Female", "Total")
  
  
  if( myGeo %in% c("Community","Census Tract") & myMeasure == "SMR" ) stop('Sorry kid, SMR calculated only for County level')
  
  
  SDOH_long_label     <-  names(which(sdohVec == mySDOH))
  SDOH_short_label    <-  filter(sdohLink, sdohCode == mySDOH) %>% pull(sdohNameShort)
  death_measure_label <-  deathMeasuresNames[which(deathMeasures== myMeasure)]
  death_measure_label <-  names(which(deathMeasures== myMeasure))
  
  
  cause_label <- deathCauseLink[deathCauseLink[,1]==myCause,2]
  
  
  yearLabel <- ifelse(myGeo=="County", myYear, my5Year)
  
  
  
  # if (myGeo=="Census Tract") {
  #   
  #   sdohWork <- sdohTract
  #   dat.1 <- filter(datTract,sex %in% mySex,causeCode==myCause,county != "CALIFORNIA", yearG5 == my5Year)  
  #   temp  <- dat.1[,c("GEOID", "sex", myMeasure)]
  #   sdohWork <- sdohWork %>%
  #     filter(sdohCode == mySDOH) %>%
  #     merge(temp, by = "GEOID") %>%
  #     left_join(select(commInfo, comID, comName), by = "comID") %>%
  #     select(sdohCode, est, myMeasure = {{ myMeasure }}, population, county, region, sex, comName, GEOID) %>%
  #     mutate(sdohText = if(mySDOH == "hpi3score") round(est, 2) else scales::percent(est, accuracy = 0.1),
  #            plotText = paste('GEOID:', GEOID,
  #                             '<br>Community:', comName,  
  #                             '<br>County:',county,
  #                             '<br>Region:', region,
  #                             '<br>Population:',format(population, big.mark = ","), 
  #                             '<br>',!!SDOH_long_label,":",sdohText,
  #                             '<br>',!!death_measure_label,":",round(myMeasure)))
  # }
  
  
  if (myGeo == "Community") {
    
    sdohGeo <- sdohComm %>%
      filter(sdohCode == mySDOH)
    
    sdohGeo$sdoh5 <- cut_number(sdohGeo$est, 5)
    
    dat.1 <- datComm %>%
      filter(sex %in% mySex, causeCode == myCause, county != "CALIFORNIA", yearG5 == my5Year)
    
    sdohWork  <- sdohGeo %>%
      left_join(dat.1[,c("comID", "sex", myMeasure)], by="comID")  %>%
      select(sdohCode, est, myMeasure = {{ myMeasure }}, population, county, region, sex, comName, comID, sdoh5) %>%
      mutate(sdohText = if(mySDOH == "hpi3score") round(est, 2) else scales::percent(est, accuracy = 0.1),
             plotText = paste('Community:', comName,  
                              '<br>County:',county,
                              '<br>Region:', region,
                              '<br>Population:',format(population, big.mark = ","), 
                              '<br>',SDOH_long_label,":", sdohText,
                              '<br>',!!death_measure_label,":",round(myMeasure, 1)))
  }
  
  
  if (myGeo=="County") {
    
    sdohGeo <- sdohCounty %>%
      filter(sdohCode == mySDOH)
    
    sdohGeo$sdoh5 <- cut_number(sdohGeo$est, 5)

    dat.1 <- datCounty %>%
      filter(sex %in% mySex, causeCode == myCause, county != "CALIFORNIA", year == myYear)

    sdohWork <- sdohGeo %>%
      left_join(dat.1[,c("county","sex",myMeasure)],by="county") %>%
      select(sdohCode, est, myMeasure = {{ myMeasure }}, population, county, region, sex, sdoh5) %>%
      mutate(sdohText = if(mySDOH == "hpi3score") round(est, 2) else scales::percent(est, accuracy = 0.1),
             plotText = paste('<br>County:',county,
                              '<br>Region:', region,
                              '<br>Population:',format(population, big.mark = ","), 
                              '<br>',SDOH_long_label,":",sdohText,
                              '<br>',!!death_measure_label,":",round(myMeasure, 1)))
    
  }
  
 
  
  sdohWorkList <- as.list(sdohWork) 
  
  # Used for scatterplot, violin plot, density plot (mortality)
  sdohWork_both  <- filter(sdohWork, sex %in% c("Male","Female"))
  
  # Used for mortality map
  sdohWork_total <- filter(sdohWork, sex == "Total")
  
  # sdohGeo used for sdoh map and density plot (sdoh)
  
  
  # If not male or female data available, then stop
  if (nrow(sdohWork_both)==0) stop("Sorry friend, data are suppressed per the California Health and Human Services Agency Data De-Identification Guidelines, or there are no cases that meet this criteria.")
  
  # if (nrow(sdohWork)==0) stop("Sorry friend, data are suppressed per the California Health and Human Services Agency Data De-Identification Guidelines, or there are no cases that meet this criteria.")
  
  
  # SCATTER PLOT ===================================================================================================================
  
  t1 <- list(
    family = "Arial",
    #face = "bold",
    size = 16, 
    color = "black")
  
  #title= list(text = "Playing with Fonts",font = t1), font=t, 
  
  # Ensures all regions in the dataset are shown in the legend
  t_sdohWork <- sdohWork_both %>%
    filter(!is.na(myMeasure)) %>% # Prevents NA values from showing on scatterplot
    complete(sex, region, fill = list(est = 0, myMeasure = 0))
  
  
  sexes <- unique(t_sdohWork$sex)
  if (length(sexes) == 2){
    sex_legend <- "Male"
    plot_rows <- 2
  } else {
    sex_legend <- sexes
    plot_rows <- 1
  }
  
  my_plot <- . %>% 
    plot_ly(x = ~est,
            y = ~myMeasure, 
            type = "scatter", 
            mode = "markers", 
            colors=pal,
            color = ~region,
            size =  ~population, sizes=c(10,15),
            hoverinfo = 'text',
            text   = ~plotText, 
            showlegend = (~unique(sex) == sex_legend), 
            legendgroup = ~region) %>%
    layout(xaxis = list(title=list(text=paste("<b>",SDOH_short_label, "- CONTINUOUS","</b>"), font =t1), showline = TRUE, linewidth = 2),
           yaxis=  list(title=list(text=""), showline = TRUE, linewidth = 2, rangemode = "tozero"),
           legend = list(y = 0.5)) %>%
    add_annotations(
      text = ~unique(sex),
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "middle",
      yanchor = "top",
      showarrow = FALSE,
      yshift = 20,
      font = list(size = 18)
    )
  
  
  p <- t_sdohWork %>%
    group_by(sex) %>%
    do(p = my_plot(.)) %>%
    subplot(nrows = plot_rows, shareX = TRUE, shareY = TRUE, margin = 0.06) %>%
    layout(margin = list(l = 100), 
           annotations = list(
             list(x = -0.15, y = 0.5, text=paste("<b>",death_measure_label,"</b>"), font = t1, textangle = 270, showarrow = F, xref = 'paper', yref='paper')
           ))
  
  
  
  # 
  # p <-plot_ly(
  #   data = filter(sdohWork, sex %in% c("Male","Female")),
  #   x =    ~est,
  #   y =    ~myMeasure,
  #   type="scatter",
  #   mode="markers",
  #   colors=pal,
  #   color = ~region,
  #   symbol = ~sex,
  #   size =  ~population, sizes=c(10,15),
  #   hoverinfo = 'text',
  #   text   = ~plotText ) %>%
  #   layout(xaxis = list(title=list(text=paste("<b>",SDOH_short_label, "- CONTINUOUS","</b>"), font =t1), showline = TRUE, linewidth = 2),
  #          yaxis=  list(title=list(text=paste("<b>",death_measure_label,"</b>"), font =t1), showline = TRUE, linewidth = 2))
  
  
  tempSize <- 16
  
  sdoh_theme  <- function(){ 
    font <- "Georgia"   #assign font family up front
    
    theme_bw() %+replace%    #replace elements we want to change
      
      
      theme(text = element_text(family = 'Arial'),
            plot.title   = element_text(size = tempSize+4, color=myTitleColor, face = 'bold'),
            axis.title   = element_text(size = tempSize, face="bold"), # was myTextSize2, changed to myAxisSize
            axis.text.y  = element_text(size = tempSize),
            axis.text.x  = element_text(size = tempSize), 
            legend.text = element_text(size = tempSize), 
            legend.title = element_text(size = tempSize),
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            # panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(color = 'black'))
    #draws x and y axis line
    
  }
  
  
  #, axis.line = element_line(color = 'black'))                 
  
  
  # VIOLIN PLOT ==============================================================================================================
  
  vio1 <- ggplot(sdohWork_both,  aes(x=sdoh5, y=myMeasure)) + 
    labs(x=paste(SDOH_short_label, "- QUINTILES"), y=death_measure_label) +
    geom_violin(adjust = 0.7) +
    geom_boxplot(width=0.1, fill = "red") + 
    sdoh_theme() + theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1)) + facet_grid(rows=vars(sex))
  
  
  
  # DENSITY PLOTS ==========================================================================================================
  
  myBinN <- 100
  
  
  hist_part1 <- geom_histogram(aes(y=..density..), col="blue",fill="lightblue", bins=myBinN)
  hist_part2 <- geom_density(col="red", size=1) 
  
  
  hist1 <- ggplot(data= sdohWork_both, aes(x=myMeasure)) + hist_part1 + hist_part2 + labs(x=death_measure_label) + sdoh_theme() + facet_grid(rows=vars(sex))
  
  x_title <- ifelse(mySDOH == "hpi3score", "HPI Score", "Percent")
  
  hist2 <- ggplot(data= sdohGeo, aes(x=est)) + hist_part1 + hist_part2 + labs(x=x_title)       +   sdoh_theme()
  
  
  # MAPS ===================================================================================================================
  
  if (myGeo %in% c("Community")) {
    map.1_mortality      <- left_join(shape_Comm, sdohWork_total, by=c("county","comID"))
    map.1_sdoh <- left_join(shape_Comm, sdohGeo, by = c("county", "comID"))
  }
  
  if(myGeo == "County")  {
    map.1_mortality <- left_join(shape_County, sdohWork_total, by = "county")
    map.1_sdoh     <- left_join(shape_County, sdohGeo, by=c("county")) 
  }
  
  
  map1 <-  tm_shape(map.1_mortality) + 
    tm_borders() + 
    tm_fill("myMeasure", title=death_measure_label) +
    tm_layout(frame=F) 
  
  
  map2 <-  tm_shape(map.1_sdoh) + 
    tm_borders() + 
    tm_fill("est", title=SDOH_short_label) +
    tm_layout(frame=F) 
  
  
  
  list(p = p, violin1 = vio1, hist1 = hist1, hist2 = hist2, map1 = map1, map2 = map2)
  
  
  
}
