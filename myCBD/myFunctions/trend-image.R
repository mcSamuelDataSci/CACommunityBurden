# library(directlabels)
# 
# 
# myLHJ="CALIFORNIA"
# myCause="D05"
# myMeasure = "YLL"
# 
# minYear <- 2002
# maxYear <- 2017
# 
# myCex <- 1.6
# myCol <- "blue"            #mycol <- rep("blue",nrow(dat.1))
# 
# inDat <- datCounty
# dat.1 <- filter(inDat,county == myLHJ,CAUSE == myCause)
# 
# 
# # myTit <- paste0("Trend in ",lMeasuresC[lMeasures==myMeasure]," of ",causeList36[causeList36[,"LABEL"]== myCause,"nameOnly"]," in ",myLHJ,", ",minYear," to ",maxYear)
# 
# 
# jpeg(paste0("E:/0  FUSION/Violence/Community Maps/",myCounty,myDeath,".jpg"),2000,2000,quality=100)
# 
# dev.off()
# 
# #jpeg(paste0("E:/0.CBD/myCBD/www/trend.jpeg"),2000,2000,quality=100,pointsize=10)
# 
# theme_set(theme_gray(base_size = 20))
# 
# #ggplot(data=dat.1, aes(x=year, y=eval(parse(text=(myMeasure))), group=sex)) +
# ggplot(data=dat.1, aes(x=year, y=Ndeaths, group=sex, color=sex)) +
#     geom_line(size=4)  +
#      scale_colour_discrete(guide = 'none') + labs(y = myMeasure) +
#      scale_x_continuous(expand=c(0,3)) + scale_y_continuous(limits = c(0, NA)) +
#      geom_dl(aes(label = sex), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1.6, font="bold")) +
#    geom_dl(aes(label = sex), method = list(dl.trans(x = x - 0.2), "first.points", cex = 1.6, font="bold"))
#  
#  
# 
# 
#  
#  
#  +
#       labs(title =myTit) +
#    theme_grey() +
#    theme(
#      plot.title=element_text(family='', face='bold', colour='black', size=18)
#    )
#  
#     
#    
# #    scale_x_continuous(limits = c(0, NA))
#   
#   
#   
# }
