# =====================================================================================
# "makeSES.R" file                                                                    |
#            designate folder locations and load packages                             |
#            load packages                                                            |
#                                                                                     |   
# =====================================================================================

# -- Designate locations and load packages-------------------------------------------------

#myDrive <- "E:/0.CBD"  
#myPlace <- paste0(myDrive,"/myCBD-Stable") 
#upPlace <- paste0(myDrive,"/myUpstream") 

 myPlace <- getwd()

#-- LOAD MAIN DATA SET, AND "INFO FILES", BEGIN KEY TRANSFORMATIONS------------------------

HPIdat     <- read.csv(paste0(myPlace,"/myData/HPI2_MasterFile_2018-04-04.csv"),as.is=TRUE)


county.map <- read.csv(paste0(myPlace,"/myInfo/county_crosswalk.csv"),as.is=TRUE)
cbdLinkCA  <- read.csv(paste0(myPlace,"/myInfo/cbdLinkCA.csv"),colClasses = "character")  # file linking MSSAs to census 
comName    <- unique(cbdLinkCA[,c("comID","comName")])                                    # dataframe linking comID and comName

myYear <- 2015

hpi.t1     <- mutate(HPIdat,
                         year         = myYear,
                         geoLev       = "Census Tract",
                         GEOID        = paste0("0",CensusTract),
                         pop          = pop2010,
                         lessBachelor = 100 - bachelorsed,
                         belowPov     = 100 - abovepoverty,
                         hpiScore     = hpi2score,
                         comID        = cbdLinkCA[match(GEOID,cbdLinkCA[,"GEOID"]),"comID"],
                         region       = county.map[match(County_Name,county.map[,"NAME1_PROPER"]),"REGION_HEALTHYCA"],
                         regionF      = as.numeric(as.factor(region))
                     ) %>%
               transform(county=County_Name)


# why are there NA's ?
hpi.t2    <- hpi.t1 %>% group_by(region,county,comID) %>%
  summarize(pop          = sum(pop),
            geoLev       = "Communuity",
            lessBachelor = sum((100 - bachelorsed)*pop2010,na.rm=TRUE)/pop,
            belowPov     = sum((100 - abovepoverty)*pop2010,na.rm=TRUE)/pop,
            hpiScore     = sum(hpi2score*pop2010,na.rm=TRUE)/pop
  )

hpi.t3    <- hpi.t1 %>% group_by(region,county) %>%
  summarize(pop          = sum(pop2010),
            geoLev       = "County",
            lessBachelor = sum((100 - bachelorsed)*pop2010,na.rm=TRUE)/pop,
            belowPov     = sum((100 - abovepoverty)*pop2010,na.rm=TRUE)/pop,
            hpiScore     = sum(hpi2score*pop2010,na.rm=TRUE)/pop
            ) 
  
hpi.t1 <- select(hpi.t1,year,GEOID,county,region,pop,lessBachelor,belowPov,hpiScore)

#load(paste0(myPlace,"/myData/datCounty.R"))

f   <- list(family = "Arial", size = 18, color = "blue")            
pal <- c("red", "blue", "green")

# t.x <- 4
# t.y <- 5

#measVecN <- 1:3
varVecL  <- c("<Bachelors Degree","Below Federal Poverty",'HCI Raw Score')
varVec   <- c("lessBachelor","belowPov","hpiScore") 
names(varVec) <- varVecL

#varVec  <- 1:3
#varVecN <- c("lessBachelor","belowPov","hpiScore") 
#names(varVec) <- varVecN


# https://plotly-book.cpsievert.me/scatter-traces.html
         

ccbSESplot1 <- function(myCause=0, myMeasure = "aRate",myGeo="Community",t.x="lessBachelor"){

t.y <- 4  
xL <-  which(varVec == t.x)
xM <-  which(lMeasures== myMeasure)


temp <- paste0("dat.1$",myMeasure)

if (myGeo=="Census Tract") {hpiT <- hpi.t1
                          dat.1 <- filter(datTract,yearG=="2011-2015",CAUSE==myCause,county != "CALIFORNIA STATE",Level == "gbd36")  
                          temp  <- dat.1[,c("GEOID",myMeasure)]
                          hpiT  <- merge(hpiT,temp,by="GEOID")}

if (myGeo=="Community") {hpiT <- hpi.t2
                       dat.1 <- filter(datComm,yearG=="2011-2015",CAUSE==myCause,county != "CALIFORNIA STATE",Level == "gbd36")  
                       temp  <- dat.1[,c("comID",myMeasure)]
                       hpiT  <- merge(hpiT,temp,by="comID")}
if (myGeo=="County") {hpiT <- hpi.t3
                   dat.1 <- filter(datCounty,year==myYear,CAUSE==myCause,county != "CALIFORNIA STATE",Level == "gbd36")  
                   temp  <- dat.1[,c("county",myMeasure)]
                   hpiT  <- merge(hpiT,temp,by="county")
             
}

  
hpiT <- hpiT[,c("lessBachelor","belowPov","hpiScore",myMeasure,"pop","county","region")]

hpiX <- as.list(hpiT)  
  
# https://plot.ly/r/axes/
  

p <- plot_ly(
     data = hpiT,
     x =  ~  hpiX[[t.x]],
     y =  ~ hpiX[[t.y]],
     type="scatter",mode="markers",
    colors=pal,
    color = as.numeric(as.factor(hpiX[["region"]])),
     size = ~ hpiX[["pop"]]*10,
      hoverinfo = 'text',
      text = ~paste('</br> County',hpiX[["county"]],
                    '</br> Population:',format(hpiX[["pop"]], big.mark = ","), 
                    '</br>',varVecL[xL],":",round(hpiX[[t.x]],1),"%",
                    '</br>',myMeasure,":",round(hpiX[[t.y]],1)) ) %>%
    hide_colorbar() %>% 
layout(title=paste('Association of',varVecL[xL],"and",lMeasuresC[xM],"for",causeList36[causeList36[,1]==myCause,2],"by",myGeo,"in",myYear),
       xaxis = list(title=varVecL[xL],      titlefont = f, showline = TRUE,linewidth = 2),
       yaxis=  list(title=lMeasuresC[xM],titlefont = f, showline = TRUE,linewidth = 2))
p
}



# add_annotations(x = c(100),
#                   y = c(10,15,20),
#                   text = c("ab","c","d"),showarrow = FALSE,
#                   colors=pal,
#                   color=c(1,2,3)) %>% 




# https://plot.ly/r/legend/
