
server <- T
if (!server) source("g:/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")
if (server) source("/mnt/projects/FusionData/0.CCB/myCCB/Standards/FusionStandards.R")


# --Global constants and settings-----------------------------------

myYear_death   <-  2022 # used for deaths
myYear_pdd <- 2022  # used for hosp

ccbChangeYear <- myYear_death - 10

mySex     <-  "Total"

# --CCB DEATH DATA ------------------------------------------------

ccbDataX     <- readRDS(paste0(ccbData,"real/datCounty.RDS")) %>%
  filter(Level == "lev2", causeCode != "Z01") %>%
  left_join(deathCauseLink ,by="causeCode")       

# Sex-specific cancers
# Need to change to datCounty_RE_1year
# ccbDataX_cancer_male <- ccbDataX %>% 
#   filter(causeNameShort %in% cancer_male, sex == "Male") %>% 
#   mutate(sex == "Total")
# 
# ccbDataX_cancer_female <- ccbDataX %>% 
#   filter(causeNameShort %in% cancer_female, sex == "Female") %>% 
#   mutate(sex == "Total")
# 
# ccbDataX <- ccbDataX %>% 
#   filter(!causeNameShort %in% c(cancer_female, cancer_male)) %>% 
#   bind_rows(ccbDataX_cancer_female, ccbDataX_cancer_male)


ccb         <- filter(ccbDataX,year==myYear_death,sex==mySex)

ccbDeaths   <- ccb %>%
  mutate(measure = Ndeaths,
         mValues = causeNameShort)

ccbYLL      <- ccb %>%
  mutate(measure = YLLper,
         mValues = causeNameShort)



ndeathsCols <- paste0("Ndeaths", c(ccbChangeYear, myYear_death))
rateCols <- paste0("rate", c(ccbChangeYear, myYear_death))

ccbChange   <- filter(ccbDataX,year %in% c(ccbChangeYear,myYear_death), sex==mySex) %>% 
  select(county,year,causeNameShort,Ndeaths, aRate) %>%
  rename(rate = aRate) %>%
  pivot_wider(names_from = year, values_from = c("Ndeaths", "rate"), names_sep = "") %>%
  rename(rate10 = !!as.symbol(rateCols[1]), rate0 = !!as.symbol(rateCols[2]), 
         Ndeaths10 = !!as.symbol(ndeathsCols[1]), Ndeaths0 = !!as.symbol(ndeathsCols[2]))

ccbChange      <- ccbChange %>%
  mutate(change = round(100*(rate0-rate10)/rate10,1))%>%
  filter(!(is.na(rate0) | is.na(rate10))) %>% # exclude if either is 0 -- check
  mutate(measure=change,
         mValues = causeNameShort) 
# mutate(mValues = ifelse(measure < 0,NA,mValues))


# --CCB RACE DATA ---------------------------------------------------


ccbRace <-  readRDS(paste0(securePlace,"myData/Exploratory/ccbRaceDisparity.RDS")) %>%
  left_join(select(deathCauseLink, -causeName, -causeNameShort) ,by="causeCode")   %>%
  mutate(measure=round(rateRatio,1),
         mValues = causeNameShort)


# -- CID DATA ------------------------------------------------


cidData     <- read_csv(paste0(ccbUpstream,"CID/dcdcData.csv"))

cidData <- cidData %>%
  group_by(Branch) %>%
  filter(Year == max(Year)) %>%
  ungroup() %>%
  mutate(county = County,
         measure=Cases,
         mValues = Disease)


# -- HOSPITALZATION DATA -----------------------------------------------



edData  <- readRDS(paste0(ccbData,"real/age_race_focus_data/hosp_ED_year.RDS")) %>%
  left_join(hospCauseLink, by="causeCode") %>%
  filter(year==myYear_pdd) %>% 
  mutate(measure = n_ED,
         mValues = causeNameShort)


hospData <- readRDS(paste0(ccbData,"real/age_race_focus_data/hosp_ED_year.RDS")) %>%
  left_join(hospCauseLink, by="causeCode") %>%
  filter(year==myYear_pdd) %>% 
  mutate(measure = n_hosp,
         mValues = causeNameShort)





# -- IMHE DATA -----------------------------------------------


# myLevel <- c('2','2,3,4')
#myLevel <- c(2, 3)

# dataIHME     <- readRDS(paste0(ccbData,"v2IHME.RDS"))
# 
# dat.YLD.cause <- dataIHME %>%  filter(measure_id ==  3,    #YLD  
#                                       year_id    == 2017,
#                                       display    == "cause",
#                                       level      %in% myLevel,
#                                       sex_id     == 3,   # Both
#                                       metric_id  == 3)  %>%    # Rate 
#                                mutate(measure = val,
#                                mValues = id_name)

# dataIHME     <- read_csv(paste0(ccbUpstream,"IHME/IHME_manual.csv"))
# 
# dat.YLD.cause <- dataIHME %>%  filter(measure_id ==  3,    #YLD  
#                                       year    == 2019,
#                                       display    == "cause",
#                                       #    level      %in% myLevel,
#                                       sex_id     == 3,   # Both
#                                       metric_id  == 3)  %>%    # Rate 
#   mutate(measure = round(val, 3),
#          mValues = cause_name)
# 
# 
# 
# #dataIHME     <- readRDS(paste0(ccbData,"v2IHME.RDS"))
# 
# 
# dat.DALY.risk <- dataIHME %>%  filter(measure_id ==  2,    #DALY  
#                                       year    == 2019,
#                                       display    == "risk",
#                                       #   level      %in% myLevel,
#                                       sex_id     == 3,   # Both
#                                       metric_id  == 3)  %>%  # Rate
#   mutate(measure = round(val, 3),
#          mValues = rei_name)

xSheets <- c("measure", "sex", "age", "metric", "cause", "risk")
ihmeLink <- lapply(xSheets, function(x) readxl::read_excel(paste0(ccbInfo, "ihmeLink.xlsx"), sheet = x))
names(ihmeLink) <- xSheets

dat.YLD.cause <- readRDS(paste0(ccbData, "ihme_cause.RDS")) %>% 
  left_join(ihmeLink$measure) %>% 
  left_join(ihmeLink$sex) %>% 
  left_join(ihmeLink$age) %>% 
  left_join(ihmeLink$metric) %>% 
  left_join(ihmeLink$cause) %>% 
  select(!ends_with("id")) %>% 
  filter(measure_name == "YLDs (Years Lived with Disability)", 
         sex_name == "Both", 
         age_name == "All ages", 
         metric_name == "Rate", 
         level == "lev2", 
         year == 2021) %>% 
  mutate(measure = round(val, 3),
         mValues = cause_name)


dat.DALY.risk <- readRDS(paste0(ccbData, "ihme_risk.RDS")) %>% 
  left_join(ihmeLink$measure) %>% 
  left_join(ihmeLink$sex) %>% 
  left_join(ihmeLink$age) %>% 
  left_join(ihmeLink$metric) %>% 
  left_join(ihmeLink$risk) %>% 
  select(!ends_with("id")) %>% 
  filter(measure_name == "DALYs (Disability-Adjusted Life Years)", 
         sex_name == "Both", 
         age_name == "All ages", 
         metric_name == "Rate", 
         level == "lev2", 
         year == 2021) %>% 
  mutate(measure = round(val, 3),
         mValues = rei_name)




# --APP Constants ------------------------------------------------------


countyList  <- sort(as.character(unique(ccbDataX$county)))

BAR_WIDTH <-  0.9
PLOT_WIDTH_MULTIPLIER <- 1.0

plot_title <- c("Deaths",
                "Years of Life Lost",
                "Increase in Deaths",
                "Race Disparity in Deaths",
                "Number of Hospitalizations",  
                "Number of ED Visits",
                "Reportable Disease Cases",
                "Years Lived with Disability",
                "Risk Factors" 
)

metric <-     c("Number",
                "Rate",
                "Percent",
                "Rate Ratio",
                "Number",
                "Number",
                "Number",
                "Rate",
                "Rate")

dataSets   <- list(ccbDeaths, ccbYLL,   ccbChange, ccbRace, hospData, edData, cidData , dat.YLD.cause, dat.DALY.risk)

#ourColors <-    c("#8F98B5", "#E9A291", "#E9A291","#8ECAE3", "#E6C8A0","#8F98B5","#E9A291",   "#8F98B5","blue")



# https://stackoverflow.com/questions/50600425/r-convert-colors-to-pastel-colors
a <-c("red","red1","red2","red3","grey","darkgreen","skyblue","blue","magenta","magenta4","yellow","orange","pink","pink")
# transform to rgb
a1 <- col2rgb(a)
# transform to HSV space
a2 <- rgb2hsv(a1)
# calculate hue for HCl
hue <- a2["h",]*360
# create color with suitable chroma and luminance to get pastel color
a3 <- hcl(hue, 35, 85)

#barplot(seq_along(a), col=a3, main="Pastel_hcl")

ourColors <- a3[c(5,6,7,8,9,11,12,14, 1)]



#==========================================================================================================================

plotMeasures <- function(IDnum=4, myCounty = "Los Angeles",myObserv = 10, decrease = F){ 
  
  
  lblwrap <- function (x,L) { # x=object, L=desired character length
    sapply(lapply(x, strwrap, L),paste, collapse = "\n   ")
  }
  
  # Jaspo
  if (IDnum == 3 & decrease) plot_title[IDnum] <- "Decrease in Deaths"
  
  myObserv=as.numeric(myObserv)
  
  if(1==2){
    IDnum=3
    myCounty = "Butte"
    myObserv = 10
  }  
  
  
  SHOW_TOP <- myObserv  
  tSize1   <- #round(((2e-07)*(myObserv^4))-((6e-05)*(myObserv^3))+
    #        0.007*(myObserv^2)-(0.3276*myObserv)+8.4091)
    round((0.02)*(myObserv^2)-(0.5*myObserv)+7)
  #4
  tSize2   <- round(((2e-07)*(myObserv^4))-((5e-05)*(myObserv^3))+
                      0.0041*(myObserv^2)-(0.144*myObserv)+4.7105)
  #3
  tSize3   <- 3
  
  
  
  if(IDnum %in% 1:7)  work.dat  <- filter(dataSets[[IDnum]],county==myCounty)
  if(IDnum %in% 8:9)  work.dat  <-        dataSets[[IDnum]]                 
  
  
  test <- data.frame(xrow=1:SHOW_TOP)
  
  
  # work.dat <- work.dat %>%
  #   mutate(rankX = rank(-measure))   %>%
  #   filter(rankX <= SHOW_TOP)   %>%
  #   arrange(rankX) %>%
  #   mutate(xrow = row_number()  ) %>%
  #   full_join(test,by="xrow")    %>%
  #   mutate(xValues = ifelse(is.na(mValues),xrow,paste(xrow,mValues)))  %>%
  #   mutate(xSize1 =ifelse(is.na(mValues),0.01,tSize1),   #5
  #          xSize2 =ifelse(is.na(mValues),0.01,tSize2),   #3
  #          xSize3 =ifelse(is.na(mValues),0.01,tSize3),   #2.5
  #   )  %>%
  #   mutate(measure=ifelse(is.na(mValues),0,measure))  %>%
  #   arrange(xrow)
  
  # Jaspo
  work.dat <- work.dat %>%
    mutate(rankX = if(decrease) rank(measure, ties.method = "first") else rank(-measure, ties.method = "first")) %>% # If xValues are tied, first xValue will be ranked higher
    filter(rankX <= SHOW_TOP)   %>%
    arrange(rankX) %>%
    mutate(xrow = row_number()  ) %>%
    full_join(test,by="xrow")    %>%
    mutate(xValues = ifelse(is.na(mValues),xrow,paste(xrow,mValues)))  %>%
    mutate(xSize1 =ifelse(is.na(mValues),0.01,tSize1),   #5
           xSize2 =ifelse(is.na(mValues),0.01,tSize2),   #3
           xSize3 =ifelse(is.na(mValues),0.01,tSize3),   #2.5
    )  %>%
    mutate(measure=ifelse(is.na(mValues),0,measure))  %>%
    arrange(xrow)
  
  
  # if (IDnum == 4) work.dat <- mutate(work.dat,xValues=paste0(xValues,"      (",raceCode,":",lowRace,")"))
  if (IDnum == 4) work.dat <- mutate(work.dat,xRaceValue=paste0("(",raceCode,":",lowRace,")"))
  if (IDnum == 4) myYear <- "2018-2020"
  if (IDnum == 3) myYear <- "2010 to 2020"
  
  plot_width <- max(work.dat$measure)*PLOT_WIDTH_MULTIPLIER
  # Jaspo
  if (decrease) plot_width <- max(abs(work.dat$measure))*PLOT_WIDTH_MULTIPLIER
  
  # Rounding, commas
  if (IDnum %in% c(1, 5, 6, 7)) work.dat <- mutate(work.dat, measureText = scales::comma(measure, accuracy = 1)) else work.dat <- mutate(work.dat, measureText = scales::comma(measure, accuracy = 0.1))
  
  # tPlot <-  
  #   ggplot(data=work.dat, aes(x=reorder(xValues, -xrow),y=measure)) +
  #   coord_flip() +
  #   geom_bar(position="dodge", stat="identity", width=BAR_WIDTH, fill=ourColors[IDnum])   +
  #   geom_text(hjust=0, y=0, label=lblwrap(paste0(work.dat$xValues),ifelse(SHOW_TOP<13,38,48) ),
  #             size=work.dat$xSize1, lineheight = 0.7) +  # , size=xSize
  #   annotate(geom="text", hjust=1, x=work.dat$xValues, y=plot_width, label=work.dat$measure,size=work.dat$xSize2) +
  #   theme(panel.grid.major=element_blank(),
  #         panel.grid.minor=element_blank(),
  #         panel.background=element_blank(),
  #         axis.title.x=element_blank(),
  #         axis.text.x=element_blank(),
  #         axis.ticks.x=element_blank(),
  #         axis.title.y=element_blank(),
  #         axis.text.y=element_blank(),
  #         axis.ticks.y=element_blank(),
  #         legend.position="none",
  #         # panel.border = element_rect(colour = "gray", fill=NA, size=1),
  #         plot.title=element_text(size=20, face="bold", vjust=-4),                 # size units?
  #         plot.subtitle=element_text(size=16, face="bold", hjust=1, vjust=-2)
  #   ) +
  #   labs(title=paste(plot_title[IDnum]), subtitle=metric[IDnum])  +
  #   scale_y_continuous(expand = c(0,0), limits = c(0, plot_width))
  
  # Jaspo
  
  tPlot <-  
    ggplot(data=work.dat, aes(x=reorder(xValues, -xrow),y=measure)) +
    coord_flip() +
    geom_bar(position="dodge", stat="identity", width=BAR_WIDTH, fill=ourColors[IDnum])   +
    geom_text(hjust= if (decrease) 1 else 0, 
              y= if (decrease) -.02 else 0, 
              label=lblwrap(paste0(work.dat$xValues), ifelse(SHOW_TOP<13,38,48) ),
              size=work.dat$xSize1, 
              lineheight = 0.7) +  # , size=xSize
    annotate(geom="text", 
             hjust= if (decrease) 0 else 1, x=work.dat$xValues, 
             y= if (decrease) -plot_width else plot_width, 
             label=work.dat$measureText,size=work.dat$xSize2) +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.background=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.position="none",
          # panel.border = element_rect(colour = "gray", fill=NA, size=1),
          plot.title=element_text(size=20, face="bold", vjust=-4, hjust = if (decrease) 1 else 0),                 # size units?
          plot.subtitle=element_text(size=16, face="bold", hjust= if (decrease) 0 else 1, vjust=-2)
    ) +
    labs(title=paste(plot_title[IDnum]), subtitle=metric[IDnum])  +
    scale_y_continuous(expand = c(0,0), limits = if (decrease) c(-plot_width, 0) else c(0, plot_width))
  
  if (IDnum == 4) {
    tPlot <- tPlot + geom_text(hjust=0, aes(x=xValues,y=plot_width*.72, label=paste0(xRaceValue)),size=work.dat$xSize3)
  }
  
  tPlot + theme(plot.margin = ggplot2::margin(0,0,0,0,"cm"))
  
}

#==========================================================================================================================




save(dataSets, ourColors, plot_title, metric, plotMeasures, file = paste0(ccbData,"real/burdenViewData.RData"))





