listPosition <- c("nPrimary","nOther","nTotal","percentPrimary","percentOther")

OSHPD.t1   <- oshpd_PDD_any  %>%
 # filter(type == "n_hosp") %>%
 # select(year, county, sex, causeCode, nPrimary = measure) 
   select(year, county, sex, causeCode, nTotal = Nany)

OSHPD.t2   <- oshpd_PDD_primary  %>%
  filter(type == "n_hosp") %>%
  select(year, county, sex, causeCode, nPrimary = measure) 


primary_any <- left_join(OSHPD.t1, OSHPD.t2, by=c("year","county","sex","causeCode")) %>%
  mutate(nPrimary = ifelse(is.na(nPrimary),0,nPrimary)) %>%
  mutate(nOther = nTotal - nPrimary,
         percentPrimary = 100*nPrimary/nTotal,
         percentOther   = 100*nOther/nTotal,
         other          = nOther,
         primary        = nPrimary)


primary_any <- left_join(primary_any,hospCauseLink,by= "causeCode") %>%
  filter(nTotal > 50,
         causeCode != "oo259")  ### TODO need to study this

if (1==2) {
  myPosition <- "nOther"
  myPosition <- "nPrimary"
  myPosition <- "nTotal"
  myPosition <- "percentPrimary"
  myPosition <- "percentOther"
  myCounty  <- "CALIFORNIA"
  myN = 10
  
  
}


anyprimary1 <- function(myCounty = "CALIFORNIA",  myPosition = "nPrimary", myN = 10, mySex = "Total"){
  
  if (myCounty %in% cityLHJs) {
    stop(cityMessage)
  }

  #   
# primary_any_NOPREG <- primary_any %>% filter(birth == "FALSE") %>%
#                        filter(sex=="Total", county == myCounty) 
# 
# 
# plot_data.0 <- primary_any_NOPREG %>% 
#   pivot_longer(cols=other:primary,names_to = "Measure") %>% ungroup()







primary_any_NOPREG <- primary_any %>% 
  filter(sex=="Total", county == myCounty) 


plot_data.0 <- primary_any_NOPREG %>% 
  pivot_longer(cols=other:primary,names_to = "Measure") %>% ungroup()







plot_data.1 <- plot_data.0 %>% 
                 arrange(desc(get(myPosition))) %>%
                 dplyr::slice(1:(myN*2))


myPlot <- ggplot(plot_data.1, aes(x = reorder(causeName,get(myPosition)), y = value, fill=Measure)) + 
  coord_flip() + geom_bar(stat = "identity") + 
    scale_y_continuous(labels = scales::comma) + #numbers shown with commas rather than scientific notation
  scale_x_discrete(labels = scales::wrap_format(50)) +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))
  theme( plot.title.position = "plot",
         plot.title = element_text( margin=margin(0,0,30,0)),
         axis.text.x = element_text(angle = 90, hjust = 1), 
        axis.title=element_blank(), axis.text = element_text(size = rel(1.5)), 
        legend.text = element_text(size = rel(1.2)), legend.title = element_text(size = rel(1.2)))  +
  labs(title=paste("Rankings of PRIMARY and all OTHER Conditions associated with Hospitalizaions, California, ",currentYear_hosp_ed)) 
  

list(plotL = myPlot, dataL = plot_data.1)


}
