
anyprimary1 <- function(myCounty = "CALIFORNIA",  myMeasure = "Nother", mySex = "Total"){
  

countyOSHPD.t   <- oshpd_PDD  %>%
                    filter(type == "n_hosp") %>%
                    select(year, county, sex, ccsCode=ccs_diagP,Nprimary=measure)  %>% 
                    mutate(ccsCode = str_pad(ccsCode, 5,"left",pad="o"))   ### FIX THIS



primary_any <- left_join(oshpd_PDD_any.t,countyOSHPD.t,by=c("year","county","sex","ccsCode")) %>%
                       mutate(Nprimary = ifelse(is.na(Nprimary),0,Nprimary)) %>%
                       mutate(Nother = Nany - Nprimary,
                              percentPrimary = 100*Nprimary/Nany,
                              percentOther   = 100*Nother/Nany)

ccsMap  <- ccsLinker()

primary_any <- left_join(primary_any,ccsMap,by="ccsCode") %>%
                 filter(Nany > 50,
                        ccsCode != "oo259")  ### TODO need to study this

primary_any_NOPREG <- primary_any %>% filter(!(birth))


plot_data.0 <- primary_any_NOPREG %>% 
                 pivot_longer(cols=Nany:percentOther,names_to = "theMeasure") %>%
                 mutate(theMeasure = as.factor(theMeasure))


plot_data.1 <- plot_data.0 %>% filter(sex=="Total")


myN <- 10

if (1==2) {
myMeasure <- "Nother"
myMeasure <- "Nany"
myMeasure <- "Nprimary"
myMeasure <- "percentOther"
myCounty  <- "CALIFORNIA"
}


#create a vector of CAUSE for top N
theseCodes <- plot_data.1 %>%
  filter(county == myCounty) %>%
  group_by(theMeasure) %>% arrange(desc(value)) %>% dplyr::slice(1:myN) %>% #this selects the top N rows for myOSHPDtype
  filter(theMeasure == myMeasure) %>% ungroup() %>% pull(ccsCode)

#creates dataframe with data only for CAUSEs from theseCodes, i.e. the top N CAUSES for the specified theseCodes
plot_data.2 <-     plot_data.1 %>%
  filter(!is.na(ccsName), county == myCounty, ccsCode %in% theseCodes) %>%
  #filter(sex == mySex, year == myYear) %>%
  group_by(theMeasure)    %>%
  mutate(ccsName = forcats::fct_reorder(ccsName, filter(., theMeasure == myMeasure)  %>%
                                           pull(value)))

ggplot(plot_data.2, aes(x = ccsName, y = value)) + 
  coord_flip() + geom_bar(stat = "identity", fill = "blue") + 
  facet_grid(. ~ theMeasure, scales = "free_x")  +
  scale_y_continuous(labels = scales::comma) + #numbers shown with commas rather than scientific notation
  scale_x_discrete(labels = scales::wrap_format(50)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

}
