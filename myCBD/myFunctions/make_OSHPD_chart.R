
calculated_metrics <- readRDS(file = path(myPlace, "myData/real/countyOSHPD.rds"))

oshpdPlot <- function(myCounty = "California") {
  calculated_metrics %>% left_join(., fullCauseList, by = c("CAUSE" = "LABEL")) %>% filter(!is.na(CAUSE), Level == "lev2", county == myCounty) %>% filter(sex == "Total") %>%
    group_by(type) %>% mutate(nameOnly = forcats::fct_reorder(nameOnly, filter(., type == "charges") %>% pull(measure))) %>% 
    ggplot(., aes(x = nameOnly, y = measure)) + coord_flip() + geom_bar(stat = "identity") + facet_grid(. ~ type, scales = "free_x") + scale_y_continuous(labels = scales::comma)
  
}

