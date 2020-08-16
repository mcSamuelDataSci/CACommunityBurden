temp <- filter(lifeTableState, sex == "Total",race7 != "MR_NH") %>% select(race7,year,LifeExpectancy = ex) %>%
          mutate(LifeExpectancy = round(LifeExpectancy,1))  %>%
          pivot_wider(names_from = race7,values_from = LifeExpectancy)

write_csv(temp,"CA_life_expectancy.csv")
