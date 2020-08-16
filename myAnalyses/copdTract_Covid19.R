

copdPlusTract <- read_csv("F:/0.CBD/myCBD/myData/real/analysisDataSets/Tract.csv")  %>%
                    filter(CAUSE %in% c("0","D66", "A07", "D68"),sex=="Total") %>%
                    select(-oDeaths,-YLL.adj.rate)


write_csv(copdPlusTract,"copdPlusTract.csv")
