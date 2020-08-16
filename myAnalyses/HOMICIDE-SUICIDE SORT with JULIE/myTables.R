
# https://cran.r-project.org/web/packages/summarytools/vignettes/Introduction.html
# https://cran.r-project.org/web/packages/summarytools/vignettes/Recommendations-rmarkdown.html





library(tidyr)
library(dplyr)


x <- c(1,1,2,3,4,4,NA,4,1,2)
a <- c("a","a","c","a","a","b","b","c","c","a")
dat <- bind_cols(this=x,that=a)


t1 <- function(dat,var1,var2) {
  dat %>%
  group_by(var,var2) %>%
  summarise(n=n())%>%
  spread(var2, n)
}
  

t1(dat,"this","that")


# %>%
#  kable()

t0 <- function(var1,var2) {
table(var1,var2,useNA = "ifany")
}

t0(dat$this,dat$that)



# http://analyticswithr.com/contingencytables.html