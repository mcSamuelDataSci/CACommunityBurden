x <- c(1,2,3,4,5)
y <- c(2,1,2,1,2)
z <- c(1,2,3,4,5)
#t <- c("SDOH","pop","Cases","standards","Deaths")


work <- as.data.frame(cbind(x,y,z))

                      
library(ggplot2)

ggplot(work, aes(x,y, size = y)) +
  geom_point() +
  ylim(1, 2)
