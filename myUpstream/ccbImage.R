x <- c(1,2,3,4,5)
y <- c(2,1,2,1,2)
z <- c(1,2,3,4,5)
c <- 1:5
t <- c("Downstream / Conditions","Tools / Teams","Place / Time / Person","Standards","Upstream / SDOH")

source("e:/0.CBD/myCBD/myFunctions/helperFunctions/wrapLabels.R")

work <- as.data.frame(cbind(x,y,z))

                      
library(ggplot2)

ggplot(work, aes(x,y, size = y,label = wrap.it(t,20) ) ) +
  #geom_point(show.legend=F, color=rainbow(5),alpha=.8) +
  geom_point(shape = 21, colour = "black", fill = rainbow(5),show.legend=F,alpha=.8) +
  ylim(0, 3) + xlim(0,6) + scale_size(range = c(50,90)) +
  geom_text(show.legend=F,size=5) +
  theme_void()

