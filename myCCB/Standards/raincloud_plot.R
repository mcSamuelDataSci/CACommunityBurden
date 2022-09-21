# Raincloud theme adapted from code written by GitHub user benmarwick: https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R
# GLD modified original code on 07/22/2022 to fix y-axis scaling issues with facet_grid

raincloud_plot <- function(g){
  g <- g + geom_point(colour="#999999",shape=21,fill='white',position = position_jitter(width = .15), size = 1) +
    geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = 1) +
    geom_boxplot(width = .1, outlier.shape = NA) +
    scale_colour_manual(values=paletteCB) +
    scale_fill_manual(values=paletteCB) +
    stat_summary(fun.data=mean_cl_boot,position=position_nudge(x = 0.3),geom="pointrange",size=0.25,shape=21,fill='white') +
    raincloud_theme 
  return(g)  
}

# Theme elements below can be customized as desired
raincloud_theme <- theme(
                        text = element_text(size = 10),
                        axis.title.x = element_blank(),
                        axis.title.y = element_text(size = 14),
                        axis.text = element_text(size = 12),
                        strip.background = element_rect(color = "black", fill="white"),
                        strip.text = element_text(size=10),
                        legend.title = element_blank(),
                        legend.text = element_text(size = 12),
                        legend.direction = "horizontal",
                        legend.position = "none",
                        plot.title = element_text(lineheight = .8, face = "plain", size = 14, hjust=0),
                        panel.border = element_blank(),
                        axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
                        axis.line.y = element_line(colour = "black", size = 0.5, linetype = "solid"))

# somewhat hackish solution to:
# https://twitter.com/EamonCaddigan/status/646759751242620928
# based mostly on copy/pasting from ggplot2 geom_violin source:
# https://github.com/hadley/ggplot2/blob/master/R/geom-violin.r

library(ggplot2)
library(dplyr)

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     #ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)
            
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data, xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )


### Example:
ggplot(diamonds, aes(cut, carat)) +
  geom_flat_violin() +
  coord_flip()