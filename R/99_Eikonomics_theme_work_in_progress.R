# this is my theme, boring and work in (not so much) progress
library(tidyverse)
library(scales)

eikonomics_colors <- c("#073b4c","#ef476f", "#118ab2", "#ffd166", 
                              "#06d6a0","#35B779FF", "#21908CFF")

show_col(eikonomics_colors)

scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = eikonomics_colors)
}


# plot <- ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = cty, color = class))

theme_eikonomics <- 
  theme(
  panel.background = element_rect(fill = "white", colour = "White", size=2),
  axis.text = element_text(size = 14), 
  legend.text = element_text(size = 14),
  axis.title = element_text(size = 16, face = "bold"),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank())



