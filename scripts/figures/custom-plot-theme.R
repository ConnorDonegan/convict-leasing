

library(tidyverse)
theme_cust <- theme_bw() +
  theme(axis.title = element_text(size=11),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x  = element_blank(),
        panel.grid.major.y = element_line( size=.05, color="grey" ),
        plot.title = element_text(size=12), 
        plot.caption = element_text(size = 8, hjust=0.5),
        legend.position = "bottom",
        legend.title = element_blank())