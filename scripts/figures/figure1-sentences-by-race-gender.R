

library(tidyverse)
source("scripts/figures/custom-plot-theme.R")

## load data

penal_data <- read_csv("data/prisoner_char.csv")

# state prison sentences by race and gender, 1883-1925

sentencing_plot <- penal_data %>%
  gather(key = rage, value = prisoners, -year) %>%
  mutate(rage = 
           factor(rage, ordered = TRUE, 
                  levels = c("black_male", "white_male", "black_female", "white_female"))
  ) %>%
  filter(year > 1882 & year < 1925) %>%
  na.omit() %>%
  ggplot( aes(year, prisoners, group = rage)) +
  geom_line( aes(year, prisoners, colour=rage), 
             lwd = .5,
             show.legend = FALSE,
             stat="identity") +
  geom_point(aes(year, prisoners, shape = rage),
             col = "black",
             size = 2.5) +
  scale_x_continuous(breaks = seq(1885, 1925, 5), 
                     labels=seq(1885, 1925, 5),
                     name = NULL) +
  scale_y_continuous(breaks = seq(0, 500, 50),
                     name = NULL) +
  # ggtitle("State prison sentences") +
  theme_cust +
  coord_cartesian(ylim = c(0, 415))  +
  scale_shape_manual(breaks = c("black_male", "white_male", 
                                "black_female", "white_female"),
                     labels = c("Black male", "White male",
                                "Black female", "White female"),
                     values = 1:4) +
  scale_color_manual(values = rep("gray25", 4)) 

ggsave("figures/figure1-sentences-by-race-gender.png", 
       sentencing_plot,
       width = 7.5, 
       height = 5.66, 
       dpi = 650)
