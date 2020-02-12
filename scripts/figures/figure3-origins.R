

# sentences by prisoner place of origin

library(tidyverse)
library(gridExtra)
library(viridis)
source("scripts/figures/custom-plot-theme.R")

penal_data <- read_csv("data/prisoner_char.csv")

origins <- dplyr::select(penal_data,
                         year, florida, alabama, georgia, northcarolina, southcarolina,
                         other_south, other_regioncountry, not_given) %>%
  mutate(other = other_south + other_regioncountry) %>%
  dplyr::select(-c(other_regioncountry, other_south))

year.col <- which(names(origins)=="year")
total_sents <- rowSums(origins[, -year.col], na.rm=T)
origins[, -year.col] <- purrr::map_df(origins[,-year.col], function(x){
  x/total_sents
} )

place_names <- c("florida",
                 "georgia",
                 "southcarolina",
                 "northcarolina",
                 "alabama",
                 "other",
                 "not_given")

leg_labels <- c("Florida",
                "Georgia",
                "South Carolina",
                "North Carolina",
                "Alabama",
                "Other",
                "Not recorded")

place_plot <- origins %>%
  tidyr::gather(key = origin, value = sentences, -year) %>%
  # na.omit %>%
  mutate(origin = factor(origin, ordered = TRUE,
                         levels = place_names)) %>%
  arrange(origin) %>%
  ggplot() +
  geom_area(aes(year, sentences, fill = origin),
            stat="identity",
            alpha = .8,
            position = position_stack(reverse = T)) + 
  scale_x_continuous(breaks = seq(1885, 1925, 5),
                     lim = c(1885, 1925)) +
  scale_y_continuous(breaks = seq(0, 100, .1),
                     labels = scales::percent,
                     name = NULL) +
  guides(fill = guide_legend(reverse=FALSE)) +
  labs(x=NULL, 
      y = NULL#,
       # title = "State prison sentences by prisoner's place of origin"
       ) +
  theme(axis.ticks.y = element_blank(), 
        axis.ticks.x=element_blank()) +
  theme_cust +
  scale_fill_viridis_d(breaks = place_names,
                    labels = leg_labels,
                  #  palette = "Paired", #"Greys", 
                   # direction = -1,
                    name = NULL)

dir <- "~/repo/convict-leasing/preprint/figures"
ggsave(#"figures/figure3-origins.png", 
       file.path(dir, "figure3-origins.png"),
       place_plot,
       width = 8.25, 
       height = 5.5, 
       dpi = 650)
