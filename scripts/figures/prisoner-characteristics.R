
#
##
### visualize state prisoner demographics
##
#

## load packages ====

library(tidyverse)
library(gridExtra)

## load data ====

penal_data <- read_csv("data/prisoner_char.csv")

# state prison sentences by race and gender, 1883-1925 ====

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
  ggtitle("State Prison Sentences") +
  theme_bw() +
  theme(axis.title = element_text(size=11),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x  = element_blank(),
        panel.grid.major.y = element_line( size=.05, color="grey" ),
        plot.title = element_text(size=12), 
        plot.caption = element_text(size = 8, hjust=0.5),
        legend.position = "bottom",
        legend.title = element_blank()) +
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
       height = 6, 
       dpi = 650)

# sentences by prisoner place of origin ====

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

place_plot <- origins %>%
  tidyr::gather(key = origin, value = sentences, -year) %>%
  mutate(origin = factor(origin, ordered = TRUE,
                         levels = c("florida",
                                    "georgia",
                                    "southcarolina",
                                    "northcarolina",
                                    "alabama",
                                    "other",
                                    "not_given"))) %>%
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
  guides(fill = guide_legend(reverse=TRUE)) +
  labs(x=NULL, 
       y = NULL, 
       title = "State Prison Sentences by Prisoner's Place of Origin") +
  theme(axis.ticks.y = element_blank(), 
        axis.ticks.x=element_blank()) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x  = element_blank(),
        panel.grid.major.y = element_line( size=.1, color="grey" )) +
  scale_fill_brewer(labels=c("Florida", 
                             "Georgia", 
                             "South Carolina",
                             "North Carolina",
                             "Alabama", 
                             "Other South",
                             "Unrecorded"),
                    palette = "Greys", #"Paired",
                    direction = -1,
                    name = NULL) +
  theme(plot.title = element_text(size=12))

ggsave("figures/figure3-origins.png", 
       place_plot,
       width = 8.25, 
       height = 5.5, 
       dpi = 650)


# pardons by race ====

# calculate pardon rates
penal_data <- penal_data %>% 
  dplyr::filter(year > 1890) %>%
  mutate(p.rate = pardons / total_sentences,
         p.rate.w = pardons_white / (white_male + white_female),
         p.rate.b = pardons_black / (black_male + black_female) ) 

# summary pardon rates by race. 
white.rate = sum(penal_data$pardons_white,na.rm=T) / (sum(penal_data$white_male,na.rm=T) +
                                                        sum(penal_data$white_female,na.rm=T))
black.rate = sum(penal_data$pardons_black,na.rm=T) / (sum(penal_data$black_male,na.rm=T) +
                                                        sum(penal_data$black_female,na.rm=T))
# Pardon rate for whites is more than double that of blacks.
white.rate/black.rate

# number of pardons
plot_raw <- penal_data %>% 
  dplyr::select(year, pardons, pardons_white, pardons_black) %>%
  tidyr::gather(key=race, value=pardons, -year) %>%
  arrange(year) %>%
  mutate(race = factor(race, ordered = TRUE,
                       levels = c("pardons", "pardons_white", "pardons_black"))) %>%
  ggplot(aes(x = year, y = pardons, col = race)) +
  geom_line(lwd=1.25) +
  scale_x_continuous(breaks = seq(1890, 1930, 5),
                     lim =c(1890, 1925)) +
  scale_y_continuous(breaks = seq(0, 175, 25)) +
  labs(y = NULL, legend = "", x = NULL) + 
  scale_color_manual(breaks = c("pardons", "pardons_white", "pardons_black"),
                     values = c('#33a02c', '#1f78b4', '#a6cee3'),
                     labels = c("Total", "White", "Black"),
                     name = NULL) +
  ggtitle("Pardons") +
  theme_bw() + 
  theme(axis.title = element_text(size=10),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x  = element_blank(),
        panel.grid.major.y = element_line( size=.05, color="grey" ),
        plot.title = element_text(size=11.5), 
        plot.caption = element_text(size = 8, hjust=0.5)) +
  theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(0, 130)) 

# pardon rate
plot_rate <- penal_data %>% 
  dplyr::select(year, p.rate, p.rate.w, p.rate.b) %>%
  tidyr::gather(key=race, value=pardons, -year) %>%
  arrange(year) %>%
  mutate(race = factor(race, ordered = TRUE,
                       levels = c("p.rate", "p.rate.w", "p.rate.b"))) %>%
  ggplot(aes(x = year, y = pardons, col = race)) +
  # geom_line(lwd=.5, alpha = .5) +
  scale_x_continuous(breaks = seq(1890, 1930, 5),
                     lim =c(1890, 1925)) +
  scale_y_continuous(breaks = seq(0, 3, .1), 
                     labels= seq(0, 3, .1),
                     lim = c(0, 100)) +
  labs(y = NULL, legend = "", x = NULL) + 
  scale_color_manual(breaks = c("p.rate", "p.rate.w", "p.rate.b"),
                     values = c('#33a02c', '#1f78b4', '#a6cee3'),
                     labels = c("Total", "White", "Black"),
                     name = NULL) +
  ggtitle("Smoothed pardon rates") +
  theme_bw() + 
  theme(axis.title = element_text(size=10),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x  = element_blank(),
        panel.grid.major.y = element_line( size=.05, color="grey" ),
        plot.title = element_text(size=11.5), 
        plot.caption = element_text(size = 8, hjust=0.5)) +
  theme(legend.position = "bottom") +
  stat_smooth( # smooth using cubic natural spline as the basis function
    method="lm", se=TRUE, fill=NA,
    formula=y ~ splines::ns(x, 3)) +
  coord_cartesian(ylim = c(0, .5)) 

pardon_plots <- gridExtra::arrangeGrob(plot_raw, plot_rate, ncol = 2)
ggsave("figures/pardons.png", 
       pardon_plots,
       width = 10, height = 6, dpi = 650)

