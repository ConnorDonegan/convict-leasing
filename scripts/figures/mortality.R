

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Compare age distribution of deaths among prisoners (1905-1909) to 
# that of whites and blacks in the Florida general population (1905) 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# The mortality rate among prisoners (2.2% after 1901) was consistently over three times greater than the mortality rate of the
# Florida general population as meausred in 1905 (0.66%). 

# Sources: General population deaths: The Ninth Biennial Report of the Commissioner of Agriculture - 
# State of Florida for the Period Beginning January 1, 1905, and Ending December 31, 1906.

# The age of prisoners who died in custody was recorded in the Biennial Reports for the years
# 1905 through 1909.

library(tidyverse)

# mortality by age ====

population.deaths <- tibble(
  lwr = c(0, 1, 5, 15, 25, 35, 45, 65),
  upr = c(1, 5, 15, 25, 35, 45, 65, 85),
  class = 1:8,
  wcount = c(408,
             250,
             137, 
             215,
             206,
             193,
             423,
             296),
  bcount = c(358,
             216,
             121,
             282,
             296,
             244,
             283,
             129))

population.deaths$class <- paste(population.deaths$lwr, 
                                              population.deaths$upr, sep = '-')

population.deaths$class <- factor(population.deaths$class,
                                  ordered = TRUE,
                                  levels = c("0-1", "1-5", "5-15", 
                                  "15-25", "25-35", "35-45", "45-65", "65-85"))

prison.deaths <- read_csv("data/prisoner-age-at-time-of-death-1905-1909.csv")

prison.deaths$class <- cut(prison.deaths$Age, 
                           breaks = c(0, population.deaths$upr),
                           ordered.result= T)


white <- ggplot(population.deaths) +
  geom_col(aes(x=class, y=wcount), width = .975) +
  scale_x_discrete(breaks = population.deaths$class) +
  scale_y_continuous(breaks = seq(0, 400, 25)) +
  labs(title = "Deaths by Age for the Florida General Population, 1905:\nWhites",
       x = "Age Group", y = NULL) +
  theme_bw()

black <- ggplot(population.deaths) +
  geom_col(aes(x=class, y=bcount), width = .975) +
  scale_x_discrete(breaks = population.deaths$class) +
  scale_y_continuous(breaks = seq(0, 400, 25)) +
  labs(title = "Deaths by Age for the Florida General Population, 1905:\nBlacks",
       x = "Age Group", y = NULL) +
  theme_bw()


prison <- ggplot(prison.deaths) +
  geom_histogram(aes(class), stat = "count", width = .975) +
  scale_x_discrete(breaks = prison.deaths$class,
                   labels = prison.deaths$class) +
  scale_x_discrete(labels = c("5-15", "15-25", "25-35", "35-45", "45-65")) +
  scale_y_continuous(breaks = seq(0, 400, 5)) +
  labs(title = "Deaths by Age in the Florida State Prison, 1905-1909",
       x = "Age Group", y = NULL) +
  theme_bw()

ages_fig <- gridExtra::arrangeGrob(white, black, prison, ncol = 3)

ggsave("figures/figure8-age-of-dead.png", ages_fig,
       width = 16, height = 6)  

# mortality by year ====

penal_data <- read_csv("data/prisoner_char.csv")

deaths <- ggplot(penal_data, aes(x = year, y = deaths)) +
  geom_col(aes(x = year, y = deaths), 
           fill = 'black', color = "gray25", size = .05) + 
  scale_x_continuous(breaks = seq(1870, 1930, 5), 
                     labels=seq(1870, 1930, 5),
                     name = NULL,
                     lim = c(1870, 1925)) +
  scale_y_continuous(breaks = seq(0, 80, 5),
                     name = NULL) +
  ggtitle("State Prison Deaths") +
  theme_bw() + 
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  theme(plot.title = element_text(size = 12),
        plot.caption = element_text(size =8,
                                    hjust=0.5))

# mortality rate: linear interpolation of population using zoo package
mortality_rate <- penal_data %>%
  mutate(Pop_avg = zoo::na.approx(object = penal_data$prison_population_mean,
                        x = 1869:1930)) %>%
  # filter(year < 1926) %>%
  mutate(mortality_rate = deaths / Pop_avg) %>%
  ggplot(aes(x = year, y = mortality_rate)) +
  geom_line(col = 'black', lwd = 1.15) +
  scale_x_continuous(breaks = seq(1870, 1925, 5), 
                     labels=seq(1870, 1925, 5),
                     name = NULL,
                     lim = c(1870, 1925)) +
  scale_y_continuous(breaks = seq(0, 1, .025),
                     labels = scales::percent,
                     name = NULL,
                     lim = c(0, .2)) +
  geom_point(col = 'black') +  
  ggtitle("State Prison Mortality rate") +
  theme_bw() +
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  theme(plot.title = element_text(size = 12),
        plot.caption = element_text(size =8,
                                    hjust=0.5))

# save figure ====

mortality_fig <- arrangeGrob(deaths, mortality_rate, ncol = 2)

ggsave("figures/figure2-annual-mortality.png", 
       mortality_fig,
       width = 11,
       height = 6, 
       dpi = 650)









