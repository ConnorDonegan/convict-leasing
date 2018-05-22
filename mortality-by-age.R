

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

# mortality by age ====

population.deaths <- data.frame(
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

prison.deaths <- read.csv("data/prisoner-age-at-time-of-death-1905-1909.csv")
prison.deaths$class <- cut(prison.deaths$Age, 
                           breaks = c(0, population.deaths$upr),
                           ordered.result= T)
prisoners <- aggregate(x=prison.deaths$Age, by = list(prison.deaths$class), FUN = length)

png('figures/prison-mortality-by-age.png', width = 600, height=480,units='px')
classes <- paste(population.deaths$lwr, population.deaths$upr, sep = '-')
barplot(prisoners$x, main = 'State Prison Deaths by Age, 1905-1909',
        space = .05,
        names.arg = classes[3:7])
dev.off()
png('figures/florida-population-mortality-by-race-age.png', width=999, height=480,units='px')
par(mfrow=c(1,2))
barplot(population.deaths$bcount,     
        space = .05,
        main = "Deaths by Age for the Florida General Population:\nBlacks, 1905",
        names.arg = classes)
barplot(population.deaths$wcount, 
        space = .05,
        main = "Deaths by Age for the Florida General Population:\nWhites, 1905",
        names.arg = classes)
dev.off()

# mortality by year ====


penal_data <- read_csv("data/prisoner_char.csv")

deaths <- ggplot(penal_stats, aes(x = year, y = deaths)) +
  geom_col(aes(x = year, y = deaths), 
           fill = 'black', color = "gray25", size = .05) + 
  scale_x_continuous(breaks = seq(1870, 1930, 10), 
                     labels=seq(1870, 1930, 10),
                     name = NULL,
                     lim = c(1870, 1925)) +
  scale_y_continuous(breaks = seq(0, 80, 5),
                     name = NULL) +
  ggtitle("Deaths") +
  theme_bw() + 
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  theme(plot.title = element_text(size = 11.5),
        plot.caption = element_text(size =8,
                                    hjust=0.5))

# mortality rate: linear interpolation of population using zoo package
mortality_rate <- penal_data %>%
  mutate(Pop_avg = zoo::na.approx(object = penal_data$prison_population_mean,
                        x = 1869:1930)) %>%
  mutate(mortality_rate = deaths / Pop_avg) %>%
  ggplot(aes(x = year, y = mortality_rate)) +
  geom_line(col = 'black', lwd = 1.15) +
  scale_x_continuous(breaks = seq(1870, 1930, 10), 
                     labels=seq(1870, 1930, 10),
                     name = NULL) +
  scale_y_continuous(breaks = seq(0, 1, .025),
                     labels = scales::percent,
                     name = NULL,
                     lim = c(0, .2)) +
  geom_point(col = 'black') +  
  ggtitle("Mortality rate") +
  theme_bw() +
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) +
  theme(plot.title = element_text(size = 11.5),
        plot.caption = element_text(size =8,
                                    hjust=0.5))

# save figure ====

g <- arrangeGrob(deaths, mortality_rate, ncol = 2)
ggsave("figures/prison-mortality-rates.png", g,
       width = 11, height = 6)
