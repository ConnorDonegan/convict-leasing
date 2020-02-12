

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Compare age distribution of deaths among prisoners (1905-1909) to 
# that of whites and blacks in the Florida general population (1905) 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# The age of prisoners who died in custody was recorded in the Biennial Reports for the years
# 1905 through 1909.

# Cf. General population deaths: The Ninth Biennial Report of the Commissioner of Agriculture - 
# State of Florida for the Period Beginning January 1, 1905, and Ending December 31, 1906.

library(tidyverse)
source("scripts/figures/custom-plot-theme.R")

 # mortality by age (only available from 1905-1909)

prison.deaths <- read_csv("data/prisoner-age-at-time-of-death-1905-1909.csv")

age_plot <- prison.deaths %>%
  ggplot(aes(Age)) +
  geom_histogram(binwidth=2,
                 # stat = "count",
                 fill = "gray20",
                 col = "gray40") +
  scale_x_continuous(breaks = seq(10, 70, by = 5)) +
  scale_y_continuous(breaks = seq(0, 25, 2)) +
  labs(#title = "State prison deaths by age, 1905-1909",
       x = "Age", y = NULL) +
  theme_cust +
  theme(panel.grid.minor.y = element_blank())

dir <- "~/repo/convict-leasing/preprint/figures"
ggsave(#"figures/figure5-age-of-dead.png", 
       file.path(dir, "figure5-age-of-dead.png"),
       age_plot,
       width = 5.5, height = 4,
       dpi = 650)  

 # annual death count
penal_data <- read_csv("data/prisoner_char.csv")

deaths <- penal_data %>%
  dplyr::select(year, deaths) %>%
  dplyr::filter(year < 1926) %>%
  na.omit %>%
  ggplot(aes(x = year, y = deaths)) +
  geom_col(aes(x = year, y = deaths), 
           fill = 'black', color = "gray25", size = .05) + 
  scale_x_continuous(breaks = seq(1860, 1930, 5), 
                     labels=seq(1860, 1930, 5),
                     name = NULL
                    # lim = c(1860, 1925)
                     ) +
  scale_y_continuous(breaks = seq(0, 80, 5),
                     name = NULL) +
  # ggtitle("State prison deaths") 
  theme_cust +
  theme(axis.text = element_text(size = 20))

 # mortality rate
mortality_rate <- penal_data %>%
  mutate(Pop_avg = zoo::na.approx(object = penal_data$prison_population_mean,
                        x = 1869:1930)) %>%
  dplyr::filter(year < 1926) %>%
  mutate(mortality_rate = deaths / Pop_avg) %>%
  dplyr::select(year, mortality_rate) %>%
  na.omit %>%
  ggplot(aes(x = year, y = mortality_rate)) +
  geom_line(col = 'black', lwd = 1) +
  scale_x_continuous(breaks = seq(1860, 1930, 5), 
                     labels=seq(1860, 1930, 5),
                     name = NULL
                   #  lim = c(1870, 1925)
                   ) +
  scale_y_continuous(breaks = seq(0, 1, .025),
                     labels = scales::percent,
                     name = NULL,
                     lim = c(0, .2)) +
  geom_point(col = 'black') +  
  # ggtitle("State prison mortality rate") +
  theme_cust +
  theme(axis.text = element_text(size = 20))

# save figure ====
## saving directly to the Latex figure folder for the preprint
dir = "~/repo/convict-leasing/preprint/figures"
ggsave( #"figures/figure2a-deaths.png", 
      file.path(dir, "figure2a-deaths.png"),
       deaths,
       width = 10,
       height = 7,
       dpi = 650)

ggsave(#"figures/figure2b-mortality-rate.png", 
       file.path(dir, "figure2b-mortality-rate.png"),
       mortality_rate,
       width = 10,
       height = 7,
       dpi = 650)
#
# mortality_fig <- gridExtra::arrangeGrob(deaths, mortality_rate, ncol = 2, padding = unit(1, "line"))
# 
# ggsave("figures/figure2-annual-mortality.png", 
#        mortality_fig,
#        width = 11,
#        height = 5.66, 
#        dpi = 650)


