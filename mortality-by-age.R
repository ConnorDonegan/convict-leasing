


# Compare age distribution of deaths among prisoners (1905-1909) to 
  # that of whites and blacks in the Florida general population (1905) 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # The mortality rate among prisoners (2.2% after 1901) was consistently over three times greater than the mortality rate of the
  # Florida general population as meausred in 1905 (0.66%). 
  # The figures generated here illustrate why the disparity was far worse still.
  # median age of prisoners who died in custody was just 28.

# Sources: General population deaths: The Ninth Biennial Report of the Commissioner of Agriculture - 
  # State of Florida for the Period Beginning January 1, 1905, and Ending December 31, 1906.
  # The age of prisoners who died in custody was recorded in the Biennial Reports for the years
  # 1905 through 1909.

# gather data
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

# plots
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
