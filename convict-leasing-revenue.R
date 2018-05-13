

# Revenue derived from the FL state prison system, 1873-1914.
# Data gathered from biennial reports of the Florida commissioner of agriculture.
# Most years are reported in 13th Biennial Report for years 1913-1914 and 
# the 16 Biennial report with data for years 1917-1920. Also see the annual
# reports of the Florida Adjutent General for missing values for 1886-1890.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


library(tidyverse)

revenue <- read_csv("data/convict-leasing-revenue.csv")   

# this is just to draw a continuous line over missing values.
revenue$rev_line <- zoo::na.approx(revenue$revenue, x = revenue$year)

ggplot(revenue, aes(x=year, y=revenue)) + 
  geom_point(aes(year, revenue), 
             size = 1.75, col = 'black') +
  geom_line(aes(year, rev_line),
             size =1, color = "black") + 
  scale_x_continuous(name=NULL, 
                     breaks = seq(1875, 1920, 5)) +
  scale_y_continuous(name=NULL, 
                     breaks=seq(0, 350000, 50000),
                     labels = scales::dollar) +
  ggtitle("State convict leasing revenue, 1877-1919") +
  theme_bw() + 
  theme(plot.title = element_text(size=11.5),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y = element_line(size = .5)) +
  geom_hline(yintercept = 0) 

ggsave("figures/convict-leasing-revenue.png", 
       width = 7.5, height = 6) 
