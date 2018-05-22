

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Calculate county sentencing rates using local empirical bayes estimates and standardized sentencing
# rates to distinguish between counties with  higher or lower than expected rates.
# The expected rate is based on the statewide sentencing rate for white and black residents respectively.

# This data set includes years corresponding to the availability of county sentencing data (starting 1905),
# until the end of convict leasing (1919). Counties that incorporated after the 1910 decennial census
# are excluded. The map uses the historical Florida county boundaries of 1910.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# load packages ====
pkgs <- c("tidyverse", "classInt", "RColorBrewer", "ggmap", "GISTools", "maptools", "sf", 
          "ggmap", "tmap", "USAboundaries", "spdep", "sp", "raster", "scales", "pracma")
lapply(pkgs, require, character.only=TRUE); rm(pkgs)

# load data and calculate mean (state wide) sentencing rates by race during the convict leasing program ====

dcounty <- read_csv("data/sentencing.csv")

bpop <- sum(dcounty$population_black, na.rm=T)
wpop <- sum(dcounty$population_white, na.rm=T)

dstate <- read_csv("data/prisoner_char.csv")
dstate <- dstate %>%
  dplyr::select(year, black_male, white_male, black_female, white_female) %>%
  transmute(Year = year,
            black = black_male + black_female,
            white = white_male + white_female) %>%
  filter(Year > 1904 & Year < 1920)

bsents <- sum(dstate$black, na.rm=T)
wsents <- sum(dstate$white, na.rm=T)

brate <- (bsents / bpop)
wrate <- (wsents / wpop)

# load and set projection for the historic map of Florida ====

fl <- us_counties(map_date="1910-01-01", 
                  resolution="high", 
                  states = "Florida") 
fl <- as(fl, "Spatial")
FLaea <- "+proj=aea +lat_1=24 +lat_2=31.5 +lat_0=24 +lon_0=-84 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs" 
fl <- spTransform(fl, CRS(FLaea))

  # neighbors list for local empirical bayes estimates
neighbs <- poly2nb(fl)

  # neighboring states for map background
bg <- us_counties(map_date="1920-01-01", resolution="high", 
                  states=c("Alabama", "Georgia", "Louisiana"))
bg <- as(bg, "Spatial")
bg <- unionSpatialPolygons(bg, bg$state_terr)
bg <- spTransform(bg, CRS(FLaea))

# calculate county sentencing rates and SIR ====

sRates <- dcounty %>%
  mutate(name = toupper(county),
         population = population_black + population_white) %>%
  filter(year > 1904 & year < 1920) %>% 
  group_by(name) %>%
  dplyr::summarize(sents = sum(sentences, na.rm=T),
                   sents_lwr = qpois(.05, sents),
                   sents_upr = qpois(.95, sents),
                   pop = sum(population, na.rm=T),
                   wpop = sum(population_white, na.rm = T),
                   bpop = sum(population_black, na.rm = T),
                   pct_agricultural = unique(pct_agricultural_1910),
                   plantation_belt = unique(plantation_belt)) %>%
  ungroup() %>%
  mutate(
    # risk-adjusted expected number of sentences based on mean rates
    expected_sents = wpop*wrate + bpop*brate,
    # raw sentencing rates
    raw_Rate = sents / pop,
    # local empirical bayes estimates
    eb_Rate = EBlocal(ri = sents, ni = pop, nb = neighbs)$est,
    # SIR
    SIR = sents / expected_sents,
    SIR_lwr = sents_lwr / expected_sents, 
    SIR_upr = sents_upr / expected_sents) %>%
  mutate(raw_Rate = 1000 * raw_Rate,
         eb_Rate = 1000 * eb_Rate)

# View shrinkage of raw rates by local eb estimate ====

plot(density(sRates$eb_Rate), lwd = 2, col = 'red', main = "County Sentencing Rates")
lines(density(sRates$raw_Rate), lwd = 2, col = 'blue')
legend(x=0, y = 2.5, legend = c("Raw rate", "EB rate"), fill = c("blue", "red"))

# plot standardized incidence rates ====

sRates <-  arrange(sRates, SIR)
sRates$name <- factor(sRates$name, ordered=T, levels = sRates$name)
SIR_plot <- ggplot(sRates) +
  geom_point(aes(x=name, y = SIR)) +
  geom_errorbar(aes(x=name, ymin = SIR_lwr, ymax = SIR_upr),
                width = 0,
                size = .25) +
  geom_hline(yintercept = 1) +
  coord_flip() +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(breaks = seq(0, 4, .5),name = NULL) +
  labs(title = "Standardized sentencing rates, 1905-1919",
       subtitle = "with 95% confidence intervals") +
  theme_bw() +
  theme(plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 9))

ggsave("figures/sentencing-rates-standardized-plot.png", 
       SIR_plot,
       width = 6, 
       height = 8,
       units = "in")

# get lat-lon for Florida's largest cities plus Tallahassee ====

cities <- data.frame(
  city = c("Key West", "Pensacola", "Jacksonville", "Tampa", "Miami"),
  state = rep("Florida", 5),
  country = rep("U.S.A.", 5)) %>% 
  mutate(name = paste(city, state, country, sep = ", "))
# xy <- geocode(cities$name, output = c("latlon"), source = "google")
# write_rds(xy, "data/urban-lat-lons.rds")
xy <- read_rds("data/urban-lat-lons.rds")
cities <- SpatialPointsDataFrame(xy, data = cities[ ,-4])

capitol <- data.frame(
  city = "Tallahassee", state = "Florida", country = "U.S.A."
) %>% mutate(name = paste(city, state, country, sep = ", "))
# xy <- geocode(capitol$name, output = c("latlon"), source = "google")
# write_rds(xy, "data/tallahassee-lat-lon.rds")
xy <- readRDS("data/tallahassee-lat-lon.rds")
capitol <- SpatialPointsDataFrame(xy, data = capitol[ , -4])

# merge data with shape and create plantation belt shapefile ====

fl <- merge(fl, sRates, by = "name")
plantation_belt <- fl[which(fl$plantation_belt == 1), ]
plantation_belt <- unionSpatialPolygons(plantation_belt, plantation_belt@data$plantation_belt)

# map the plantation belt and largest cities ====

# breaksAg = c(0, 15, 30, 45, 60)
palAg <- c('#ffffcc','#c2e699','#78c679','#238443')
plantation_map <- tm_shape(fl) +
  tm_fill("pct_agricultural",
          title = "Percent of Land\nArea in Agriculture,\n1910",
          style = "cont", 
          palette = palAg) +
  tm_borders(col="gray35") +
  tm_legend(legend.hist.bg.color = "azure", 
            legend.position = c("left", "bottom"),
            legend.text.size = .8,
            legend.title.size = 1.05) +
  tm_layout(bg.color = "lightblue",
            frame.double.line = F,
            legend.format = list(digits = 0)) +
  tm_shape(bg) +
  tm_fill(col = "azure") +
  tm_borders(col="gray35") +
  tm_shape(plantation_belt) +
  tm_borders(col = "gray20",
             lwd = 2.5) +
  tm_compass(position = c("center", "bottom"),
             size = 1,
             fontsize = .75) +
  tm_shape(cities) + 
  tm_dots(legend.show = F, 
          xmod = .3,
          shape = 21,
          alpha = .75,
          size = 0.35,
          col = "black") +
  tm_text("city", size = .8, 
          auto.placement = T, 
          alpha = .9) +
  tm_shape(capitol) + 
  tm_dots(legend.show = F, 
          xmod = -.25,
          shape = 18,
          alpha = .75,
          size = 0.35,
          col = "black") +
  tm_text("city", size = .8, 
          auto.placement = T, 
          alpha = .9) 

save_tmap(tm = plantation_map, 
          filename = "figures/plantation-belt-map.png",
          units = "in", width = 6)

# map sentencing rates ====

# map local EB rates
 # brks <- classIntervals(fl$eb_Rate, n = 6, style = 'jenks')$brks
  # pal <- brewer.pal(n = 6, name = "Reds")
ebrate_map <- tm_shape(fl) + 
  tm_fill("eb_Rate",
          title = "Mean State Prison\nSentencing Rates\nper 1,000 Residents,\n1905-1919",
          style = "cont",
          palette = "Reds",
          frame = F
  ) +
  tm_borders(col="gray35") +
  tm_legend(legend.hist.bg.color = "azure", 
            legend.position = c("left", "bottom"),
            legend.text.size = .8,
            legend.title.size = 1.05) +
  tm_layout(bg.color = "lightblue",
            legend.hist.height=.25,
            frame.double.line = F,
            legend.format = list(digits = 1)) +
  tm_shape(bg) +
  tm_fill(col = "azure") +
  tm_borders(col="gray35") +
  tm_shape(plantation_belt) +
  tm_borders(col = "gray20",
             lwd = 2.5) +
  tm_compass(position = c("center", "bottom"),
             size = 1,
             fontsize = .75)

save_tmap(tm = ebrate_map, 
          filename = "figures/sentencing-rates-local-EB-map.png",
          units = "in", width = 6)

# map standardized rates
fl$SIR_zero_cntr <- fl$SIR - 1
standardized_map <- tm_shape(fl) + 
  tm_fill("SIR_zero_cntr",
          title = "Standardized State\nPrison Sentencing\nRates, 1905-1919",
          style = "cont",
          palette = "-RdBu",
          frame = F,
          labels = c("0.5", "1.0", "1.5", "2.0", "2.5", "3.0")
          ) +
  tm_borders(col="gray35") +
  tm_legend(legend.hist.bg.color = "azure", 
            legend.position = c("left", "bottom"),
            legend.text.size = .8,
            legend.title.size = 1.05) +
  tm_layout(bg.color = "lightblue",
            legend.hist.height=.25,
            frame.double.line = F,
            legend.format = list(digits = 1)) +
  tm_shape(bg) +
  tm_fill(col = "azure") +
  tm_borders(col="gray35") +
tm_shape(plantation_belt) +
  tm_borders(col = "gray20",
             lwd = 2.5) +
  tm_compass(position = c("center", "bottom"),
             size = 1,
             fontsize = .75)

save_tmap(tm = standardized_map, 
          filename = "figures/sentencing-rates-standardized-map.png",
          units = "in", width = 6)
