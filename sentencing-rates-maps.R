

# Calculate county sentencing rates using local empirical bayes estimates and 
# risk (race) adjusted chi square statistics to distinguish between counties with 
# higher or lower than expected sentencing rates.
# The expected rate is based on the statewide sentencing rate for white and black residents.

# This data set includes years corresponding to the availability of county sentencing data (starting 1905),
# until the end of convict leasing (1919). Counties that incorporated after the 1910 decennial census
# are excluded. The map uses the Florida county boundaries as of 1910.

# load packages ====
pkgs <- c("tidyverse", "classInt", "RColorBrewer", "ggmap", "GISTools", "maptools", "sf", 
          "tmap", "USAboundaries", "spdep", "sp", "raster", "scales", "pracma")
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

# calculate county sentencing rates and chi square statistics ====

sRates <- dcounty %>%
  mutate(name = toupper(county),
         population = population_black + population_white) %>%
  filter(#name %in% mapped$fl.data.name &
           year > 1904 & year < 1920) %>% 
  group_by(name) %>%
  dplyr::summarize(sents = sum(sentences, na.rm=T),
                   pop = sum(population, na.rm=T),
                   wpop = sum(population_white, na.rm = T),
                   bpop = sum(population_black, na.rm = T)) %>%
  ungroup() %>%
  # raw sentencing rates
  mutate(raw_Rate = sents / pop,
  # local empirical bayes estimates
         eb_Rate = EBlocal(ri = sents, ni = pop, nb = neighbs)$est) %>%
  # risk-adjusted expected number of sentences based on mean rates
  mutate(expected_sents = wpop*wrate + bpop*brate) %>%
  # Chi square statistics, carrying forward the local EB estimates
  mutate(stand_Rate = ((eb_Rate*pop) - expected_sents)/ sqrt(expected_sents),
         stand_Rate2 = ((eb_Rate*pop) - expected_sents)^2 / expected_sents,
         eb_shrinkage = eb_Rate - raw_Rate) %>%
  mutate(raw_Rate = 1000*raw_Rate,
         eb_Rate = 1000*eb_Rate,
         eb_shrinkage = 1000*eb_shrinkage)

# visualize results ====

  # View shrinkage of raw rates by local eb estimate
par(mfcol =  c(2,2), cex = .75)
plot(density(sRates$eb_Rate), lwd = 2, col = 'red', main = "County Sentencing Rates")
lines(density(sRates$raw_Rate), lwd = 2, col = 'blue')
legend(x=0, y = 2.5, legend = c("Raw rate", "EB rate"), fill = c("blue", "red"))
hist((sRates$eb_shrinkage), 50, main = 'difference')
plot(density(((sRates$eb_shrinkage))), main = 'difference')
boxplot(sRates$eb_shrinkage, main = "difference")

  # plot spatial distribution of rates and shrinkage
fl <- merge(fl, sRates)
pal <- brewer.pal(n = 6, name = "Purples")
brks <- classIntervals(fl$raw_Rate, style = 'jenks', n = 6)$brks
brks[7] <- brks[7]+.000001 # the top value is getting cut off...
spplot(fl, c('raw_Rate', 'eb_Rate'), col.regions = pal, at = brks)

pal <-  rev(brewer.pal(n = 10, name = "RdBu"))
spplot(fl, 'eb_shrinkage', col.regions = pal, at = seq(-.1,.08,.02))

  # the first is the conventional approach taken in epidemiology
  # the second is Dykes and Unwin's approach which preserves the direction of deviation from the expectation
  # See 'Maps of the census: a rough guide', http://www.agocg.ac.uk/reports/visual/casestud/dykes/dykes.pdf
par(mfcol =  c(1,2), cex = .5)
plot(density(sRates$stand_Rate2), col = 'blue', lwd = 2, main = 'conventional')
plot(density(sRates$stand_Rate), col = 'blue', lwd = 2, main = 'with sign')

# map sentencing rates ====

plantation_belt <- distinct(dcounty, county, .keep_all=T) %>% mutate(name = str_to_upper(county))
plantation_belt <- merge(fl, plantation_belt)
plantation_belt <- plantation_belt[which(plantation_belt$plantation_belt == 1), ]
plantation_belt <- unionSpatialPolygons(plantation_belt, plantation_belt@data$plantation_belt)


brks <- classIntervals(fl$eb_Rate, n = 6, style = 'jenks')$brks
pal <- brewer.pal(n = 6, name = "Reds")
ebrate_map <- tm_shape(fl) + 
  tm_fill("eb_Rate",
          title = "Mean Sentencing\nRates, 1905-1919",
          type = "fixed",
          legend.hist=T,
          palette = pal, 
          breaks = brks,
          frame = F
  ) +
  tm_borders(col="gray35") +
  tm_legend(legend.hist.bg.color = "azure", 
            legend.position = c("left", "bottom")) +
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
  tm_credits(bg.alpha = .15, 
             bg.color = "azure", 
    text = "*Plantation belt highlighted. Estimates obtained using local Empirical Bayes method.\nSource: Florida Dept. of Agriculture and author's calculations.")
ebrate_map
save_tmap(tm = ebrate_map, 
          filename = "figures/sentencing-rates-local-EB.png",
          units = "in", width = 6)

brks <- c(-10, -6, -2, 0, 2, 6, 10, 20, 34)
pal <- rev( brewer.pal(n = 7, name = "RdBu") )
standardized_map <- tm_shape(fl) + 
  tm_fill("stand_Rate",
          title = "Standardized\nsentencing rates\n(chi-square statistics),\n1905-1919",
          type = "fixed",
          legend.hist=T,
          palette = "-RdBu", 
          breaks = brks,
          frame = F
          ) +
  tm_borders(col="gray35") +
  tm_legend(legend.hist.bg.color = "azure", 
            title.size = 1.25,
            legend.position = c("left", "bottom")) +
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
  tm_credits(bg.alpha = .15, 
             bg.color = "azure", 
             size = 3,
             text = "*Plantation belt highlighted. Estimates account for racial composition and\nsize of the population. Source: Florida Dept. of Agriculture and author's calculations.")
standardized_map
save_tmap(tm = standardized_map, 
          filename = "figures/sentencing-rates-standardized.png",
          units = "in", width = 6)
