# load packages ====

pkgs <- c("tidyverse", "classInt", "RColorBrewer", "GISTools", "maptools", "sf", "magick",
          # "ggmap", 
          "tmap", "USAboundaries", "USAboundariesData", "spdep", "sp", "raster", "scales")
lapply(pkgs, require, character.only=TRUE); rm(pkgs)

# load data ===== 

load("data/table-of-model-results.Rdata")
dcounty <- read_csv("data/sentencing.csv")

# combine data ====

dd <- dtable %>%
  rename(name = County) %>%
  left_join(
    dcounty %>%
      transmute(name = str_to_upper(county), 
                pct_ag = pct_agricultural_1910) %>%
      distinct
  ) %>% 
  as_tibble

# load and set projection for the historic map of Florida ====

fl <- us_counties(map_date="1910-01-01", 
                  resolution="high", 
                  states = "Florida") 
fl <- as(fl, "Spatial")
FLaea <- "+proj=aea +lat_1=24 +lat_2=31.5 +lat_0=24 +lon_0=-84 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs" 
fl <- spTransform(fl, CRS(FLaea))

# merge shape with data ====

fl <- merge(fl, dd, by = "name")

# plantation belt boundaries ==== 

plantation_belt <- fl[which(fl$Plantation_Belt == 1), ]
plantation_belt <- unionSpatialPolygons(plantation_belt, plantation_belt@data$Plantation_Belt)

# neighboring states for map background ====

bg <- us_counties(map_date="1910-01-01", resolution="high", 
                  states=c("Alabama", "Georgia", "Mississippi", "Louisiana"))
bg <- as(bg, "Spatial")
bg <- unionSpatialPolygons(bg, bg$state_terr)
bg <- spTransform(bg, CRS(FLaea))

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

# set color (gray scale) parameters ==== 

bg.color = "gray95"
#pal = "Greys"
neighbors.col = "gray90"
belt.col = "gray10"
leg.title.size = 2
leg.text.size = 1
cities_xmod = c(0, 0, .5, -.1, -.25)
cities_ymod = .5

# save to Latex figures folder for the preprint #
dir <- "~/repo/convict-leasing/preprint/figures"

# map the plantation belt and largest cities 
# brks <- classIntervals(fl@data$pct_ag, n = 5, style = "jenks")$brks

plantation_map <- tm_shape(fl) +
  tm_fill("pct_ag",
          title = "Percent of Land\nArea in Agriculture,\n1910",
          style =  "cont",
          # breaks = brks,
          legend.reverse = TRUE,
          palette = "BuGn") +
  tm_borders(col="gray35") +
  tm_legend(
    legend.position = c("left", "bottom"),
    legend.text.size = leg.text.size,
    legend.title.size = leg.title.size) +
  tm_layout(bg.color = bg.color,
            frame.double.line = F,
            legend.format = list(digits = 0)) 

 # add neighboring states
plantation_map <- plantation_map +  
  tm_shape(bg) +
  tm_fill(col = neighbors.col) +
  tm_borders(col="gray35") +
  tm_shape(plantation_belt) +
  tm_borders(col = belt.col, lty = 1,
             lwd = 2)
  
 # add the cities
plantation_map <- plantation_map +
  tm_shape(cities) + 
  tm_dots(legend.show = F, 
          shape = 21,
          alpha = .75,
          size = 0.3,
          col = "black") +
  tm_text("city", 
          size = .8, 
          # xmod = -.5,
          xmod = cities_xmod,
          ymod = cities_ymod,
          alpha = .9) +
  tm_shape(capitol) + 
  tm_dots(legend.show = F, 
          shape = 23,
          alpha = 1,
          size = 0.35,
          col = "gray",
  ) +
  tm_text("city", 
          size = .8, 
          ymod = cities_ymod,
          bg.color = "snow2",
          bg.alpha = .25,
          alpha = .9) 

 # add a compass
plantation_map <- plantation_map +
  tm_compass(position = c("center", "bottom"),
            size = 1,
            fontsize = .75) 

tmap_save(tm = plantation_map,
          filename = file.path(dir, "plantation-map.png"),
          # filename = "figures/plantation-map.png",
          units = "in", width = 6.75)

 # map SIRs
sir_map <- tm_shape(fl) +
  tm_fill("Model_SIR",
          title = "Standardized State\nPrison Sentencing\nRatios, 1905-1910",
          style = "cont",
          midpoint = 1,
          palette = "seq",
          legend.reverse = TRUE,
          frame = F
  ) +
  tm_borders(col="gray35") +
  tm_legend(legend.hist.bg.color = bg.color, 
            legend.position = c("left", "bottom"),
            legend.text.size = leg.text.size,
            legend.title.size = leg.title.size) +
  tm_layout(bg.color = bg.color,
            legend.hist.height=.25,
            frame.double.line = F,
            legend.format = list(digits = 1)) +
  tm_layout(aes.palette = list(seq = "-RdBu")) # RdGy PuOr

 # add neighboring states
sir_map <- sir_map +
  tm_shape(bg) +
  tm_fill(col = neighbors.col) +
  tm_borders(col="gray35") +
  tm_shape(plantation_belt) +
  tm_borders(col = belt.col,
             lwd = 2)

 # add cities
sir_map <- sir_map + 
  tm_shape(cities) + 
  tm_dots(legend.show = F, 
          shape = 21,
          alpha = .75,
          size = 0.35,
          col = "black") +
  tm_text("city", 
          size = .8, 
          xmod = cities_xmod,
          ymod = .6,
          alpha = .9) +
  tm_shape(capitol) + 
  tm_dots(legend.show = F, 
          shape = 23,
          alpha = 1,
          size = 0.35,
          col = "black"
  ) +
  tm_text("city", 
          size = .8, 
          ymod = cities_ymod,
          bg.alpha = .35,
          alpha = .9) 

 # add a compass
sir_map <- sir_map +
  tm_compass(position = c("center", "bottom"),
             size = 1,
             fontsize = .75) 

tmap_save(tm = sir_map,
          # filename = "figures/sir-map.png",
          filename = file.path(dir, "sir-map.png"),
          units = "in", width = 6.75)

# Figure 4: combine sentencing maps with magick ====

m1 <- image_read("figures/plantation-map.png")
m2 <- image_read("figures/sir-map.png")

m <- c(m1, m2)
m <- image_append(m)

image_write(m, 
            path = "figures/figure4-plantations-sentences.png", 
            format = "png")

# file.remove(c("figures/plantation-map.png",
#               "figures/sir-map.png"))


