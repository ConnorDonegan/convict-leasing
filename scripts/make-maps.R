


# load packages ====

pkgs <- c("tidyverse", "classInt", "RColorBrewer", "GISTools", "maptools", "sf", 
          "ggmap", "tmap", "USAboundaries", "spdep", "sp", "raster", "scales")
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
  ) %>% as.tibble

# load and set projection for the historic map of Florida ====

fl <- us_counties(map_date="1910-01-01", 
                  resolution="high", 
                  states = "Florida") 
fl <- as(fl, "Spatial")
FLaea <- "+proj=aea +lat_1=24 +lat_2=31.5 +lat_0=24 +lon_0=-84 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs" 
fl <- spTransform(fl, CRS(FLaea))

# merge shape with data 

# fl <- merge(fl, dd, by = "name")
# plantation_belt <- fl[which(fl$Plantation_Belt == 1), ]
# plantation_belt <- unionSpatialPolygons(plantation_belt, plantation_belt@data$Plantation_Belt)

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
pal = "Greys"
neighbors.col = "gray80"
belt.col = "gray10"
leg.title.size = 1.2

# map the plantation belt and largest cities 

plantation_map <- tm_shape(fl) +
  tm_fill("pct_ag",
          title = "Percent of Land\nArea in Agriculture,\n1910",
          style = "cont", 
          legend.reverse = TRUE,
          palette = pal) +
  tm_borders(col="gray35") +
  tm_legend(
    legend.position = c("left", "bottom"),
    legend.text.size = .8,
    legend.title.size = leg.title.size) +
  tm_layout(bg.color = bg.color,
            frame.double.line = F,
            legend.format = list(digits = 0)) 

 # add neighboring states
plantation_map <- plantation_map +  
  tm_shape(bg) +
  tm_fill(col = neighbors.col) +
  tm_borders(col="gray35") 
  # tm_shape(plantation_belt) +
  # tm_borders(col = belt.col, lty = 2,
  #            lwd = 2.5) +
  
 # add the cities
plantation_map <- plantation_map +
  tm_shape(cities) + 
  tm_dots(legend.show = F, 
          shape = 21,
          alpha = .75,
          size = 0.35,
          col = "black") +
  tm_text("city", 
          size = .8, 
          xmod = -.5,
          ymod = .6,
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
          ymod = -.25,
          bg.color = "snow2",
          bg.alpha = .25,
          alpha = .9) 

 # add a compass
plantation_map <- plantation_map +
  tm_compass(position = c("center", "bottom"),
            size = 1,
            fontsize = .75) 

 # map SIRs
sir_map <- tm_shape(fl) +
  tm_fill("SIR",
          title = "Standardized State\nPrison Sentencing\nRatios, 1905-1919",
          style = "cont",
          palette = pal,
          legend.reverse = TRUE,
          frame = F,
          labels = c("0.5", "1.0", "1.5", "2.0", "2.5", "3.0")
  ) +
  tm_borders(col="gray35") +
  tm_legend(legend.hist.bg.color = bg.color, 
            legend.position = c("left", "bottom"),
            legend.text.size = .8,
            legend.title.size = 1.2) +
  tm_layout(bg.color = bg.color,
            legend.hist.height=.25,
            frame.double.line = F,
            legend.format = list(digits = 1)) 

 # add neighboring states
sir_map <- sir_map +
  tm_shape(bg) +
  tm_fill(col = neighbors.col) +
  tm_borders(col="gray35") +
  # tm_shape(plantation_belt) +
  # tm_borders(col = "gray20",
  #            lwd = 2.5) +

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
          xmod = -.5,
          ymod = .6,
          alpha = .9) +
  tm_shape(capitol) + 
  tm_dots(legend.show = F, 
          # xmod = -.25,
          shape = 23,
          alpha = 1,
          size = 0.35,
          col = "black"
  ) +
  tm_text("city", 
          size = .8, 
          ymod = -.25,
          bg.alpha = .25,
          alpha = .9) 

 # add a compass
sir_map <- sir_map +
  tm_compass(position = c("center", "bottom"),
             size = 1,
             fontsize = .75) 


current.mode <- tmap_mode("plot")
tmap_arrange(w1, w2, w3, w4)
tmap_mode(current.mode)