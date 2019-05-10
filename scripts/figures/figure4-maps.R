

# load packages ====

pkgs <- c("tidyverse", "classInt", "RColorBrewer", "GISTools", "maptools", "sf", "magick",
          "ggmap", "tmap", "USAboundaries", "sp", "raster", "scales")
lapply(pkgs, require, character.only=TRUE); rm(pkgs)

# load simple features file with local EB and Moran's I estimates

fl <- read_rds("data/constructed/florida-1910-sp.rds")

# plantation belt boundaries ==== 

plantation_belt <- fl[which(fl$plantation_belt == 1), ]
plantation_belt <- unionSpatialPolygons(plantation_belt, plantation_belt@data$plantation_belt)

# neighboring states for map background ====

FLaea <- "+proj=aea +lat_1=24 +lat_2=31.5 +lat_0=24 +lon_0=-84 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs" 
bg <- us_counties(map_date="1910-01-01", resolution="low", 
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
# write_csv(xy, "data/urban-lat-lons.csv")
xy <- read_csv("data/urban-lat-lons.csv")
cities <- SpatialPointsDataFrame(xy, data = cities[ ,-4])
rm(xy)

capitol <- data.frame(
  city = "Tallahassee", state = "Florida", country = "U.S.A."
) %>% mutate(name = paste(city, state, country, sep = ", "))
# xy <- geocode(capitol$name, output = c("latlon"), source = "google")
# write_csv(xy, "data/tallahassee-lat-lon.csv")
xy <- read_csv("data/tallahassee-lat-lon.csv")
capitol <- SpatialPointsDataFrame(xy, data = capitol[ , -4])

# set color (gray scale) parameters ==== 

bg.color = "gray95"
pal = "Greys"
neighbors.col = "gray90"
belt.col = "gray10"
leg.title.size = 1.65
leg.text.size = 1
cities_xmod = c(0, 0, .5, -.1, -.25)
cities_ymod = .5
border.col = "gray30"

# map the plantation belt and largest cities 
brks <- classIntervals(fl@data$pct_ag, n = 5, style = "jenks")$brks

plantation_map <- tm_shape(fl) +
  tm_fill("pct_ag_1910",
          title = "Percent of Land\nArea in Agriculture,\n1910",
          style =  "cont",
          # breaks = brks,
          legend.reverse = TRUE,
          palette = pal) +
  tm_borders(col=border.col) +
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

# add major cities
plantation_map <- plantation_map +
  tm_shape(cities) + 
  tm_dots(legend.show = F, 
          shape = 21,
          alpha = .85,
          size = 0.3,
          col = "black") +
  tm_text("city", 
          size = .8, 
          xmod = cities_xmod,
          ymod = cities_ymod,
          alpha = .9) 

# add Tallahassee
plantation_map <- plantation_map +
  tm_shape(capitol) + 
  tm_dots(legend.show = F, 
          shape = 23,
          alpha = 1,
          size = 0.35,
          col = "gray"
  ) +
  tm_text("city", 
          size = .7, 
          ymod = cities_ymod,
          col = "gray70",
          # bg.color = "snow2",
          # bg.alpha = .25,
          alpha = .9) 

# add a compass
plantation_map <- plantation_map +
  tm_compass(position = c("center", "bottom"),
             size = 1,
             fontsize = .75) 

tmap_save(tm = plantation_map,
          filename = "figures/plantation-map.png",
          units = "in", width = 6.75)

# map SIRs
# brks <- c(0, .5, 1, 1.5,  2, 2.5, 3)
ssr_map <- tm_shape(fl) +
  tm_fill("LEB",
          title = "Standardized State\nPrison Sentencing\nRatios, 1905-1910",
          style = "cont",
          # n = 6,
          # breaks = brks,
          palette = pal,
          legend.reverse = TRUE,
          frame = F#,
          # labels = c("0.5", "1.0", "1.5", "2.0", "2.5", "3.0")
  ) +
  tm_borders(col="gray35") +
  tm_legend(legend.hist.bg.color = bg.color, 
            legend.position = c("left", "bottom"),
            legend.text.size = leg.text.size,
            legend.title.size = leg.title.size
            ) +
  tm_layout(bg.color = bg.color,
            legend.hist.height=.25,
            frame.double.line = F,
            legend.format = list(digits = 1)) 

# add neighboring states and plantation belt
ssr_map <- ssr_map +
  tm_shape(bg) +
  tm_fill(col = neighbors.col) +
  tm_borders(col="gray35") +
  tm_shape(plantation_belt) +
  tm_borders(col = belt.col,
             lwd = 2) 

# add major cities
ssr_map <- ssr_map +
  tm_shape(cities) + 
  tm_dots(legend.show = F, 
          shape = 21,
          alpha = .85,
          size = 0.3,
          col = "black") +
  tm_text("city", 
          size = .8, 
          xmod = cities_xmod,
          ymod = cities_ymod,
          alpha = .9) 

# add Tallahassee
ssr_map <- ssr_map +
  tm_shape(capitol) + 
  tm_dots(legend.show = F, 
          shape = 23,
          alpha = 1,
          size = 0.35,
          col = "gray"
  ) +
  tm_text("city", 
          size = .7, 
          ymod = cities_ymod,
          col = "black",
          # bg.color = "snow2",
          # bg.alpha = .25,
          alpha = .9) 

# add a compass
ssr_map <- ssr_map +
  tm_compass(position = c("center", "bottom"),
             size = 1,
             fontsize = .75) 
# save
tmap_save(tm = ssr_map,
          filename = "figures/SSR-map.png",
          units = "in", width = 6.75)


# combine images
m1 <- image_read("figures/plantation-map.png")
m2 <- image_read("figures/SSR-map.png")

m <- c(m1, m2)
m <- image_append(m)

image_write(m, 
            path = "figures/figure4-plantations-sentences.png", 
            format = "png")
# 
# file.remove(c("figures/plantation-map.png",
#               "figures/SSR-map.png",
#               "figures/cluster-map.png"))

