

pkgs <- c("tidyverse",  "tmap", "GISTools", "USAboundaries")
lapply(pkgs, require, character.only=TRUE); rm(pkgs)

fl <- read_rds("data/constructed/florida-1910-sp.rds")

plantation_belt <- fl[which(fl$plantation_belt == 1), ]
plantation_belt <- unionSpatialPolygons(plantation_belt, plantation_belt@data$plantation_belt)

FLaea <- "+proj=aea +lat_1=24 +lat_2=31.5 +lat_0=24 +lon_0=-84 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs" 
bg <- us_counties(map_date="1910-01-01", resolution="low", 
                  states=c("Alabama", "Georgia", "Mississippi", "Louisiana"))
bg <- as(bg, "Spatial")
bg <- unionSpatialPolygons(bg, bg$state_terr)
bg <- spTransform(bg, CRS(FLaea))

bg.color = "gray95"
pal = "Greys"
neighbors.col = "gray90"
belt.col = "gray10"
leg.title.size = 1.65
leg.text.size = 1
border.col = "gray30"

# shrinkage plot

dplyr::select(fl@data, SSR_raw, LEB, plantation_belt) %>%
  ggplot(aes(SSR_raw, LEB, col = factor(plantation_belt))) +
  geom_point(size = 2.75) +
  geom_abline() +
  scale_x_continuous(breaks = seq(0, 4.15, by = .5),
                     lim = c(0, 4.15)) +
  scale_y_continuous(breaks = seq(0, 4, by = .5),
                     lim = c(0, 4)) +
  scale_color_manual(breaks = c(0, 1), 
                     labels = c("No", "Yes"),
                     values = c("gray15", "blue"),
                     name = "Plantation Belt") +
  theme_gray() +
  theme(legend.position = "bottom") +
  labs(x = "Raw", y = "Local EB",
       title = "Local EB-imposed shrinkage on SSR estimates")

ggsave("supplementary-SSR-shrinkage.png", path = "figures")

# cluster map

fl@data$cluster <- ifelse(fl$LEB < 1, "Low", "High")
fl@data$cluster <- ifelse(fl$P.i < .05, fl$cluster, NA)

low_risk <- fl[which(!is.na(fl$cluster) & fl$cluster == "Low"),]
high_risk <- fl[which(!is.na(fl$cluster) & fl$cluster == "High-High"),]

cluster_map <- tm_shape(fl) +
  tm_fill("cluster",
          title = "Local Moran's I\nClusters of High\nStandardized Sentencing\nRatios (p-value < .05)",
          showNA = FALSE,
          breaks = c("High-High", "Low"),
          labels = c("Cluster centroid", "nada"),
          colorNA = "white",
          palette = c("red", "blue")) +
  tm_borders(col = border.col) +
  tm_legend(
    legend.position = c("left", "bottom"),
    legend.text.size = 1, 
    legend.title.size = 1.5) +  
  tm_shape(bg) +
  tm_fill(col = neighbors.col) +
  tm_borders(col="gray35") +
  tm_shape(plantation_belt) +
  tm_borders(col = belt.col, lwd = 2) +
  tm_compass(position = c("center", "bottom"),
             size = 1,
             fontsize = .75) +
  tm_layout(bg.color = bg.color,
            frame.double.line = F,
            legend.format = list(digits = 0)) 

tmap_save(tm = cluster_map,
          filename = "figures/supplementary-SSR-clusters.png",
          units = "in", width = 6.75)

# APLEs

aple_map <- tm_shape(fl) +
  tm_fill("aple",
          title = "Local Approximate Profile\nLikelihood Estimate (APLE)\nfor Spatial Clustering of\nStandardized Sentencing",
          palette = "BuPu",
          legend.reverse = TRUE,
          n = 5,
          midpoint = NA,
          style = "kmeans") +
  tm_borders(col = border.col) +
  tm_legend(
    legend.position = c("left", "bottom"),
    legend.text.size = 1, 
    legend.title.size = 1.5) +  
  tm_shape(bg) +
  tm_fill(col = neighbors.col) +
  tm_borders(col="gray35") +
  tm_shape(plantation_belt) +
  tm_borders(col = belt.col, lwd = 2) +
  tm_compass(position = c("center", "bottom"),
             size = 1,
             fontsize = .75) +
  tm_layout(bg.color = bg.color,
            frame.double.line = F,
            legend.format = list(digits = 1)) 

tmap_save(tm = aple_map,
          filename = "figures/supplementary-SSR-clusters-APLE.png",
          units = "in", width = 6.75)

# Raw SSR map

breaks = seq(0, 4, by = .5)
raw_ssr <- tm_shape(fl) +
  tm_fill("SSR_raw",
          breaks = breaks,
          # style = "cont",
          title = "Raw Standardized\nSentencing Ratios\n(1905-1910)") +
  tm_borders(col = border.col) +
  # tm_shape(bg) +
  # tm_fill(col = neighbors.col) +
  # tm_borders(col="gray35") +
  tm_shape(plantation_belt) +
  tm_borders(col = belt.col,
             lwd = 2) +
  tm_compass(position = c("center", "bottom"),
             size = 1,
             fontsize = .75) +
  tm_layout(bg.color = bg.color,
            frame.double.line = F,
            legend.format = list(digits = 0)) +
  tm_legend(legend.format = list(digits = 2))


ssr <- tm_shape(fl) +
  tm_fill("LEB",
          breaks = breaks,
          # style = "cont",
          title = "Local EB Estimated Standardized\nSentencing Ratios (1905-1910)") +
  tm_borders(col = border.col) +
  # tm_shape(bg) +
  # tm_fill(col = neighbors.col) +
  # tm_borders(col="gray35") +
  tm_shape(plantation_belt) +
  tm_borders(col = belt.col,
             lwd = 2) +
  tm_compass(position = c("center", "bottom"),
             size = 1,
             fontsize = .75) +
  tm_layout(bg.color = bg.color,
            frame.double.line = F,
            legend.format = list(digits = 0)) +
  tm_legend(legend.format = list(digits = 2))

tmap_save(tm = raw_ssr,
          filename = "figures/supplementary-raw-SSR.png",
          units = "in", width = 6.75)

tmap_save(tm = ssr,
          filename = "figures/supplementary-local-EB-SSR.png",
          units = "in", width = 6.75)


