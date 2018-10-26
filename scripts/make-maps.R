



# load packages ====
pkgs <- c("tidyverse", "classInt", "RColorBrewer", "GISTools", "maptools", "sf", 
          "ggmap", "tmap", "USAboundaries", "spdep", "sp", "raster", "scales", "pracma",
          "magick")
lapply(pkgs, require, character.only=TRUE); rm(pkgs)
load("data/table-of-model-results.Rdata")
