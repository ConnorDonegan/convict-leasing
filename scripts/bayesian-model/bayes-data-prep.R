

 ## prepare model matrix and spatial weights matrix for the bayesian model ##

# load packages ====

pkgs <- c("tidyverse", "classInt", "RColorBrewer", "GISTools", "maptools", "sf", 
          "ggmap", "tmap", "USAboundaries", "spdep", "brms")
lapply(pkgs, require, character.only=TRUE); rm(pkgs)

# load and set projection for the historic map of Florida ====

fl <- us_counties(map_date="1910-01-01", 
                  resolution="high", 
                  states = "Florida") 
fl <- as(fl, "Spatial")
FLaea <- "+proj=aea +lat_1=24 +lat_2=31.5 +lat_0=24 +lon_0=-84 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs" 
fl <- spTransform(fl, CRS(FLaea))

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

# aggregate data by county ====

sents <- dcounty %>%
  mutate(name = toupper(county),
         population = population_black + population_white) %>%
  filter(year > 1904 & year < 1920) %>% 
  group_by(name) %>%
  dplyr::summarize(sents = sum(sentences, na.rm=T),
                   pop = sum(population, na.rm=T),
                   wpop = sum(population_white, na.rm = T),
                   bpop = sum(population_black, na.rm = T),
                   plantation_belt = unique(plantation_belt)) %>%
  ungroup() 

# ensure row index of sents data conforms to spatial weights matrix #
neighbs <- poly2nb(fl)
W <- nb2mat(neighbs, style = "B")

fl@data <- inner_join(fl@data, sents, by = "name")

sents <- fl@data %>%
  mutate( expected_sents = wpop*wrate + bpop*brate,
    eb_Rate = EBlocal(ri = sents, ni = pop, nb = neighbs)$est
    ) %>%
  mutate(eb_ratio = eb_Rate*pop / expected_sents,
         raw_ratio = sents / expected_sents) %>%
  mutate(plantation_belt = factor(plantation_belt),
         log_expectation = log(expected_sents),
         pop = round(pop))

row.names(W) <- fl$name 







