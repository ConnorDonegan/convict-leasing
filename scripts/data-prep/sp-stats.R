

 ## prepare model matrix and spatial weights matrix for local empirical bayes estimates ##

# load packages ====

pkgs <- c("tidyverse", "GISTools", "maptools", "sf", 
          "USAboundaries", "USAboundariesData", "spdep")
lapply(pkgs, require, character.only=TRUE); rm(pkgs)

# load and set projection for the historic map of Florida ====

fl <- us_counties(map_date="1910-01-01", 
                  resolution="high", 
                  states = "Florida") 

fl <- as(fl, "Spatial")

FLaea <- "+proj=aea +lat_1=24 +lat_2=31.5 +lat_0=24 +lon_0=-84 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs" 

fl <- spTransform(fl, CRS(FLaea))

# load data and calculate mean (state wide) sentencing rates by race during the convict leasing program ====

# the full period is defined by between(1904, 1920)
start_year <- 1904  
end_year <- 1911


dcounty <- read_csv("data/sentencing.csv") %>%
  dplyr::filter(between(year, start_year, end_year))

bpop <- sum(dcounty$population_black, na.rm=T)
wpop <- sum(dcounty$population_white, na.rm=T)

dstate <- read_csv("data/prisoner_char.csv")

dstate <- dstate %>%
  dplyr::select(year, black_male, white_male, black_female, white_female) %>%
  transmute(Year = year,
            black = black_male + black_female,
            white = white_male + white_female) %>%
  filter(Year > start_year & Year < end_year)

bsents <- sum(dstate$black, na.rm=T)
wsents <- sum(dstate$white, na.rm=T)

brate <- (bsents / bpop)
wrate <- (wsents / wpop)

# aggregate data by county ====

sents <- dcounty %>%
  mutate(name = toupper(county),
         population = population_black + population_white) %>%
  # filter(year > 1904 & year < 1920) %>% 
  group_by(name) %>%
  dplyr::summarize(sents = sum(sentences, na.rm=T),
                   pop = sum(population, na.rm=T),
                   wpop = sum(population_white, na.rm = T),
                   bpop = sum(population_black, na.rm = T),
                   plantation_belt = unique(plantation_belt),
                   pct_ag_1910 = unique(pct_agricultural_1910)) %>%
  mutate(expected_sents = wpop*wrate + bpop*brate) %>%
  ungroup() 

# ensure row index of sentencing data conforms to spatial weights matrix #
fl@data <- inner_join(fl@data, sents, by = "name")
neighbs <- poly2nb(fl)
W <- nb2mat(neighbs, style = "B")
row.names(W) <- fl$name 

# calculate local empirical bayes
# using expected value as 'at risk population' to produce risk ratios or SIRs 
lEB <- EBlocal(ri = fl@data$sents, ni = fl@data$expected_sents, nb = neighbs)
fl@data$LEB <- lEB$est
fl@data$SSR_raw <- lEB$raw

spplot(fl[,'LEB'])

# get local moran's I test statistics to identify clusters of high and low risk ratios
lisa <- localmoran(lEB$est, listw = nb2listw(neighbs), p.adjust.method = "holm")
lisa <- as_tibble(lisa)

# store the Z scores and p-values
fl@data$Z.i <- lisa$Z.Ii
fl@data$P.i <- lisa$`Pr(z > 0)`

fl@data$sig <- fl@data$P.i < .1

spplot(fl[,'sig'])

# aple instead of I---gives substantively similar results as Moran's I for our purposes.
fl@data$aple <- localAple(scale(lEB$est, scale = FALSE)[,1], listw = nb2listw(neighbs, style = "W"))

spplot(fl[,'aple'])

write_rds(fl, path = "data/florida-1910-sp.rds")



