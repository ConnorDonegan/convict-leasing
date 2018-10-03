

# Floria Convict Leasing Data
# Interpolate county population between census years, merge with county sentencing data
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Sources.
# All population data is from United States Decennial Census. https://www.census.gov/prod/www/decennial.html

# 1900 total population counts are from the 1900 (or Twelfth)
  # Census of the United States, Volume 1, Section 4, Population of states and territories
  # by counties, at each census: 1790 to 1900. Page 13. United States Census Office (1901).
  # http://www2.census.gov/prod2/decennial/documents/33405927v1.zip

  # 1900 detail population counts are from Volume 1, Section 9, 
  # Table 19. White, Negro, and Indian population by counties: 1880 to 1900.
  # Page 532-533. United States Census Office (1901).
  # http://www2.census.gov/prod2/decennial/documents/33405927v1.zip

    # * Population totals for 1900 in many cases do not equal the sum of the racial components. Some counts were updated
    # in 1910, but still do not sum correctly. 
    # Summary counts for the State of Florida in 1900 are not reported here
    # because they are far too inconsistent (relative to the county totals and to the total population count) to be reliable.*

# 1910 population counts are from the 1910 (or Thirteenth) 
  # Census of Population and Housing, Supplement for Florida,
  # Chapter 2, 'Table I. Population and characteristics for the state and for counties.' 
  # Government Priting Office (1913). Pages 590-599. 
  # http://www2.census.gov/prod2/decennial/documents/41033935v9-14.zip

# Percent of land area in agriculture is also from 
  # the 1910 Census of Population and Housing, Supplement for Florida,
  # Chapter 3, 'Table 1. Farms and Farm Property, by counties, Apr. 15, 1910,'
  # pages 622-626.

# 1920 population counts are from the 1920 (Fourteenth) 
  # Census of the United States, State Compendium Florida,
  # Table 9. Composition and characteristics of the population, for counties, 1920.
  # Government Printing Office (1924). Pages 29-34. 
  # http://www2.census.gov/prod2/decennial/documents/06229686v8-13.zip
  

# Sentencing data is from the Biennial Reports of the Commissioner of Agriculture for the State of Florida
  # Original copies of the reports are held in the Florida State Library. 
  # Some digitcal copies can be found online: https://catalog.hathitrust.org/Record/008893986
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# load packages ====
pkgs <- c("zoo", "tidyverse")
lapply(pkgs, library, character.only=TRUE)

# check for consistency of census population counts ====

  # The author entered all prison and census data by hand.
  # The following process was undertaken to find transcription errors. 
  # In the process, errors in the original file were identified.
  # See that racial components of each county population sum to the total county population.
census <- read.csv("data/FL_pop_char.csv")
attach(census)
# all 1920 figures are consistent except for Pinellas (off by 6 people). This is an error in the original census file.
Pinellas <- which(pop_native_white_1920 + pop_foreign_white_1920 + pop_other_1920 + pop_black_1920 != pop_1920)
census$County[Pinellas]
(pop_native_white_1920 + pop_foreign_white_1920 + pop_other_1920 + pop_black_1920 - pop_1920)[Pinellas]

# All values for 1910 are consistent
all((pop_white_1910 + pop_black_1910 + pop_other_1910 == pop_1910), na.rm=T)

# 1900 has numerous errors. Again, these are all in the original census file, including the 1910 updates.
# The discrepancies are minor.
errors <- which(pop_white_1900 + pop_black_1900 + pop_amerindian_1900 != pop_1900)
census$County[errors]
na.omit(pop_white_1900 + pop_black_1900 + pop_amerindian_1900 - pop_1900) 
detach(census)
rm(census)

# load sentencing and Census data ====

census <- read_csv("data/FL_pop_char.csv")[-68, ] %>%
  filter(!is.na(pop_1910)) %>%
  mutate(pop_nonWhite_1900 = pop_black_1900 + pop_amerindian_1900,
         pop_nonWhite_1910 = pop_black_1910 + pop_other_1910,
         pop_nonWhite_1920 = pop_black_1920 + pop_other_1920,
         pop_white_1920 = pop_foreign_white_1920 + pop_native_white_1920)

white.population <- census[, c('county', 'pop_white_1900', 'pop_white_1910', 'pop_white_1920')]
black.population <- census[, c('county', 'pop_nonWhite_1900', 'pop_nonWhite_1910', 'pop_nonWhite_1920')]
colNames <- c("county", "pop_1900", "pop_1910", "pop_1920")
names(black.population) <- colNames
names(white.population) <- colNames

sents.index <- c(1, 
               grep("sentences", x = names(census)),
               grep("pct_agricultural", x = names(census)))
sentences <- census[, sents.index]

# interpolate population between census years ====

  # linear interpolation of inter-cencus population data may have a much higher
  # range of error when population is small, particularly when under 5,000. 
  # See Wedden et al., Evaluating Linearly Interpolated Intercensal Estimates...',
  # Population Research and Policy Review (2015).
  # Half of the counties have white population over 5,000 in 1900,
  # Only about 70% of black population counts were over 5,000 in 1900.
quantile(black.population$pop_1900, na.rm=T, probs = seq(0, 1, .1))
quantile(white.population$pop_1900, na.rm=T, probs = seq(0, 1, .1))

population_interpolation <- function(data) {
  
  counties <- data$county
  n.years = 21
  n.counties = length(counties)
  start <- 1900
  end = 1920
  years <- seq(start, end, 1)
  
  # empty matrix to fill with interpolated population counts
  fill <-  data.frame(matrix(NA, n.years, n.counties))
  names(fill) <- counties
  row.names(fill) <- years
  
  fill[which(row.names(fill) == 1900), ] <- data$pop_1900
  fill[which(row.names(fill) == 1910), ] <- data$pop_1910
  fill[which(row.names(fill) == 1920), ] <- data$pop_1920
  
  fill <- zoo::na.approx(fill) %>%
    as.tibble() %>%
    mutate(year = years) %>%
    gather(key = county, value = population, -year) %>%
    arrange(year, county) 
  
  return(fill)
}

white.population <- population_interpolation(white.population) %>%
  rename(population_white = population)

black.population <- population_interpolation(black.population) %>%
  rename(population_black = population)

# merge ====

sentences <- sentences %>% 
  gather(key = year, value = sentences, -c(county, pct_agricultural_1910)) %>%
  mutate(year = as.numeric(sub(pattern = "sentences_", replacement = "", year))) %>%
  arrange(year, county) %>%
  merge(black.population, by = c("county", "year")) %>%
  merge(white.population, by = c("county", "year")) %>%
  mutate(plantation_belt = ifelse(pct_agricultural_1910 > 33, 1, 0)) %>%
  filter(year > 1904 & year < 1920) %>%
  as.tibble()

# save ====
write_csv(sentences, "data/sentencing.csv")




