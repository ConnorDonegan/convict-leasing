

library(tidyverse)
library(rstan)

d1 <- read_csv("data/state-pop-totals.csv") %>%
  mutate(black = black + other) %>%
  full_join(
    tibble(year = 1900:1920), by = "year"
  ) %>%
  arrange(year) %>%
  mutate(white = zoo::na.approx(white),
         black = zoo::na.approx(black)) %>%
  filter(between(year, 1905, 1919)) %>%
  inner_join(
    read_csv("data/prisoner_char.csv") %>% 
      transmute(total_sents = total_sentences,
                year = year,
                bsents = black_male + black_female,
                wsents = white_male + white_female),
    by = "year"
  )  %>%
  select(year, white, black, bsents, wsents, total_sents) 


d2 <- read_csv("data/sentencing.csv") %>%
  select(county, year, sentences, population_black, population_white) %>%
  arrange(county) %>%
  mutate(countyid = factor(county, 
                           levels = unique(county), labels = 1:length(unique(county)))) %>%
  mutate(countyid = as.numeric(as.character(countyid))) %>%
  na.omit   

