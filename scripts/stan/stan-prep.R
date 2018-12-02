
library(tidyverse)
library(rstan)

fl_rates <- read_csv("data/state-pop-totals.csv") %>%
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
  ) %>%
  mutate( # next, add to model: (black, white) ~ noraml(); brate=bsents/black etc.
            brate = bsents/black,
            wrate = wsents/white) %>%
  select(year, white, black, bsents, wsents, total_sents) %>%
  mutate(bsents = ifelse(is.na(bsents), round(total_sents*mean(bsents/total_sents, na.rm=T)), bsents),
         wsents = ifelse(is.na(wsents), round(total_sents*mean(wsents/total_sents, na.rm=T)), wsents)) %>%
  arrange(year) %>%
  transmute(
    year = year,
    wrate = wsents/white,
    brate = bsents/black
  )

dcounty <- read_csv("data/sentencing.csv") %>%
  select(county, year, sentences, population_black, population_white) %>%
  inner_join(fl_rates, by = "year") %>%
  select(year, county, sentences, population_black, population_white, wrate, brate) %>%
  rename("y" = sentences) %>%
  arrange(county) %>%
  mutate(countyid = factor(county, 
                         levels = unique(county), labels = 1:length(unique(county)))) %>%
  mutate(countyid = as.numeric(as.character(countyid))) %>%
  na.omit        # just for now. Palm Beach pre-1910 should be removed but at least one other should just be zero sentences

N = nrow(dcounty)
J = length(unique(dcounty$countyid))

IDs <- unique(dcounty$countyid)
start = vector(mode = "numeric", length = length(IDs))
end = vector(mode = "numeric", length = length(IDs))

for(i in IDs) {
  # get first row index, then the last one, for each countyid
  idi <- which(dcounty$countyid == i)
  start[i] <- min(idi)
  end[i] <- max(idi)
}

data_list <- list(
  N = N,
  J = J,
  start = start,
  end = end,
  y = dcounty$y,
  countyid = dcounty$countyid,
  wrate = dcounty$wrate,
  brate = dcounty$brate,
  bpop_meas = dcounty$population_black,
  wpop_meas = dcounty$population_white,
  tau = .05
)

model_debug = stan(file = "scripts/stan/poisson.stan", data = data_list, iter=100, chains=1)
fit <- stan(file = "scripts/stan/poisson.stan", data = data_list, fit = model_debug,
            iter=2e3, chains=1)
