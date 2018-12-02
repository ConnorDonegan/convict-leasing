
library(brms)
library(tidyverse)

load("bayesian-models/fit2.Rdata")
load("data/sents-model-data.Rdata")

d <- cbind(fit2$data, predict(fit2, probs = c(.05, .95))) %>%
  inner_join(dplyr::select(sents, name, expected_sents, pop, eb_ratio),
             by = "name") %>%
  mutate(Estimate = round(Estimate),
         srate = 1e3*(Estimate/pop),
         rawratio = sents / expected_sents,
         sratio = Estimate/expected_sents,
         lwr = Q5/expected_sents,
         upr = Q95/expected_sents)

# counties with urban areas
urban_counties <- tibble(County = c("ESCAMBIA", "LEON", "DUVAL", "HILLSBOROUGH", "DADE", "MONROE"),
                         City = c("Pensacola", "Tallahassee", "Jacksonville", "Tampa", "Miami", "Key West"))

# table of observed sentences, estimates, SIRS and 95% credible intervals

gather_intervals <- function(lwr, upr) {
  paste0(
    "[", lwr, ", ", upr, "]" 
  )
}

dtable <- d %>%
  # filter(name %in% cnties) %>%
  dplyr::select(name, plantation_belt, sents, expected_sents, Estimate, srate, rawratio, sratio, lwr, upr) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  arrange(desc(sratio)) %>%
  mutate(cred_interval = gather_intervals(lwr, upr)) %>%
  transmute(County = name,
            Plantation_Belt = plantation_belt,
            Sentences = sents,
            Expected_Sentences = round(expected_sents),
            Model_Estimate = Estimate,
            Sentencing_Rate = srate,
            Raw_SIR = rawratio,
            Model_SIR = sratio,
            cred_intervals = cred_interval
  ) %>%
  left_join(urban_counties) %>%
  as.tibble

save(dtable, file = "data/table-of-model-results.Rdata")
rm(list=ls())

 # produce latex table for the Appendix 
library(xtable)
load("data/table-of-model-results.Rdata")

dtable %>%
  select(-c(Sentencing_Rate, City)) %>%
  mutate(County = str_to_title(County)) %>%
  # head(5) %>%
  xtable(floating = TRUE, 
         caption = "County sentencing data",
         digits = c(0, 1, 1, 0, 0, 0, 2, 2, 1),
         align = c("l", rep("c", 7), "r"),
         latex.environments = "center") %>%
  print(include.rownames = FALSE)




