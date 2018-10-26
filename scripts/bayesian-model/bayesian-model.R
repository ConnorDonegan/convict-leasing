
library(tidyverse)
library(brms)
load("data/sents-model-data.Rdata")

  # model standard deviation of the intercepts
form1 <- bf(sents ~ 1 + (1 | name) + offset(log_expectation))

control <- list(max_treedepth = 20,
                adapt_delta = .97)

fit1 <- brm(form1,
            data = sents,
            cores = 3,
            control = control,
            family = poisson())
save(fit1, file = "bayesian-models/fit1.Rdata")

  # same but sd is distinct within and outside of the plantation belt
form2 <- bf(sents ~ 1 + (1 | gr(name, by = plantation_belt)) + offset(log_expectation))

fit2 <- brm(form2,
            data = sents, 
            cores = 3,
            control = control,
            family = poisson())

save(fit2, file = "bayesian-models/fit2.Rdata")
