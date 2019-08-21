
library(tidyverse)
library(brms)

load("data/sents-model-data.Rdata")

  # model standard deviation of the intercepts
form1 <- bf(sents ~ 1 + (1 | name) + offset(log_expectation))

control <- list(max_treedepth = 25,
                adapt_delta = .99)

fit1 <- brm(form1,
            data = sents,
            cores = 3,
            iter = 20e3,
            thin = 10,
            control = control,
            family = poisson(),
            file = "bayesian-models/fit1")

  # same but sd is distinct within and outside of the plantation belt
form2 <- bf(sents ~ 1 + (1 | gr(name, by = plantation_belt)) + offset(log_expectation))

fit2 <- brm(form2,
            data = sents, 
            cores = 3,
            iter = 20e3,
            thin = 10,
            control = control,
            family = poisson(),
            file = "bayesian-models/fit2")

  # add an ICAR component for spatial smoothing
load("data/spatial-weights-matrix.Rdata")
fit1.2 <- update(fit1, autocor = cor_car(W, ~1|name), iter = 40e3, file = "bayesian-models/fit1-car")
fit2.2 <- update(fit2, autocor = cor_car(W, ~1|name), iter = 40e3, file = "bayesian-models/fit2-car")


