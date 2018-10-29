

library(brms)
library(tidyverse)

load("bayesian-models/fit1.Rdata")
load("bayesian-models/fit2.Rdata")

waic(fit1, fit2)

pp_check(fit1)
pp_check(fit2)
