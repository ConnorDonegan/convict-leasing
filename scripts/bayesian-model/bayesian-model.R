
library(tidyverse)
library(brms)
source("scripts/bayesian-model/bayes-data-prep.R")

form <- bf(sents ~ 1 + (1|name) + offset(log_expectation))

# get_prior(form,
#           data=sents,
#           family = poisson(),
#           autocor = cor_car(W, ~ 1 | name))

car_prior <- c(
             prior(uniform(0, 1), class = car),
             prior(normal(.5, .2), class = sdcar)
                     )

mod <- brm(form,
           autocor = cor_car(W, ~1|name),
           data = sents, 
           prior = car_prior,
           iter = 4e3,
           warmup = 3e3,
           control = list(max_treedepth = 22, 
                          adapt_delta = .99),
           family = poisson())

 # check the fit
pp_check(mod)

save(mod, file = "scripts/bayesian-model/bayes-fit.Rdata")

 # plot the sentencing ratios with credible intervals
d <- cbind(mod$data, predict(mod, probs = c(.05, .95))) %>%
  inner_join(dplyr::select(sents, name, plantation_belt, expected_sents, pop),
             by = "name") %>%
  mutate(srate = Estimate/pop,
         rawratio = sents / expected_sents,
         sratio = Estimate/expected_sents,
         lwr = Q5/expected_sents,
         upr = Q95/expected_sents)

confidence_plot <- d %>%
  arrange(sratio) %>%
  mutate(name = factor(name, ordered = T, levels = name)) %>%
  ggplot() +
  geom_point(aes(name, rawratio), col = "darkgray") +
  geom_point(aes(name, sratio), col = "firebrick") +
  geom_errorbar(aes(name, ymin = lwr, ymax = upr,
                    col = factor(plantation_belt)),
                width = .2) +
  scale_color_manual(name = "Plantation\nBelt",
                     values = c("darkgray", "purple2"),
                       breaks = c("0", "1"),
                       labels = c("No", "Yes")) +
  labs(y = "Ratio", x = "",
       caption = "Raw ratios are the gray points, model-predicted estimates are the red points.",
       title = "Standardized sentencing ratios with 95% credible intervals") +
  coord_flip() +
  theme_gray() +
  theme(plot.title = element_text(size = 11),
        legend.title = element_text(size = 9),
        legend.position = c(.8, .33))

ggsave(confidence_plot,
       file = "figures/confidence-plot.png",
        dpi = 600,
       units = "in",
       width = 7, height = 8)











