

library(brms)
library(tidyverse)

load("bayesian-models/fit2.Rdata")
load("data/sents-model-data.Rdata")

theme_set(  theme_bw() + 
              theme(plot.background = element_rect(fill  = "gray"),
                    plot.title = element_text(size = 12.5),
                    axis.title = element_text(size = 10)))



d <- cbind(fit2$data, predict(fit2, probs = c(.05, .95))) %>%
  inner_join(dplyr::select(sents, name, expected_sents, pop, eb_ratio),
             by = "name") %>%
  mutate(Estimate = round(Estimate),
         srate = 1e3*(Estimate/pop),
         rawratio = sents / expected_sents,
         sratio = Estimate/expected_sents,
         lwr = Q5/expected_sents,
         upr = Q95/expected_sents)


confidence_plot <- d %>%
  arrange(sratio) %>%
  mutate(name = factor(name, ordered = T, levels = name)) %>%
  select(name, rawratio, sratio, eb_ratio, lwr, upr, plantation_belt) %>%
  gather(key = key, value = value, -c(name, lwr, upr, plantation_belt)) %>%
  ggplot() +
  geom_point(aes(name, value, col = key)) +
  geom_hline(yintercept = 1, alpha = .5) +
  geom_errorbar(aes(name, ymin=lwr, ymax = upr),
                col = "gray50",
                alpha = .2,
                width = .2) +
  scale_y_continuous(breaks = seq(0, 5, by = 0.5)) +
  scale_color_brewer(type = "qual", 
                     palette  = 2,
                     breaks = c("rawratio",
                                "eb_ratio",
                                "sratio"),
                     labels = c("Raw", 
                                "Local\nEmpirical\nBayes", 
                                "Bayesian\nModel"),
                     name = "Estimate") +
  labs(y = NULL, x = NULL,
       title = "Standardized sentencing ratios with 95% Bayesian credible intervals") +
  coord_flip() +
  # theme_classic() +
  theme(plot.title = element_text(size = 11),
        legend.title = element_text(size = 9),
        legend.position = c(.8, .33)) 

confidence_plot

ggsave(confidence_plot,
       file = "figures/confidence-plot.png",
       dpi = 600,
       units = "in",
       width = 7, height = 8)











