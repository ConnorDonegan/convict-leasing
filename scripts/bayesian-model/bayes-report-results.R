
library(brms)
library(tidyverse)
load("bayesian-models/fit2.Rdata")

# plot the sentencing ratios with credible intervals
d <- cbind(fit2$data, predict(mod, probs = c(.05, .95))) %>%
  inner_join(dplyr::select(sents, name, expected_sents, pop),
             by = "name") %>%
  mutate(Estimate = round(Estimate),
    srate = 1e3*(Estimate/pop),
         rawratio = sents / expected_sents,
         sratio = Estimate/expected_sents,
         lwr = Q5/expected_sents,
         upr = Q95/expected_sents)

# table of observed sentences, estimates, SIRS and 95% credible intervals

gather_intervals <- function(x, y, z) {
  paste0(
    x,
    " [", y, ", ", z, "]" 
  )
}

dtable <- d %>%
  # filter(name %in% cnties) %>%
  dplyr::select(name, plantation_belt, sents, Estimate, pop, srate, rawratio, sratio, lwr, upr) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  arrange(desc(sratio)) %>%
  mutate(sratio = gather_intervals(sratio, lwr, upr)) %>%
  transmute(County = name,
            Plantation_Belt = plantation_belt,
         Sentences = sents,
         Estimate = Estimate,
    population_years = pop,
    Sentencing_Rate = srate,
    Raw_SIR = rawratio,
    Model_SIR = sratio
    ) %>%
  as.tibble

save(dtable, file = "data/table-of-model-results.Rdata")

theme_set(  theme_bw() + 
              theme(plot.background = element_rect(fill  = "gray"),
                    plot.title = element_text(size = 12.5),
                    axis.title = element_text(size = 10)))

confidence_plot <- d %>%
  arrange(sratio) %>%
  mutate(name = factor(name, ordered = T, levels = name)) %>%
  ggplot() +
  geom_point(aes(name, rawratio), col = "darkgray") +
  geom_hline(yintercept = 1, alpha = .5) +
  geom_point(aes(name, sratio), col = "firebrick") +
  geom_errorbar(aes(name, ymin = lwr, ymax = upr,
                    col = factor(plantation_belt)),
                width = .2) +
  scale_y_continuous(breaks = seq(0, 5, by = 0.5)) +
  scale_color_manual(name = "Plantation\nBelt",
                     values = c("darkgray", "purple2"),
                     breaks = c("0", "1"),
                     labels = c("No", "Yes")) +
  labs(y = "Standardized Sentencing Ratio", x = "",
       caption = "Raw ratios are the gray points, model-predicted estimates are the red points.",
       title = "Standardized sentencing ratios with 95% credible intervals") +
  coord_flip() +
  # theme_classic() +
  theme(plot.title = element_text(size = 11),
        legend.title = element_text(size = 9),
        legend.position = c(.8, .33)) 

ggsave(confidence_plot,
       file = "figures/confidence-plot.png",
       dpi = 600,
       units = "in",
       width = 7, height = 8)











