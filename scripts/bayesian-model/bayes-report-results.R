
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

theme_set(  theme_bw() + 
              theme(plot.background = element_rect(fill  = "gray"),
                    plot.title = element_text(size = 12.5),
                    axis.title = element_text(size = 10)))

confidence_plot <- d %>%
  arrange(sratio) %>%
  mutate(name = factor(name, ordered = T, levels = name)) %>%
  select(name, rawratio, sratio, eb_ratio, lwr, upr, plantation_belt) %>%
  gather(key = key, value = value, -c(name, lwr, upr, plantation_belt)) %>%
  ggplot() +
  geom_point(aes(name, value, col = key)) +
  geom_hline(yintercept = 1, alpha = .5) +
  geom_errorbar(aes(name, ymin=lwr, ymax = upr),
                col = "purple1",
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

ggsave(confidence_plot,
       file = "figures/confidence-plot.png",
       dpi = 600,
       units = "in",
       width = 7, height = 8)











