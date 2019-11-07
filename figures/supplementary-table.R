

library(tidyverse)
library(xtable)
library(scales)

fl <- read_rds("data/florida-1910-sp.rds")

d <- fl@data

tbl <- d %>% 
  select(c(County = full_name, 
           `Plantation Belt` = plantation_belt, 
           Sentences = sents, 
           `Expected Sentences` = expected_sents, 
           # `White Population-Years` = wpop, 
           # `Black Population-Years` = bpop, 
           `SSR Raw` = SSR_raw, 
           `Local EB SSR` = LEB)) %>%
  dplyr::mutate(County = stringr::str_to_title(County),
         Sentences = as.character(round(Sentences)),
         `Plantation Belt` = as.character(round(`Plantation Belt`)),
         # `White Population-Years` = comma(`White Population-Years`),
         # `Black Population-Years` = comma(`Black Population-Years`)
         `Expected Sentences` = as.character(round(`Expected Sentences`))
         ) %>%
  arrange(desc(`Local EB SSR`)) 

print(xtable(tbl, caption = "County sentencing data (1905-1919) and SSR estimates.", 
             align = c('l', rep('c',6))), digits = 2, include.rownames = FALSE)
    