


# load packages ====

pkgs <- c("tidyverse","xtable", "tmap")
lapply(pkgs, require, character.only=TRUE); rm(pkgs)

# load simple features file with local EB and Moran's I estimates

fl <- read_rds("data/constructed/florida-1910-sp.rds")

tbl <- fl@data %>%
  dplyr::select(c(County = name, 
                  `Plantation Belt` = plantation_belt, 
                   Sentences = sents, 
                  `Expected Sentences` = expected_sents, 
                  `Raw SSR` = SSR_raw, 
                  `Local EB SSR` = LEB)) %>%
  dplyr::mutate(County = str_to_title(County),
         `Plantation Belt` = as.character(`Plantation Belt`),
         Sentences = as.character(Sentences),
         `Expected Sentences` = as.character(round(`Expected Sentences`)),
         `Raw SSR` = round(`Raw SSR`, 2),
         `Local EB SSR` = round(`Local EB SSR`, 2)) %>%
  arrange(desc(`Local EB SSR`)) %>%
  xtable()


print(xtable(tbl, caption = "County sentencing data (1905-1910) and SSR estimates.", 
             align = c('l', rep('c',6))), digits = 2, include.rownames = FALSE)
