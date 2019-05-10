
library(tidybayes)








ext_samples <- function(fit, params = NULL, regex = TRUE) {
  s <- fit@sim$samples[[1]]
  l <- length(s[[1]])
  if(is.null(params)) return(matrix(unlist(s), nrow = l, dimnames = list(NULL, names(s))))
  if(regex) {
    s <- s[grep(params, names(s))]
    return(matrix(unlist(s), nrow = l, dimnames = list(NULL, names(s))))
  } else
  s <- s[which(names(s) %in% params)]
  matrix(unlist(s), nrow = l, dimnames = list(NULL, names(s)))
}
samples <- ext_samples(fit, params = "SSR", regex = TRUE)


s %>%
  gather(key = param, value = value) %>% 
  group_by(param) %>% 
  median_hdi(value) %>% 
  mutate(countyid = as.numeric(str_extract(param, "[:digit:]+"))) %>%
  left_join(distinct(dcounty, county, countyid), by = "countyid") %>%
  arrange(value) %>%
  mutate(county = factor(county, ordered = T,
                         levels = unique(county))) %>%
  ggplot() +
  geom_errorbar(aes(county, ymin = .lower, ymax=.upper),
                width=.1,col="purple",alpha=.5) +
  geom_point(aes(county, value)) +
  coord_flip()



samples <- ext_samples(fit, params = "bpop", regex = TRUE)
 
get_year <- function(J) start[J]:end[J]

samples %>%
  as.tibble %>%
  select(get_year(40)) %>%
  gather(key= year, value = value) %>%
  mutate(year = str_extract(year, "[:digit:]+")) %>%
  group_by(year) %>%
  median_hdi(value) %>%
  ggplot() + 
  geom_ribbon(aes(as.numeric(year), ymin = .lower, ymax = .upper),
              fill = "red", alpha = .5)




