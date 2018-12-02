pkgs <- c("rstan", "tidyverse", "tidybayes") # see trees data, or nottem for time series
lapply(pkgs, library, character.only = TRUE)

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

data(trees)

x <- cbind(trees$Girth)
x2 <- cbind(trees$Height)
y <- trees$Volume

nd_x <- matrix(seq(6, 21, length.out = nrow(x)), ncol=1)
nd_x2 = matrix(0, ncol=1,nrow = nrow(nd_x))

data_list <- list(x = scale(x),
                  x2 = scale(x2),
                  y = as.numeric(scale(y)),
                  N = nrow(x), 
                  D = ncol(x),
                  D2 = ncol(x2),
                  nd_N = nrow(nd_x),
                  nd_x = nd_x,
                  nd_x2 = nd_x2)

# election model, missing spatial structure
load("data/model-matrix.Rdata")
y <- votes$GOP_growth
x <- cbind(votes$unemployment, votes$white, votes$obama_pct)
x2 <- cbind(votes$log_education)

ndxx <- seq(-2,2,length.out=17)

nd_x <- expand.grid(ndxx, ndxx) %>%
  as.tibble %>%
  rowid_to_column(var="id") %>%
  full_join(
    tibble(id = 1:310)
  ) %>% 
  transmute(Var1 = ifelse(is.na(Var1), 0, Var1),
         Var2 = ifelse(is.na(Var2), 0, Var2),
         Var3 = 0) %>%
  as.matrix(ncol = 3)

nd_x2 <- cbind(rep(0, nrow(x))) 

data_list <- list(x = scale(x),
                  x2 = scale(x2),
                  y = as.numeric(scale(y)),
                  N = nrow(x), 
                  D = ncol(x),
                  nd_x = nd_x,
                  nd_x2 = nd_x2)

model_debug = stan(file = "stan/GP-with-linpred-newdata.stan",
                   data = data_list, 
                   iter=500, cores = 1, 
                   chains=1)


samples <- ext_samples(model_debug
                       , params = "y_pred"
                       )

gd <- samples %>%
  as.tibble %>%
 gather(key = point, value = value) %>%
  group_by(point) %>%
  median_hdi(value, .width = .65) %>%
  mutate(point = str_extract(point, "[:digit:]+")) %>%
  inner_join(
    nd_x %>% 
      as.tibble %>%
      select(Var1, Var2) %>%
      rowid_to_column(var = "point") %>%
      mutate(point = as.character(point)) %>%
      slice(1:175) %>% distinct(Var1,Var2,.keep_all=T),
    by = "point"
  ) %>%
  mutate(very_white = factor(Var2 > -.5))
  
gd %>%
  ggplot() +
  geom_ribbon(aes(Var1, ymin =.lower, ymax =.upper, fill = very_white),
              alpha = .5) +
  geom_point(aes(x=Var1, y=value, colour = Var2)) +
  geom_smooth(aes(Var1, value, fill = very_white), alpha=.25) +
  theme_dark()

# g<-gam(Volume ~ s(Girth)+ s(Height),data=trees)  
# plot(g)  
  
  
  
  
  
