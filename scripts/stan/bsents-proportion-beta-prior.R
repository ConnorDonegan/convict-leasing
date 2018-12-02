
 # get scale and shape parameters for the beta prior on proportion of prisoners who are black
d <- read_csv("data/prisoner_char.csv") %>% 
  transmute(total_sents = total_sentences,
            year = year,
            bsents = black_male + black_female,
            wsents = white_male + white_female,
            brate = bsents / total_sentences) %>%
  filter(year < 1920)
  # filter(between(year, 1905, 1919))

mu = median(d$brate,na.rm=T) 
k = sum(!is.na(d$brate)) 
a = mu*k
b = (1 - mu)*k

plot(density(na.omit(d$brate)),
     col='red', 
     lwd = 2,
     main = "Proprotion of prisoners who are black",
     sub = paste0("brate ~ beta(", round(a), ", ", round(b), ")"),
     xlab = "",
     ylab = "Density")
for(i in 1:100) {
  lines(density(rbeta(n=1e3, shape1 = a, shape2 = b)), col = "lightblue")
}

print(c(a, b, k))




