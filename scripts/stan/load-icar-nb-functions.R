
# load spatial functions from stan-dev ICAR model github page
library(RCurl)
script <- getURL("https://raw.githubusercontent.com/stan-dev/example-models/master/knitr/car-iar-poisson/nb_data_funs.R")
eval(parse(text = script))