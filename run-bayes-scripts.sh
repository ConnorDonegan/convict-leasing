

# run scripts in this order to get figures and table with model estimated SIRs

# interpolate county population between Decennial Censuses for 1900-1920
Rscript scripts/data-prep/sentencing-rates-prep.R

# count total person-years and sentences by county, 
 # calculate log of expected sentences,
 # and add local empirical bayes estimates for mean sentencing count/rate/ratio
Rscript scripts/data-prep/bayes-data-prep.R

# fit multi-level bayesian poisson model to estimate 'shrunken' rates for each county
Rscript scripts/bayesian-model/bayesian-model.R

# compare model specifications: distinct distribution of rates in plantation belt or not
# Rscript scripts/bayesian-model/bayes-model-compare.R

# compare estimates and raw values by table of results and plot of all estimates
Rscript scripts/figures/appendix-table-of-results.R
Rscript scripts/figures/bayes-plot-estimates.R

# make maps of plantation belt and standardized sentencing ratios
Rscript scripts/figures/figure4-maps.R
