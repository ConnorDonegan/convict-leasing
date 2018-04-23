# convict-leasing

Beginning in 1877, all Florida State prisoners were to be leased to private businesses as a servile labor force subject to corporal punishment. By the end of the program in 1919 over 10,000 Florida residents and visitors were sentenced to forced labor, the vast majority of them being young black males. 

This data was compiled from the Biennial Reports of the Commissioner of Agriculture for the State of Florida, who ran the prison system, and is supplemented with US Census data. The full data set includes county sentencing rates, prisoner demographics (race, gender, place of origin), and prison mortality (annual counts, rates, and age on date of death for select years) as well as county characteristics including population by race, percent of land area in agriculture in 1910, and an indicator for counties in the plantation belt.

This repo currently includes the following files:

  - `sentencing-prep.R` interpolates county population between Decennial Censuses for 1900-1920 for use in calculating mean sentencing rates.

  - `sentencing-rates-maps.R` calculates and maps county sentencing rates by three methods
      - raw estimates: sentences / population
      - Local empirical Bayes estimates
      - standardized rates that account for the racial composition of each county population 
      
  - `mortality-by-age.R` produces bar plots illustrating the distribution of prisoner deaths by age, and as well as the ages of death for whites and blacks in the Florida general population.
      
  - `figures` contains `.png` files of all maps and plots.
 
 The standardized sentencing rates from `sentencing-rates-maps.R` follow a common approach in epidemiology, where local counts are compared to their expected risk-adjusted count: (observed - expected)^2/expected. This script calculates those as well as [Dykes and Unwin's](http://www.agocg.ac.uk/reports/visual/casestud/dykes/dykes.pdf "Maps of the Census: a rough guide") method which preserves the sign of the difference, to identify counties above and below the expected rate of occurrence: (observed - expected)/sqrt(expected). 
The standardized rates calculated here also make use of the local EB estimates by making the observed count of sentences equal to the local EB rate*population. 

