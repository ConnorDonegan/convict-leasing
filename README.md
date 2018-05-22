# convict-leasing

Beginning in 1877, all Florida State prisoners were to be leased to private businesses as a servile labor force subject to corporal punishment. By the end of the program in 1919 over 10,000 Florida residents and visitors were sentenced to forced labor, the vast majority of them being young black males. 

This data was compiled from the Biennial Reports of the Commissioner of Agriculture for the State of Florida, who ran the prison system, and is supplemented with US Census data. The full data set includes county sentencing rates, prisoner demographics (race, gender, place of origin), and prison mortality (annual counts, rates, and age on date of death for select years) as well as county characteristics including population by race, percent of land area in agriculture in 1910, and an indicator for counties in the plantation belt.

This repo currently includes the following files:

  - `sentencing-prep.R` interpolates county population between Decennial Censuses for 1900-1920 for use in calculating mean sentencing rates.

  - `sentencing-rates-maps.R` calculates and maps county sentencing rates by three methods
      - raw estimates: sentences / population
      - Local empirical Bayes estimates
      - standardized rates that account for the racial composition of each county population 
      
  - `convict-leasing-revenue.R` plots the annual revenue derived from leasing state prisoners, 1877-1919.
      
  - `mortality.R` produces bar plots illustrating the distribution of prisoner deaths by age, as well as frequency of death by age group and race in the Florida general population; and plots the number of deaths and mortality rate among state prisoners (1870-1925).
  
  - `prisoner-characteristics.R` generates figures on state prison sentences by race, gender, and place of origin; as well as pardons and pardon rates by race.
      
  - `figures` contains `.png` files of all maps and plots.
 
 The standardized sentencing rates from `sentencing-rates-maps.R` follow a common approach in epidemiology (Standardized Mortality Rates (SMR) or Standardized Incidence Rates (SIR)) where local counts are compared to their expected risk-adjusted count using the ratio, observed/expected. The expected number of sentences is based on the average rate for whites and blacks respectively in the State of Florida. This method is used because we lack county-level data on the racial composition of state prison sentencing, but we do have counts of sentences for each county. The result is an easily interpreted score: a county with an SIR less than one has fewer sentences than expected given its demographics while scores above one saw higher than expected sentencing rates. 

