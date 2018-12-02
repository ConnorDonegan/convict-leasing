data {
  // prison sentences by county-year with county-year population estimates
  int N;      // observations
  int J;      // counties
  int y[N];   // sentences (per county-year)
  int start[J]; // row index, first and last, for each county
  int end[J];
  int<lower=1,upper=47> countyid[N];    // county of each observation 
  vector<lower=0>[N] wpop_meas; // white and black population estimates for each county-year
  vector<lower=0>[N] bpop_meas;
  real<lower=0,upper=1> tau;  // standard deviation of population estimates 
  // FL population estimates and statewide sentences, both annual by race
  
  
}








