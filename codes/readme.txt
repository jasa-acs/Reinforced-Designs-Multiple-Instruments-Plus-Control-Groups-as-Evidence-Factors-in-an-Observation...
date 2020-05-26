The R code for the simulation results in Table 2 and Table 3.

simualtiontable_power.R   creates Table 2.
simualtiontable_ds.R      creates Table 3.
functions.R 		      code is sourced by the R file 
				simualtiontable_ds.R

The R code are written in a way that they can be run in parallel.
In each R code, computation of one Table is split into four sets.


External R packages needed:
simualtiontable_power.R    'AER' and 'senstrat'
simualtiontable_ds.R       'yaml'
