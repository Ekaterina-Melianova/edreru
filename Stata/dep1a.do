// dep1a.do 

// Attempt to replicate Arrazola and de Hevia, Weber et al

// Set wd
cd C:\Country\Russia\Data\SEASHELL\SEABYTE\edreru\Stata

// Preamble
version 16
set more off
capture log close
log using dep1a, replace

use df_18f

nl dep1a @ lnwage exper edu_yrs tlabor0, parameters(lnW bk delta alpha) initial(lnW 10 bk .1 delta .05 alpha .5)



