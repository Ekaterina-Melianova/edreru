// dep1b.do 

// Attempt to replicate Arrazola and de Hevia, Weber et al

// Set wd
cd C:\Country\Russia\Data\SEASHELL\SEABYTE\edreru\Stata

// Preamble
version 16
set more off
capture log close
log using dep1b, replace

capture program drop nldep1b

use df_18f

nl dep1b @ lnwage exper edu_yrs tlabor0, parameters(lnW bk delta alpha) initial(lnW 10 bk .1 delta .05 alpha .5)




