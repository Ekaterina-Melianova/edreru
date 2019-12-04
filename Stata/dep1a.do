// dep1a.do 

// Attempt to replicate Arrazola and de Hevia, Weber et al Model I

// Set wd
cd C:\Country\Russia\Data\SEASHELL\SEABYTE\edreru\Stata

// Preamble
version 16
set more off
capture log close
log using dep1a, replace

// Females

use df_18f, clear
nl dep1a @ lnwage exper edu_yrs tlabor0, parameters(lnW bk delta alpha) initial(lnW 20 bk .2 delta .05 alpha .5) vce(robust) 

// Males

use df_18m, clear
nl dep1a @ lnwage exper edu_yrs tlabor0, parameters(lnW bk delta alpha) initial(lnW 20 bk .2 delta .05 alpha .5) vce(robust) 


