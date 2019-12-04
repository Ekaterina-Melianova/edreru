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

nl dep1a @ lnwage exper edu_yrs tlabor0, parameters(lnW bk delta alpha) initial(lnW 4 bk .2 delta .25 alpha .05)

nl dep1a @ lnwage exper edu_yrs tlabor0, parameters(lnW bk delta alpha) initial(lnW 20 bk .2 delta .15 alpha .2)

nl dep1a @ lnwage exper edu_yrs tlabor0, parameters(lnW bk delta alpha) initial(lnW 20 bk .2 delta .005 alpha .5) vce(robust) nolog coeflegend


use df_18m, clear
nl dep1a @ lnwage exper edu_yrs tlabor0, parameters(lnW bk delta alpha) initial(lnW 20 bk .2 delta .05 alpha .5) vce(robust) 

use df_18f, clear
nl dep1a @ lnwage exper edu_yrs tlabor0, parameters(lnW bk delta alpha) initial(lnW 20 bk .2 delta .05 alpha .5) vce(robust) 


