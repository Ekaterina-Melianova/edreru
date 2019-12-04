// dep1b.do 

// Attempt to replicate Arrazola and de Hevia, Weber et al model II

// Set wd
cd C:\Country\Russia\Data\SEASHELL\SEABYTE\edreru\Stata

// Preamble
version 16
set more off
capture log close
log using dep1b, replace

capture program drop nldep1b

// Females

use df_18f
nl dep1b @ lnwage exper edu_yrs tlabor0 d_voc d_uni, parameters(lnW bk delta_base delta_voc delta_uni alpha) initial(lnW 10 bk .13 delta_base .02 delta_voc .02 delta_uni .02 alpha .5)

// Males

use df_18m
nl dep1b @ lnwage exper edu_yrs tlabor0 d_voc d_uni, parameters(lnW bk delta_base delta_voc delta_uni alpha) initial(lnW 10 bk .13 delta_base .02 delta_voc .02 delta_uni .02 alpha .5)



