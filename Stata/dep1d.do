// dep1d.do 

// Attempt to replicate Arrazola and de Hevia, Weber et al Model IV

// Set wd
cd C:\Country\Russia\Data\SEASHELL\SEABYTE\edreru\Stata

// Preamble
version 16
set more off
capture log close
log using dep1d, replace

capture program drop nldep1d

// Females

use df_18f
nl dep1d @ lnwage exper edu_yrs tlabor0 d_voc d_uni, parameters(lnW bk delta_base delta_voc delta_uni alpha_base alpha_voc alpha_uni) initial(lnW 10 bk .13 delta_base .02 delta_voc .02 delta_uni .02 alpha_base .5 alpha_voc .5 alpha_uni .5)


// Males

use df_18m
nl dep1d @ lnwage exper edu_yrs tlabor0 d_voc d_uni, parameters(lnW bk delta_base delta_voc delta_uni alpha_base alpha_voc alpha_uni) initial(lnW 10 bk .13 delta_base .02 delta_voc .02 delta_uni .02 alpha_base .5 alpha_voc .5 alpha_uni .5)


