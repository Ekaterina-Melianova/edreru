// dep1c.do 

// Attempt to replicate Arrazola and de Hevia, Weber et al Model III

// Set wd
cd C:\Country\Russia\Data\SEASHELL\SEABYTE\edreru\Stata

// Preamble
version 16
set more off
capture log close
log using dep1c, replace

capture program drop nldep1c

// Females

use df_18f
nl dep1c @ lnwage exper edu_yrs tlabor0 d_voc d_uni, parameters(lnW bk delta_base alpha_base alpha_voc alpha_uni) initial(lnW 10 bk .13 delta_base .02 alpha_base .5 alpha_voc .5 alpha_uni .5)

// Males

use df_18m
nl dep1c @ lnwage exper edu_yrs tlabor0 d_voc d_uni, parameters(lnW bk delta_base alpha_base alpha_voc alpha_uni) initial(lnW 10 bk .13 delta_base .02 alpha_base .5 alpha_voc .5 alpha_uni .5)


