// dep_all.do 

// Attempt to replicate Arrazola and de Hevia, Weber et al Model I, II, II, IV

// Need to unstall for easier tex tables
* ssc install estout, replace

// Set wd
cd C:\Country\Russia\Data\SEASHELL\SEABYTE\edreru\Stata

// Preamble
version 16
set more off
capture log close
log using dep1a, replace

// Females

use df_18f, clear
eststo: nl dep1a @ lnwage exper edu_yrs tlabor0, parameters(lnW bk delta alpha) initial(lnW 20 bk .2 delta .05 alpha .5) vce(robust) 
eststo: nl dep1b @ lnwage exper edu_yrs tlabor0 d_voc d_uni, parameters(lnW bk delta_base delta_voc delta_uni alpha) initial(lnW 10 bk .13 delta_base .02 delta_voc .02 delta_uni .02 alpha .5)
eststo: nl dep1c @ lnwage exper edu_yrs tlabor0 d_voc d_uni, parameters(lnW bk delta_base alpha_base alpha_voc alpha_uni) initial(lnW 10 bk .13 delta_base .02 alpha_base .5 alpha_voc .5 alpha_uni .5)
eststo: nl dep1d @ lnwage exper edu_yrs tlabor0 d_voc d_uni, parameters(lnW bk delta_base delta_voc delta_uni alpha_base alpha_voc alpha_uni) initial(lnW 10 bk .13 delta_base .02 delta_voc .02 delta_uni .02 alpha_base .5 alpha_voc .5 alpha_uni .5)


// Males

use df_18m, clear
eststo: nl dep1a @ lnwage exper edu_yrs tlabor0, parameters(lnW bk delta alpha) initial(lnW 20 bk .2 delta .05 alpha .5) vce(robust) 
eststo: nl dep1b @ lnwage exper edu_yrs tlabor0 d_voc d_uni, parameters(lnW bk delta_base delta_voc delta_uni alpha) initial(lnW 10 bk .13 delta_base .02 delta_voc .02 delta_uni .02 alpha .5)
eststo: nl dep1c @ lnwage exper edu_yrs tlabor0 d_voc d_uni, parameters(lnW bk delta_base alpha_base alpha_voc alpha_uni) initial(lnW 10 bk .13 delta_base .02 alpha_base .5 alpha_voc .5 alpha_uni .5)
eststo: nl dep1d @ lnwage exper edu_yrs tlabor0 d_voc d_uni, parameters(lnW bk delta_base delta_voc delta_uni alpha_base alpha_voc alpha_uni) initial(lnW 10 bk .13 delta_base .02 delta_voc .02 delta_uni .02 alpha_base .5 alpha_voc .5 alpha_uni .5)


* TeX Males
esttab using all_tbl.tex, ar2 se aic bic title(Empirical Estimates for Females and Males) ///
nonumbers mtitles("I" "II" "III" "IV" "I" "II" "III" "IV" ) 


eststo clear








