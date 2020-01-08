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

********************************************************************* 2018 **************************************************************************
// Females

use df_6yrsf, clear
keep if YEAR == 2018
eststo: nl dep1a @ lnwage exper edu_yrs tlabor0, parameters(lnW bk delta alpha) initial(lnW 20 bk .2 delta .05 alpha .5) vce(robust) 
eststo: nl dep1b @ lnwage exper edu_yrs tlabor0 d_voc d_uni, parameters(lnW bk delta_base delta_voc delta_uni alpha) initial(lnW 10 bk .13 delta_base .02 delta_voc .02 delta_uni .02 alpha .5)
eststo: nl dep1c @ lnwage exper edu_yrs tlabor0 d_voc d_uni, parameters(lnW bk delta_base alpha_base alpha_voc alpha_uni) initial(lnW 10 bk .13 delta_base .02 alpha_base .5 alpha_voc .5 alpha_uni .5)
eststo: nl dep1d @ lnwage exper edu_yrs tlabor0 d_voc d_uni, parameters(lnW bk delta_base delta_voc delta_uni alpha_base alpha_voc alpha_uni) initial(lnW 10 bk .13 delta_base .02 delta_voc .02 delta_uni .02 alpha_base .5 alpha_voc .5 alpha_uni .5)


// Males

use df_6yrsm, clear
keep if YEAR == 2018
eststo: nl dep1a @ lnwage exper edu_yrs tlabor0, parameters(lnW bk delta alpha) initial(lnW 20 bk .2 delta .05 alpha .5) vce(robust) 
eststo: nl dep1b @ lnwage exper edu_yrs tlabor0 d_voc d_uni, parameters(lnW bk delta_base delta_voc delta_uni alpha) initial(lnW 10 bk .13 delta_base .02 delta_voc .02 delta_uni .02 alpha .5)
eststo: nl dep1c @ lnwage exper edu_yrs tlabor0 d_voc d_uni, parameters(lnW bk delta_base alpha_base alpha_voc alpha_uni) initial(lnW 10 bk .13 delta_base .02 alpha_base .5 alpha_voc .5 alpha_uni .5)
eststo: nl dep1d @ lnwage exper edu_yrs tlabor0 d_voc d_uni, parameters(lnW bk delta_base delta_voc delta_uni alpha_base alpha_voc alpha_uni) initial(lnW 10 bk .13 delta_base .02 delta_voc .02 delta_uni .02 alpha_base .5 alpha_voc .5 alpha_uni .5)


* TeX
esttab using all_tbl.tex, ar2 se aic bic title(Empirical Estimates for Females and Males) ///
nonumbers mtitles("I" "II" "III" "IV" "I" "II" "III" "IV" ) 
eststo clear

**************************************************************** 6 years Model I ********************************************************************
// Females

use df_6yrsf, clear
keep if YEAR == 1994
eststo: nl dep1a @ lnwage exper edu_yrs tlabor0, parameters(lnW bk delta alpha) initial(lnW 20 bk .2 delta .05 alpha .5) vce(robust) 
use df_6yrsf, clear
keep if YEAR == 1998
eststo: nl dep1a @ lnwage exper edu_yrs tlabor0, parameters(lnW bk delta alpha) initial(lnW 20 bk .2 delta .05 alpha .5) vce(robust) 
use df_6yrsf, clear
keep if YEAR == 2003
eststo: nl dep1a @ lnwage exper edu_yrs tlabor0, parameters(lnW bk delta alpha) initial(lnW 20 bk .2 delta .05 alpha .5) vce(robust) 
use df_6yrsf, clear
keep if YEAR == 2006
eststo: nl dep1a @ lnwage exper edu_yrs tlabor0, parameters(lnW bk delta alpha) initial(lnW 20 bk .2 delta .05 alpha .5) vce(robust) 
use df_6yrsf, clear
keep if YEAR == 2012
eststo: nl dep1a @ lnwage exper edu_yrs tlabor0, parameters(lnW bk delta alpha) initial(lnW 20 bk .2 delta .05 alpha .5) vce(robust) 
use df_6yrsf, clear
keep if YEAR == 2018
eststo: nl dep1a @ lnwage exper edu_yrs tlabor0, parameters(lnW bk delta alpha) initial(lnW 20 bk .2 delta .05 alpha .5) vce(robust) 

* TeX Females
esttab using 6yrs_tbl_f.tex, se title(Empirical Estimates for Females, Model I) ///
nonumbers mtitles("1994" "1998" "2003" "2006" "2012" "2018")
eststo clear

// Males

use df_6yrsm, clear
keep if YEAR == 1994
eststo: nl dep1a @ lnwage exper edu_yrs tlabor0, parameters(lnW bk delta alpha) initial(lnW 20 bk .2 delta .05 alpha .5) vce(robust) 
use df_6yrsm, clear
keep if YEAR == 1998
eststo: nl dep1a @ lnwage exper edu_yrs tlabor0, parameters(lnW bk delta alpha) initial(lnW 20 bk .2 delta .05 alpha .5) vce(robust) 
use df_6yrsm, clear
keep if YEAR == 2003
eststo: nl dep1a @ lnwage exper edu_yrs tlabor0, parameters(lnW bk delta alpha) initial(lnW 20 bk .2 delta .05 alpha .5) vce(robust) 
use df_6yrsm, clear
keep if YEAR == 2006
eststo: nl dep1a @ lnwage exper edu_yrs tlabor0, parameters(lnW bk delta alpha) initial(lnW 20 bk .2 delta .05 alpha .5) vce(robust) 
use df_6yrsm, clear
keep if YEAR == 2012
eststo: nl dep1a @ lnwage exper edu_yrs tlabor0, parameters(lnW bk delta alpha) initial(lnW 20 bk .2 delta .05 alpha .5) vce(robust) 
use df_6yrsm, clear
keep if YEAR == 2018
eststo: nl dep1a @ lnwage exper edu_yrs tlabor0, parameters(lnW bk delta alpha) initial(lnW 20 bk .2 delta .05 alpha .5) vce(robust) 

* TeX Males
esttab using 6yrs_tbl_m.tex, se title(Empirical Estimates for Males, Model I) ///
nonumbers mtitles("1994" "1998" "2003" "2006" "2012" "2018")
eststo clear







