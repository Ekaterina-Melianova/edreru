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

nl (lnwage = {lnW=10} + {bk}*((1-{delta})^exper*(edu_yrs)+{alpha}(1-(1-{delta})^exper)/{delta}*(1+((1-{delta})/({delta}*tlabor0))-{alpha}*exper/`tlabor0'/`delta_term') \\\
+ln(1-(`alpha_term'-((`alpha_term'*`exper')/`tlabor0'))))



