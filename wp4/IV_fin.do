
use "C:\Country\Russia\Data\SEASHELL\SEABYTE\edreru\wp4\rlms18.dta", clear

* Install
ssc install ivreg2
ssc install ivhettest

*******************************

***** First stage: Schooling equations

***** Females
ivreg2 lnwage exper exper2 (edu_yrs  = prestige_family edu_family ///
Permskiy_Krai Tverskaya_Oblast Krasnoyarskiy_Kray Rostovskaya_Oblast) ///
urban if female == 1, savefirst robust first

eststo: estimates restore _ivreg2_edu_yrs

***** Males
ivreg2 lnwage exper exper2 (edu_yrs  = prestige_family edu_family ///
Permskiy_Krai Tambovskaya_Oblast Kabardino_Balkarskaya_Resp) /// 
urban if female == 0, savefirst robust first

eststo: estimates restore _ivreg2_edu_yrs

* TeX
esttab using iv_1st_out.tex, nogaps  compress nostar ar2 t title(Schooling Equations: Russia, 2018) ///
nonumbers mtitles("Females" "Males" ) replace 
eststo clear

***** Graph

label define fem 0 "Males" 1 "Females" 
label values female fem

* Prestige

twoway (lfit  edu_yrs prestige_family), /// 
by(female, leg(off))  ytitle(Years of education for an individual) xtitle(Occupational prestige of the family (maximum between parents) at 15 years old) /// 
scheme(s1mono)

graph export fam_prestige_schooling.png, replace

* Edu

twoway  (lfit edu_yrs edu_family), /// 
by(female, leg(off)) ytitle(Years of education for an individual) xtitle(Years of education for the family (maximum between parents) at 15 years old) /// 
scheme(s1mono)

*(scatter edu_yrs edu_family, jitter (0.5))

graph export fam_edu_schooling.png, replace

*******************************

***** Females
eststo: ivreg2 lnwage exper exper2 (edu_yrs  = prestige_family edu_family ///
Permskiy_Krai Tverskaya_Oblast Krasnoyarskiy_Kray Rostovskaya_Oblast) ///
urban if female == 1, endog(edu_yrs) robust ffirst
* Pagan–Hall for heteroskedasticity
ivhettest
*Partial R2 for excluded instruments in the first stage
matrix list e(first)
*est sto fem_rlms18

***** Males
eststo: ivreg2 lnwage exper exper2 (edu_yrs  = prestige_family edu_family ///
Permskiy_Krai Tambovskaya_Oblast Kabardino_Balkarskaya_Resp) /// 
urban if female == 0, endog(edu_yrs) robust first
* Pagan–Hall for heteroskedasticity
ivhettest
*Partial R2 for excluded instruments in the first stage
matrix list e(first)
*est sto male_rlms18


* TeX
esttab using iv_out.tex, nogaps  compress nostar z title(Returns to Education from Instrumental Variables (IV): Russia, 2018) ///
nonumbers mtitles("Females" "Males" ) replace 
eststo clear


