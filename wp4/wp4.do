use "C:\Country\Russia\Data\SEASHELL\SEABYTE\edreru\wp4\Rosstat18.dta", clear

*create log file*
log using "C:\Country\Russia\Data\SEASHELL\SEABYTE\edreru\wp4\wp42", text


* Rosstat 2018
ivregress 2sls lnwage exper exper2 (edu_yrs  = high_n)
est sto ros1_18
ivregress 2sls lnwage exper exper2 (edu_yrs  = HSGPER) 
est sto ros2_18
ivregress 2sls lnwage exper exper2 (edu_yrs  = s1z) 
est sto ros3_18
ivregress 2sls lnwage exper exper2 (edu_yrs  = migrationrate) 
est sto ros4_18
ivregress 2sls lnwage exper exper2 (edu_yrs  = women2menratio)
est sto ros5_18
ivregress 2sls lnwage exper exper2 (edu_yrs  = marriagerate) 
est sto ros6_18
ivregress 2sls lnwage exper exper2 (edu_yrs  = fem_ind_prop)
est sto ros7_18 
ivregress 2sls lnwage exper exper2 (edu_yrs  = Literacy_97) 
est sto ros8_18

esttab ros1_18 ros2_18 ros3_18 ros4_18 ros5_18 ros6_18 ros7_18 ros8_18, b(a2) se(2) nogaps  compress ///
 mtitle("high_n" "HSGPER" "s1z" "migrate" "women2men" "marrate" "fem_ind_prop" "Literacy_97")

use "C:\Country\Russia\Data\SEASHELL\SEABYTE\edreru\wp4\Rosstat15.dta", clear

* Rosstat 2015
ivregress 2sls lnwage exper exper2 (edu_yrs  = high_n)
est sto ros1_15
ivregress 2sls lnwage exper exper2 (edu_yrs  = HSGPER) 
est sto ros2_15
ivregress 2sls lnwage exper exper2 (edu_yrs  = s1z) 
est sto ros3_15
ivregress 2sls lnwage exper exper2 (edu_yrs  = migrationrate) 
est sto ros4_15
ivregress 2sls lnwage exper exper2 (edu_yrs  = women2menratio)
est sto ros5_15
ivregress 2sls lnwage exper exper2 (edu_yrs  = marriagerate) 
est sto ros6_15
ivregress 2sls lnwage exper exper2 (edu_yrs  = fem_ind_prop)
est sto ros7_15 
ivregress 2sls lnwage exper exper2 (edu_yrs  = Literacy_97) 
est sto ros8_15

esttab ros1_15 ros2_15 ros3_15 ros4_15 ros5_15 ros6_15 ros7_15 ros8_15, b(a2) se(2) nogaps  compress ///
 mtitle("high_n" "HSGPER" "s1z" "migrate" "women2men" "marrate" "fem_ind_prop" "Literacy_97")

use "C:\Country\Russia\Data\SEASHELL\SEABYTE\edreru\wp4\rlms18_15.dta", clear

* RLMS 2018

ivregress 2sls lnwage exper exper2 (edu_yrs  = high_n) if YEAR == 2018
est sto rlms1_18
ivregress 2sls lnwage exper exper2 (edu_yrs  = HSGPER) if YEAR == 2018
est sto rlms2_18
ivregress 2sls lnwage exper exper2 (edu_yrs  = s1z) if YEAR == 2018
est sto rlms3_18
ivregress 2sls lnwage exper exper2 (edu_yrs  = migrationrate) if YEAR == 2018
est sto rlms4_18
ivregress 2sls lnwage exper exper2 (edu_yrs  = women2menratio) if YEAR == 2018
est sto rlms5_18
ivregress 2sls lnwage exper exper2 (edu_yrs  = marriagerate) if YEAR == 2018
est sto rlms6_18
ivregress 2sls lnwage exper exper2 (edu_yrs  = fem_ind_prop)if YEAR == 2018
est sto rlms7_18 
ivregress 2sls lnwage exper exper2 (edu_yrs  = Literacy_97) if YEAR == 2018
est sto rlms8_18

esttab rlms1_18 rlms2_18 rlms3_18 rlms4_18 rlms5_18 rlms6_18 rlms7_18 rlms8_18, b(a2) se(2) nogaps  compress ///
 mtitle("high_n" "HSGPER" "s1z" "migrate" "women2men" "marrate" "fem_ind_prop" "Literacy_97")

* RLMS 2015

ivregress 2sls lnwage exper exper2 (edu_yrs  = high_n) if YEAR == 2015
est sto rlms1_15
ivregress 2sls lnwage exper exper2 (edu_yrs  = HSGPER) if YEAR == 2015
est sto rlms2_15
ivregress 2sls lnwage exper exper2 (edu_yrs  = s1z) if YEAR == 2015
est sto rlms3_15
ivregress 2sls lnwage exper exper2 (edu_yrs  = migrationrate) if YEAR == 2015
est sto rlms4_15
ivregress 2sls lnwage exper exper2 (edu_yrs  = women2menratio) if YEAR == 2015
est sto rlms5_15
ivregress 2sls lnwage exper exper2 (edu_yrs  = marriagerate) if YEAR == 2015
est sto rlms6_15
ivregress 2sls lnwage exper exper2 (edu_yrs  = fem_ind_prop)if YEAR == 2015
est sto rlms7_15 
ivregress 2sls lnwage exper exper2 (edu_yrs  = Literacy_97) if YEAR == 2015
est sto rlms8_15

esttab rlms1_15 rlms2_15 rlms3_15 rlms4_15 rlms5_15 rlms6_15 rlms7_15 rlms8_15, b(a2) se(2) nogaps  compress ///
 mtitle("high_n" "HSGPER" "s1z" "migrate" "women2men" "marrate" "fem_ind_prop" "Literacy_97")

 log close


