program nldepreciation1_educ6

version 10

syntax varlist if , at(name)

local lincome_gross : word 1 of `varlist'
local experience : word 2 of `varlist'
local educ6 : word 3 of `varlist'
local tlabor0 : word 4 of `varlist'
local married : word 5 of `varlist'
local separated : word 6 of `varlist'
local dep : word 7 of `varlist'
local lmat : word 8 of `varlist'
local city : word 9 of `varlist'
local tenure_10 : word 10 of `varlist'
local permit1 : word 11 of `varlist'
local permit2 : word 12 of `varlist'
local continent2 : word 13 of `varlist'
local continent3 : word 14 of `varlist'
local continent4 : word 15 of `varlist'
local continent5 : word 16 of `varlist'
local continent6 : word 17 of `varlist'
local continent7 : word 18 of `varlist'
local continent8 : word 19 of `varlist'
local unemp1 : word 20 of `varlist'
local unemp2 : word 21 of `varlist'
local unemp3 : word 22 of `varlist'
local unemp4 : word 23 of `varlist'
local subord0 : word 24 of `varlist'
local subord11 : word 25 of `varlist'
local subord12 : word 26 of `varlist'
local subord13 : word 27 of `varlist'
local subord14 : word 28 of `varlist'
local firmsize11 : word 29 of `varlist'
local firmsize12 : word 30 of `varlist'
local firmsize13 : word 31 of `varlist'
local firmsize14 : word 32 of `varlist'
local sector3 : word 33 of `varlist'
local sector4 : word 34 of `varlist'
local sector5 : word 35 of `varlist'
local sector6 : word 36 of `varlist'
local sector7 : word 37 of `varlist'
local sector8 : word 38 of `varlist'
local sector9 : word 39 of `varlist'
local sector10 : word 40 of `varlist'
local sector11 : word 41 of `varlist'
local sector12 : word 42 of `varlist'
local sector13 : word 43 of `varlist'
local sector14 : word 44 of `varlist'
local sector15 : word 45 of `varlist'
local sector16 : word 46 of `varlist'
local sector17 : word 47 of `varlist'
local canton1 : word 48 of `varlist'
local canton3 : word 49 of `varlist'
local canton4 : word 50 of `varlist'
local canton5 : word 51 of `varlist'
local canton6 : word 52 of `varlist'
local canton7 : word 53 of `varlist'
local canton8 : word 54 of `varlist'
local canton9 : word 55 of `varlist'
local canton10 : word 56 of `varlist'
local canton11 : word 57 of `varlist'
local canton12 : word 58 of `varlist'
local canton13 : word 59 of `varlist'
local canton14 : word 60 of `varlist'
local canton15 : word 61 of `varlist'
local canton16 : word 62 of `varlist'
local canton17 : word 63 of `varlist'
local canton18 : word 64 of `varlist'
local canton19 : word 65 of `varlist'
local canton20 : word 66 of `varlist'
local canton21 : word 67 of `varlist'
local canton22 : word 68 of `varlist'
local canton23 : word 69 of `varlist'
local canton24 : word 70 of `varlist'
local canton25 : word 71 of `varlist'
local canton26 : word 72 of `varlist'
local year1999 : word 73 of `varlist'
local year2000 : word 74 of `varlist'
local year2001 : word 75 of `varlist'
local year2002 : word 76 of `varlist'
local year2003 : word 77 of `varlist'
local year2004 : word 78 of `varlist'
local year2005 : word 79 of `varlist'
local year2006 : word 80 of `varlist'
local year2007 : word 81 of `varlist'
local year2008 : word 82 of `varlist'




#d ;

tempname lnW bk delta alpha 
xb_married xb_separated xb_dep xb_lmat xb_city xb_tenure_10 xb_permit1 xb_permit2 xb_continent2 xb_continent3 xb_continent4 xb_continent5 xb_continent6 xb_continent7 xb_continent8 xb_unemp1 xb_unemp2 xb_unemp3 xb_unemp4 xb_subord0 xb_subord11 xb_subord12 xb_subord13 xb_subord14 xb_firmsize11 xb_firmsize12 xb_firmsize13 xb_firmsize14 xb_sector3 xb_sector4 xb_sector5 xb_sector6 xb_sector7 xb_sector8 xb_sector9 xb_sector10 xb_sector11 xb_sector12 xb_sector13 xb_sector14 xb_sector15 xb_sector16 xb_sector17 xb_canton1 xb_canton3 xb_canton4 xb_canton5 xb_canton6 xb_canton7 xb_canton8 xb_canton9 xb_canton10 xb_canton11 xb_canton12 xb_canton13 xb_canton14 xb_canton15 xb_canton16 xb_canton17 xb_canton18 xb_canton19 xb_canton20 xb_canton21 xb_canton22 xb_canton23 xb_canton24 xb_canton25 xb_canton26 xb_year1999 xb_year2000 xb_year2001 xb_year2002 xb_year2003 xb_year2004 xb_year2005 xb_year2006 xb_year2007 xb_year2008
;

#d cr

scalar `lnW' = `at'[1,1]
scalar `bk' = `at'[1,2]
scalar `delta' = `at'[1,3]
scalar `alpha' = `at'[1,4]
scalar `xb_married' = `at'[1,5]
scalar `xb_separated' = `at'[1,6]
scalar `xb_dep' = `at'[1,7]
scalar `xb_lmat' = `at'[1,8]
scalar `xb_city' = `at'[1,9]
scalar `xb_tenure_10' = `at'[1,10]
scalar `xb_permit1' = `at'[1,11]
scalar `xb_permit2' = `at'[1,12]
scalar `xb_continent2' = `at'[1,13]
scalar `xb_continent3' = `at'[1,14]
scalar `xb_continent4' = `at'[1,15]
scalar `xb_continent5' = `at'[1,16]
scalar `xb_continent6' = `at'[1,17]
scalar `xb_continent7' = `at'[1,18]
scalar `xb_continent8' = `at'[1,19]
scalar `xb_unemp1' = `at'[1,20]
scalar `xb_unemp2' = `at'[1,21]
scalar `xb_unemp3' = `at'[1,22]
scalar `xb_unemp4' = `at'[1,23]
scalar `xb_subord0' = `at'[1,24]
scalar `xb_subord11' = `at'[1,25]
scalar `xb_subord12' = `at'[1,26]
scalar `xb_subord13' = `at'[1,27]
scalar `xb_subord14' = `at'[1,28]
scalar `xb_firmsize11' = `at'[1,29]
scalar `xb_firmsize12' = `at'[1,30]
scalar `xb_firmsize13' = `at'[1,31]
scalar `xb_firmsize14' = `at'[1,32]
scalar `xb_sector3' = `at'[1,33]
scalar `xb_sector4' = `at'[1,34]
scalar `xb_sector5' = `at'[1,35]
scalar `xb_sector6' = `at'[1,36]
scalar `xb_sector7' = `at'[1,37]
scalar `xb_sector8' = `at'[1,38]
scalar `xb_sector9' = `at'[1,39]
scalar `xb_sector10' = `at'[1,40]
scalar `xb_sector11' = `at'[1,41]
scalar `xb_sector12' = `at'[1,42]
scalar `xb_sector13' = `at'[1,43]
scalar `xb_sector14' = `at'[1,44]
scalar `xb_sector15' = `at'[1,45]
scalar `xb_sector16' = `at'[1,46]
scalar `xb_sector17' = `at'[1,47]
scalar `xb_canton1' = `at'[1,48]
scalar `xb_canton3' = `at'[1,49]
scalar `xb_canton4' = `at'[1,50]
scalar `xb_canton5' = `at'[1,51]
scalar `xb_canton6' = `at'[1,52]
scalar `xb_canton7' = `at'[1,53]
scalar `xb_canton8' = `at'[1,54]
scalar `xb_canton9' = `at'[1,55]
scalar `xb_canton10' = `at'[1,56]
scalar `xb_canton11' = `at'[1,57]
scalar `xb_canton12' = `at'[1,58]
scalar `xb_canton13' = `at'[1,59]
scalar `xb_canton14' = `at'[1,60]
scalar `xb_canton15' = `at'[1,61]
scalar `xb_canton16' = `at'[1,62]
scalar `xb_canton17' = `at'[1,63]
scalar `xb_canton18' = `at'[1,64]
scalar `xb_canton19' = `at'[1,65]
scalar `xb_canton20' = `at'[1,66]
scalar `xb_canton21' = `at'[1,67]
scalar `xb_canton22' = `at'[1,68]
scalar `xb_canton23' = `at'[1,69]
scalar `xb_canton24' = `at'[1,70]
scalar `xb_canton25' = `at'[1,71]
scalar `xb_canton26' = `at'[1,72]
scalar `xb_year1999' = `at'[1,73]
scalar `xb_year2000' = `at'[1,74]
scalar `xb_year2001' = `at'[1,75]
scalar `xb_year2002' = `at'[1,76]
scalar `xb_year2003' = `at'[1,77]
scalar `xb_year2004' = `at'[1,78]
scalar `xb_year2005' = `at'[1,79]
scalar `xb_year2006' = `at'[1,80]
scalar `xb_year2007' = `at'[1,81]
scalar `xb_year2008' = `at'[1,82]


local delta_term `delta'
local alpha_term `alpha'

#d ;

replace `lincome_gross' = `lnW' + `bk' 
* 
(
(1-`delta_term')^`experience' * (`educ6') 
+ `alpha_term'
* (1 - (1-`delta_term')^`experience') / `delta_term' 
* (1 + (1-`delta_term') / `delta_term' / `tlabor0') 
- `alpha_term' * `experience' / `tlabor0' / `delta_term'
) 
+ ln(1 - (`alpha_term' - `alpha_term' * `experience' / `tlabor0')) 
+ `xb_married' * `married' 
+ `xb_separated' * `separated'
+ `xb_dep' * `dep'
+ `xb_lmat' * `lmat'
+ `xb_city' * `city'
+ `xb_tenure_10' * `tenure_10'
+ `xb_permit1' * `permit1'
+ `xb_permit2' * `permit2'
+ `xb_continent2' * `continent2'
+ `xb_continent3' * `continent3'
+ `xb_continent4' * `continent4'
+ `xb_continent5' * `continent5'
+ `xb_continent6' * `continent6'
+ `xb_continent7' * `continent7'
+ `xb_continent8' * `continent8'
+ `xb_unemp1' * `unemp1'
+ `xb_unemp2' * `unemp2'
+ `xb_unemp3' * `unemp3'
+ `xb_unemp4' * `unemp4'
+ `xb_subord0' * `subord0'
+ `xb_subord11' * `subord11'
+ `xb_subord12' * `subord12'
+ `xb_subord13' * `subord13'
+ `xb_subord14' * `subord14'
+ `xb_firmsize11' * `firmsize11'
+ `xb_firmsize12' * `firmsize12'
+ `xb_firmsize13' * `firmsize13'
+ `xb_firmsize14' * `firmsize14'
+ `xb_sector3' * `sector3'
+ `xb_sector4' * `sector4'
+ `xb_sector5' * `sector5'
+ `xb_sector6' * `sector6'
+ `xb_sector7' * `sector7'
+ `xb_sector8' * `sector8'
+ `xb_sector9' * `sector9'
+ `xb_sector10' * `sector10'
+ `xb_sector11' * `sector11'
+ `xb_sector12' * `sector12'
+ `xb_sector13' * `sector13'
+ `xb_sector14' * `sector14'
+ `xb_sector15' * `sector15'
+ `xb_sector16' * `sector16'
+ `xb_sector17' * `sector17'
+ `xb_canton1' * `canton1'
+ `xb_canton3' * `canton3'
+ `xb_canton4' * `canton4'
+ `xb_canton5' * `canton5'
+ `xb_canton6' * `canton6'
+ `xb_canton7' * `canton7'
+ `xb_canton8' * `canton8'
+ `xb_canton9' * `canton9'
+ `xb_canton10' * `canton10'
+ `xb_canton11' * `canton11'
+ `xb_canton12' * `canton12'
+ `xb_canton13' * `canton13'
+ `xb_canton14' * `canton14'
+ `xb_canton15' * `canton15'
+ `xb_canton16' * `canton16'
+ `xb_canton17' * `canton17'
+ `xb_canton18' * `canton18'
+ `xb_canton19' * `canton19'
+ `xb_canton20' * `canton20'
+ `xb_canton21' * `canton21'
+ `xb_canton22' * `canton22'
+ `xb_canton23' * `canton23'
+ `xb_canton24' * `canton24'
+ `xb_canton25' * `canton25'
+ `xb_canton26' * `canton26'
+ `xb_year1999' * `year1999'
+ `xb_year2000' * `year2000'
+ `xb_year2001' * `year2001'
+ `xb_year2002' * `year2002'
+ `xb_year2003' * `year2003'
+ `xb_year2004' * `year2004'
+ `xb_year2005' * `year2005'
+ `xb_year2006' * `year2006'
+ `xb_year2007' * `year2007'
+ `xb_year2008' * `year2008'

 `if'

;

#d cr

end

