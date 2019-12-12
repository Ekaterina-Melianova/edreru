// nldep1aZ.ado : Replicates Sylvain Weber Model I Alpha and Delta parameters same for all education levels
// Template with single z covariates

program nldep1aZ

version 16

syntax varlist if , at(name)

local lnwage : word 1 of `varlist'
local exper : word 2 of `varlist'
local edu_yrs : word 3 of `varlist'
local tlabor0 : word 4 of `varlist'
local RTI : word 5 of `varlist'

#delimit ; 

tempname lnW bk delta alpha xb_RTI
;

#delimit cr

scalar `lnW'   = `at'[1,1]
scalar `bk'    = `at'[1,2]
scalar `delta' = `at'[1,3]
scalar `alpha' = `at'[1,4]
scalar `xb_RTI'= `at'[1,5]

local delta_term `delta'
local alpha_term `alpha'
local xb_RTIterm `xb_RTI'

#delimit ; 

replace `lnwage' = `lnW' + `bk'
*
(
(1-`delta_term')^`exper'*(`edu_yrs')
+`alpha_term'
*(1-(1-`delta_term')^`exper')/`delta_term'
*(1+(1-`delta_term')/`delta_term'/`tlabor0')
-`alpha_term'*`exper'/`tlabor0'/`delta_term'
) 
+ln(1-(`alpha_term'-`alpha_term'*`exper'/`tlabor0')) 
+`xb_RTI'*`RTI'

`if'
;
#delimit cr

end




