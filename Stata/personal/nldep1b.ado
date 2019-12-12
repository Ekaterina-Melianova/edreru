// nldep1b.ado : Replicates Sylvain Weber Model II Delta parameter varies by education level, Alpha same
// No z covariates

program nldep1b

version 16

syntax varlist if , at(name)

local lnwage : word 1 of `varlist'
local exper : word 2 of `varlist'
local edu_yrs : word 3 of `varlist'
local tlabor0 : word 4 of `varlist'
local d_voc : word 5 of `varlist'
local d_uni : word 6 of `varlist'

#delimit ; 

tempname lnW bk delta_base delta_voc delta_uni alpha_base 
;

#delimit cr

scalar `lnW'   = `at'[1,1]
scalar `bk'    = `at'[1,2]
scalar `delta_base' = `at'[1,3]
scalar `delta_voc' = `at'[1,4]
scalar `delta_uni' = `at'[1,5]
scalar `alpha_base' = `at'[1,6]

local deltab_term `delta_base'
local alphab_term `alpha_base'
local dv_term `delta_voc'
local du_term `delta_uni'

#delimit ; 

tempvar aterm dterm;  
generate double `dterm' = `deltab_term' +`dv_term'*`d_voc' + `du_term'*`d_uni' `if'; 
generate double `aterm' = `alphab_term' `if';  

replace `lnwage' = `lnW' + `bk'
*
(
(1-`dterm')^`exper'*(`edu_yrs')
+`aterm'
*(1-(1-`dterm')^`exper')/`dterm'
*(1+(1-`dterm')/`dterm'/`tlabor0')
-`aterm'*`exper'/`tlabor0'/`dterm'
)
+ ln(1-(`aterm'-`aterm'*`exper'/`tlabor0')) 

`if'
;
#delimit cr

end



