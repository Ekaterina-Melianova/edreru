version 10

*Estimations avec nl en version "Function evaluator program"

set more off
forvalues n = 0/1 {

local sex = "males"
if `n'==1 {
	local sex = "females"
}

#d ;


di _n(2) as input "Model I - `sex'" ;

eststo modelI_`sex', r noe: nl depreciation1_educ6 @ 	
			lincome_gross experience educ6 tlabor0 married separated dep lmat 
			city tenureT_frac permit1 permit2 
			continent2 continent3 continent4 continent5 continent6 continent7 continent8 
			unemp1 unemp2 unemp3 unemp4 subord0 subord11 subord12 subord13 subord14 
			firmsize11 firmsize12 firmsize13 firmsize14 
			sector3 sector4 sector5 sector6 sector7 sector8 sector9 sector10 sector11 
			sector12 sector13 sector14 sector15 sector16 sector17 
			canton1 canton3 canton4 canton5 canton6 canton7 canton8 canton9 
			canton10 canton11 canton12 canton13 canton14 canton15 canton16 
			canton17 canton18 canton19 canton20 canton21 canton22 canton23 
			canton24 canton25 canton26 
			year1999 year2000 year2001 year2002 year2003 year2004 year2005 year2006 year2007 year2008
			
			if sex==`n' & sample & !outlier, 
			
			parameters(
			lnW bk delta alpha 
			xb_married xb_separated xb_dep xb_lmat xb_city 
			xb_tenureT_frac xb_permit1 xb_permit2 xb_continent2 xb_continent3 xb_continent4 xb_continent5 
			xb_continent6 xb_continent7 xb_continent8 xb_unemp1 xb_unemp2 xb_unemp3 xb_unemp4 
			xb_subord0 xb_subord11 xb_subord12 xb_subord13 xb_subord14 
			xb_firmsize11 xb_firmsize12 xb_firmsize13 xb_firmsize14 
			xb_sector3 xb_sector4 xb_sector5 xb_sector6 xb_sector7 xb_sector8 xb_sector9 
			xb_sector10 xb_sector11 xb_sector12 xb_sector13 xb_sector14 xb_sector15 xb_sector16 xb_sector17 
			xb_canton1 xb_canton3 xb_canton4 xb_canton5 xb_canton6 xb_canton7 xb_canton8 xb_canton9 
			xb_canton10 xb_canton11 xb_canton12 xb_canton13 xb_canton14 xb_canton15 xb_canton16 
			xb_canton17 xb_canton18 xb_canton19 xb_canton20 xb_canton21 xb_canton22 xb_canton23 xb_canton24 xb_canton25 xb_canton26 
			xb_year1999 xb_year2000 xb_year2001 xb_year2002 xb_year2003 xb_year2004 xb_year2005 xb_year2006 xb_year2007 xb_year2008
			) 
			initial(lnW 10 bk .1 delta .05 alpha .5) robust cluster(id) nolog
;


di _n(2) as input "Model II - `sex'" ;

eststo modelII_`sex', r noe: nl depreciation2_VvsA @ 	
			lincome_gross experience educ6 tlabor0 married separated dep lmat 
			city tenureT_frac permit1 permit2 
			continent2 continent3 continent4 continent5 continent6 continent7 continent8 
			unemp1 unemp2 unemp3 unemp4 subord0 subord11 subord12 subord13 subord14 
			firmsize11 firmsize12 firmsize13 firmsize14 
			sector3 sector4 sector5 sector6 sector7 sector8 sector9 sector10 sector11 
			sector12 sector13 sector14 sector15 sector16 sector17 
			canton1 canton3 canton4 canton5 canton6 canton7 canton8 canton9 
			canton10 canton11 canton12 canton13 canton14 canton15 canton16 
			canton17 canton18 canton19 canton20 canton21 canton22 canton23 
			canton24 canton25 canton26 
			year1999 year2000 year2001 year2002 year2003 year2004 year2005 year2006 year2007 year2008
			vocational academic
			
			if sex==`n' & sample & !outlier, 
			
			parameters(
			lnW bk delta_vocational delta_academic alpha 
			xb_married xb_separated xb_dep xb_lmat xb_city
			xb_tenureT_frac xb_permit1 xb_permit2 xb_continent2 xb_continent3 xb_continent4 xb_continent5 
			xb_continent6 xb_continent7 xb_continent8 xb_unemp1 xb_unemp2 xb_unemp3 xb_unemp4 
			xb_subord0 xb_subord11 xb_subord12 xb_subord13 xb_subord14 
			xb_firmsize11 xb_firmsize12 xb_firmsize13 xb_firmsize14 
			xb_sector3 xb_sector4 xb_sector5 xb_sector6 xb_sector7 xb_sector8 xb_sector9 
			xb_sector10 xb_sector11 xb_sector12 xb_sector13 xb_sector14 xb_sector15 xb_sector16 xb_sector17 
			xb_canton1 xb_canton3 xb_canton4 xb_canton5 xb_canton6 xb_canton7 xb_canton8 xb_canton9 
			xb_canton10 xb_canton11 xb_canton12 xb_canton13 xb_canton14 xb_canton15 xb_canton16 
			xb_canton17 xb_canton18 xb_canton19 xb_canton20 xb_canton21 xb_canton22 xb_canton23 xb_canton24 xb_canton25 xb_canton26 
			xb_year1999 xb_year2000 xb_year2001 xb_year2002 xb_year2003 xb_year2004 xb_year2005 xb_year2006 xb_year2007 xb_year2008
			) 
			initial(lnW 10 bk .1 delta_vocational .05 delta_academic .05 alpha .5) robust cluster(id) nolog
;
test _b[/delta_vocational] = _b[/delta_academic] = 0;
test _b[/delta_vocational] = _b[/delta_academic];
estadd scalar delta_diff = r(F);



di _n(2) as input "Model III - `sex'" ;

eststo modelIII_`sex', r noe: nl depreciation3_VvsA @ 	
			lincome_gross experience educ6 tlabor0 married separated dep lmat 
			city tenureT_frac permit1 permit2 
			continent2 continent3 continent4 continent5 continent6 continent7 continent8 
			unemp1 unemp2 unemp3 unemp4 subord0 subord11 subord12 subord13 subord14 
			firmsize11 firmsize12 firmsize13 firmsize14 
			sector3 sector4 sector5 sector6 sector7 sector8 sector9 sector10 sector11 
			sector12 sector13 sector14 sector15 sector16 sector17 
			canton1 canton3 canton4 canton5 canton6 canton7 canton8 canton9 
			canton10 canton11 canton12 canton13 canton14 canton15 canton16 
			canton17 canton18 canton19 canton20 canton21 canton22 canton23 
			canton24 canton25 canton26 
			year1999 year2000 year2001 year2002 year2003 year2004 year2005 year2006 year2007 year2008
			vocational academic
			
			if sex==`n' & sample & !outlier, 
			
			parameters(
			lnW bk delta alpha_vocational alpha_academic
			xb_married xb_separated xb_dep xb_lmat xb_city
			xb_tenureT_frac xb_permit1 xb_permit2 xb_continent2 xb_continent3 xb_continent4 xb_continent5 
			xb_continent6 xb_continent7 xb_continent8 xb_unemp1 xb_unemp2 xb_unemp3 xb_unemp4 
			xb_subord0 xb_subord11 xb_subord12 xb_subord13 xb_subord14 
			xb_firmsize11 xb_firmsize12 xb_firmsize13 xb_firmsize14 
			xb_sector3 xb_sector4 xb_sector5 xb_sector6 xb_sector7 xb_sector8 xb_sector9 
			xb_sector10 xb_sector11 xb_sector12 xb_sector13 xb_sector14 xb_sector15 xb_sector16 xb_sector17 
			xb_canton1 xb_canton3 xb_canton4 xb_canton5 xb_canton6 xb_canton7 xb_canton8 xb_canton9 
			xb_canton10 xb_canton11 xb_canton12 xb_canton13 xb_canton14 xb_canton15 xb_canton16 
			xb_canton17 xb_canton18 xb_canton19 xb_canton20 xb_canton21 xb_canton22 xb_canton23 xb_canton24 xb_canton25 xb_canton26 
			xb_year1999 xb_year2000 xb_year2001 xb_year2002 xb_year2003 xb_year2004 xb_year2005 xb_year2006 xb_year2007 xb_year2008
			) 
			initial(lnW 10 bk .1 delta .05 alpha_vocational .5 alpha_academic .5) robust cluster(id) nolog
;
di _n(1) as res "Log-likelihood = " `e(ll)' ;
test _b[/alpha_vocational] = _b[/alpha_academic] = 0;
test _b[/alpha_vocational] = _b[/alpha_academic];
estadd scalar alpha_diff = r(F);

di _n(2) as input "Model IV - `sex'" ;



eststo modelIV_`sex', r noe: nl depreciation4_VvsA @ 	
			lincome_gross experience educ6 tlabor0 married separated dep lmat 
			city tenureT_frac permit1 permit2 
			continent2 continent3 continent4 continent5 continent6 continent7 continent8 
			unemp1 unemp2 unemp3 unemp4 subord0 subord11 subord12 subord13 subord14 
			firmsize11 firmsize12 firmsize13 firmsize14 
			sector3 sector4 sector5 sector6 sector7 sector8 sector9 sector10 
			sector11 sector12 sector13 sector14 sector15 sector16 sector17 
			canton1 canton3 canton4 canton5 canton6 canton7 canton8 canton9 canton10 
			canton11 canton12 canton13 canton14 canton15 canton16 canton17 canton18 canton19 
			canton20 canton21 canton22 canton23 canton24 canton25 canton26 
			year1999 year2000 year2001 year2002 year2003 year2004 year2005 year2006 year2007 year2008
			vocational academic
			
			if sex==`n' & sample & !outlier, 

			parameters(
			lnW bk delta_vocational delta_academic alpha_vocational alpha_academic
			xb_married xb_separated xb_dep xb_lmat xb_city 
			xb_tenureT_frac xb_permit1 xb_permit2 xb_continent2 xb_continent3 xb_continent4 
			xb_continent5 xb_continent6 xb_continent7 xb_continent8 xb_unemp1 xb_unemp2 xb_unemp3 xb_unemp4 
			xb_subord0 xb_subord11 xb_subord12 xb_subord13 xb_subord14 
			xb_firmsize11 xb_firmsize12 xb_firmsize13 xb_firmsize14 
			xb_sector3 xb_sector4 xb_sector5 xb_sector6 xb_sector7 xb_sector8 xb_sector9 xb_sector10 
			xb_sector11 xb_sector12 xb_sector13 xb_sector14 xb_sector15 xb_sector16 xb_sector17 
			xb_canton1 xb_canton3 xb_canton4 xb_canton5 xb_canton6 xb_canton7 xb_canton8 xb_canton9 xb_canton10 
			xb_canton11 xb_canton12 xb_canton13 xb_canton14 xb_canton15 xb_canton16 xb_canton17 xb_canton18 xb_canton19 
			xb_canton20 xb_canton21 xb_canton22 xb_canton23 xb_canton24 xb_canton25 xb_canton26 
			xb_year1999 xb_year2000 xb_year2001 xb_year2002 xb_year2003 xb_year2004 xb_year2005 xb_year2006 xb_year2007 xb_year2008
			) 
			initial(lnW 10 bk .1 delta_vocational .05 delta_academic .05 alpha_vocational .5 alpha_academic .5) robust cluster(id) nolog
;
di _n(1) as res "Log-likelihood = " `e(ll)' ;
test _b[/delta_vocational] = _b[/delta_academic] = 0;
test _b[/alpha_vocational] = _b[/alpha_academic] = 0;
test _b[/delta_vocational] = _b[/delta_academic];
estadd scalar delta_diff = r(F);
test _b[/alpha_vocational] = _b[/alpha_academic];
estadd scalar alpha_diff = r(F);


#d cr

*lrtest (modelII_`sex' modelIII_`sex' modelIV_`sex') (modelI_`sex'), stats dir
*lrtest (modelIV_`sex') (modelII_`sex' modelIII_`sex'), stats dir
/*LR test likely invalid for models with robust vce 
r(498);*/
*Mais on peut faire un Wald test après chaque estimation, avec la commande test

*save, replace	/*to save the estimates in the database currently opened. NO: this does not work...*/



*************************************
* Sortie des tables en format .tex: *
*************************************

*cd "C:\Documents and Settings\Administrateur\Mes documents\Work\Uni\3.Thèse\Paper1\tables"
#d ;
/*
estout modelI_`sex' modelII_`sex' modelIII_`sex' modelIV_`sex' using ./tables/results_dep_`sex'_VvsA.tex, 
	cells(b(star fmt(%5.3f) vacant(\multicolumn{1}{c}{---})) se(par)) 
	starlevel(\sigl .1 \sigh .05 \sigvh .01) 
	varlabels(
	lnW: "$\ln W$" bk: $\beta_k$ 
	delta: "$\delta \cdot 100$" delta_vocational: "$\delta_{\text{vocational}} \cdot 100$" delta_academic: "$\delta_{\text{academic}} \cdot 100$"
	alpha: $\alpha$	alpha_vocational: "$\alpha_{\text{vocational}}$" alpha_academic: "$\alpha_{\text{academic}}$"
	)
	order(
	lnW: bk: 
	delta: delta_vocational: delta_academic:
	alpha: alpha_vocational: alpha_academic:
	)
	transform(delta: 100*@ 100 delta_vocational: 100*@ 100 delta_academic: 100*@ 100)
	drop(
	xb_year*: xb_canton*: xb_sector*: xb_firm*: xb_subord*: xb_unemp*: xb_continent*: xb_permit*: xb_city: 
	xb_married: xb_separated: xb_dep: xb_lmat: xb_tenureT_frac:
	) 
	mlabels(none) collabels(none) eqlabels("",none) 
	prehead(
	"\tablefirsthead{\toprule[1.5pt]"
	"& \multicolumn{1}{c}{I} & \multicolumn{1}{c}{II} & \multicolumn{1}{c}{III} & \multicolumn{1}{c}{IV} \\" 
	"\midrule [1pt]}"
	"\tablehead{\multicolumn{5}{l}{Table~\ref{tab:res`sex'} (\emph{continued})} \\" 
	"\toprule [1.5pt]" 
	"& \multicolumn{1}{c}{I} & \multicolumn{1}{c}{II} & \multicolumn{1}{c}{III} & \multicolumn{1}{c}{IV} \\" 
	"\midrule [1pt]}"
	"\tabletail{\midrule[1pt] \multicolumn{5}{r}{(\emph{continued on next page})} \\}"
	"\tablelasttail{}" 
	"\begin{supertabular}[c]{ldddd}"
	)
	postfoot(
	"\midrule[1pt]" 
	"\multicolumn{5}{r}{(\emph{continued on next page})} \\ %PAGE BREAK" 
	"%\multicolumn{5}{r}{}\\"
	)
	style(tex) replace
;
*/
estout modelI_`sex' modelII_`sex' modelIII_`sex' modelIV_`sex' using ./tables/results_dep_`sex'_VvsA.tex, 
	cells(b(star fmt(%5.3f) vacant(\multicolumn{1}{c}{---})) se(par)) 
	starlevel(\sigl .1 \sigh .05 \sigvh .01) 
	varlabels(
	lnW: "$\ln W$" bk: $\beta_k$ 
	delta: "$\delta \cdot 100$" delta_vocational: "$\delta_{\text{vocational}} \cdot 100$" delta_academic: "$\delta_{\text{academic}} \cdot 100$"
	alpha: $\alpha$	alpha_vocational: "$\alpha_{\text{vocational}}$" alpha_academic: "$\alpha_{\text{academic}}$"
	xb_married: Married xb_separated: Separated xb_dep: "\# dependents" 
	xb_tenureT_frac: "Tenure (years)" xb_lmat: "Language" 
	xb_city: "City $\geq$ 100,000 inhabitants" 
	xb_unemp1: "1 unemployment spell" xb_unemp2: "2 unemployment spells" xb_unemp3: "3 unemployment spells" xb_unemp4: "4 or more unemployment spells"
	xb_subord0: "No subordinate" xb_subord11: "11-19 subordinates" xb_subord12: "20-49 subordinates" xb_subord13: "50-99 subordinates" xb_subord14: "100 or more subordinates"
	xb_firmsize11: "Firm size: 11-19" xb_firmsize12: "Firm size: 20-49" xb_firmsize13: "Firm size: 50-99" xb_firmsize14: "Firm size: 100 or more"
	xb_permit1: "Foreigners' permit: settlement (C)" xb_permit2: "Foreigners' permit: residence (B)"
	xb_continent2: "Origin: EU25 ($-$ EU15)" xb_continent3: "Origin: Europe ($-$ EU25)" xb_continent4: "Origin: Africa" 
	xb_continent5: "Origin: North America" xb_continent6: "Origin: South America" xb_continent7: "Origin: Asia" xb_continent8: "Origin: Australia"
	)
	order(
	lnW: bk: 
	delta: delta_vocational: delta_academic:
	alpha: alpha_vocational: alpha_academic:
	xb_married: xb_separated: xb_dep: xb_tenureT_frac:
	xb_lmat: xb_city: 
	xb_unemp1: xb_unemp2: xb_unemp3: xb_unemp4: 
	xb_subord0: xb_subord11: xb_subord12: xb_subord13: xb_subord14: 
	xb_firmsize11: xb_firmsize12: xb_firmsize13: xb_firmsize14:
	xb_permit1: xb_permit2: 
	xb_continent2: xb_continent3: xb_continent4: 
	xb_continent5: xb_continent6: xb_continent7: xb_continent8:
	)
	transform(delta: 100*@ 100 delta_vocational: 100*@ 100 delta_academic: 100*@ 100)
	drop(xb_year*: xb_canton*: xb_sector*:)
	mlabels(none) collabels(none) eqlabels("",none) 
	prefoot(
	"Year dummies & \multicolumn{1}{c}{yes} & \multicolumn{1}{c}{yes} & \multicolumn{1}{c}{yes} & \multicolumn{1}{c}{yes} \\" 
	"Canton dummies & \multicolumn{1}{c}{yes} & \multicolumn{1}{c}{yes} & \multicolumn{1}{c}{yes} & \multicolumn{1}{c}{yes} \\"
	"Sector dummies & \multicolumn{1}{c}{yes} & \multicolumn{1}{c}{yes} & \multicolumn{1}{c}{yes} & \multicolumn{1}{c}{yes} \\" 
	"\midrule[1pt]"
	)
	stats(
		N N_clust r2_a ll aic bic delta_diff alpha_diff, 
		labels(
		"\# Obs" "\# Ind" "Adj.\ R$^2$" "LogL" "AIC" "BIC" 
		"F-stat for $\delta_{\text{voc}} = \delta_{\text{ac}}$" "F-stat for $\alpha_{\text{voc}} = \alpha_{\text{ac}}$"
		) 
		fmt(0 0 3 0 0 0 3 3) 
		layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" @ "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" @ @)
	)
		prehead(
	"\tablefirsthead{\toprule[1.5pt]"
	"& \multicolumn{1}{c}{I} & \multicolumn{1}{c}{II} & \multicolumn{1}{c}{III} & \multicolumn{1}{c}{IV} \\" 
	"\midrule [1pt]}"
	"\tablehead{\multicolumn{5}{l}{Table~\ref{tab:res`sex'} (\emph{continued})} \\" 
	"\toprule [1.5pt]" 
	"& \multicolumn{1}{c}{I} & \multicolumn{1}{c}{II} & \multicolumn{1}{c}{III} & \multicolumn{1}{c}{IV} \\" 
	"\midrule [1pt]}"
	"\tabletail{}" 
	"\tablelasttail{}" 
	"\begin{supertabular}[c]{ldddd}"
	)
	postfoot("\bottomrule[1.5pt]") 
	style(tex) replace
;
#d cr




***********************************************************
*Short version (wihtout covariates) of the results tables:*
***********************************************************

#d ;
estout modelI_`sex' modelII_`sex' modelIII_`sex' modelIV_`sex' using ./tables/results_dep_`sex'_VvsA_short.tex, 
	cells(b(star fmt(%5.3f) vacant(\multicolumn{1}{c}{---})) se(par)) 
	starlevel(\sigl .1 \sigh .05 \sigvh .01) 
	varlabels(
	lnW: "$\ln W$" bk: $\beta_k$ 
	delta: "$\delta \cdot 100$" delta_vocational: "$\delta_{\text{vocational}} \cdot 100$" delta_academic: "$\delta_{\text{academic}} \cdot 100$"
	alpha: $\alpha$	alpha_vocational: "$\alpha_{\text{vocational}}$" alpha_academic: "$\alpha_{\text{academic}}$"
	)
	order(
	lnW: bk: 
	delta: delta_vocational: delta_academic:
	alpha: alpha_vocational: alpha_academic:
	)
	transform(delta: 100*@ 100 delta_vocational: 100*@ 100 delta_academic: 100*@ 100)
	drop(
	xb_year*: xb_canton*: xb_sector*: xb_firm*: xb_subord*: xb_unemp*: xb_continent*: xb_permit*: xb_city: 
	xb_married: xb_separated: xb_dep: xb_lmat: xb_tenureT_frac:
	) 
	mlabels(none) collabels(none) eqlabels("",none) 
	prehead(
	"\toprule[1.5pt]"
	"& \multicolumn{1}{c}{I} & \multicolumn{1}{c}{II} & \multicolumn{1}{c}{III} & \multicolumn{1}{c}{IV} \\" 
	"\midrule [1pt]"
	)
	prefoot("\midrule[1pt]") 
	stats(
		N N_clust r2_a ll aic bic delta_diff alpha_diff, 
		labels(
		"\# Obs" "\# Ind" "Adj.\ R$^2$" "LogL" "AIC" "BIC" 
		"F-stat for $\delta_{\text{voc}} = \delta_{\text{ac}}$" "F-stat for $\alpha_{\text{voc}} = \alpha_{\text{ac}}$"
		) 
		fmt(0 0 3 0 0 0 3 3) 
		layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" @ "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" @ @)
	)
	postfoot("\bottomrule[1.5pt]") 
	style(tex) replace
;

#d cr


*************************************
* Sortie des tables en format .rtf: *
*************************************

*FORMAT COMPLET

#d ;
esttab modelI_`sex' modelII_`sex' modelIII_`sex' modelIV_`sex' using ./tables/results_dep_`sex'_VvsA.rtf, 
	cells(b(star fmt(%5.3f) vacant(--)) se(par)) 
	starlevel(* .1 ** .05 *** .01) 
	varlabels(
	lnW: "lnW" bk: "b_k"
	delta: "d x 100" delta_vocational: "d_vocational x 100" delta_academic: "d_academic x 100"
	alpha: "a"	alpha_vocational: "a_vocational" alpha_academic: "a_academic"
	xb_married: Married xb_separated: Separated xb_dep: "# dependents" 
	xb_tenureT_frac: "Tenure (years)" xb_lmat: "Language" 
	xb_city: "City > 100,000 inhabitants" 
	xb_unemp1: "1 unemployment spell" xb_unemp2: "2 unemployment spells" xb_unemp3: "3 unemployment spells" xb_unemp4: "4 or more unemployment spells"
	xb_subord0: "No subordinate" xb_subord11: "11-19 subordinates" xb_subord12: "20-49 subordinates" xb_subord13: "50-99 subordinates" xb_subord14: "100 or more subordinates"
	xb_firmsize11: "Firm size: 11-19" xb_firmsize12: "Firm size: 20-49" xb_firmsize13: "Firm size: 50-99" xb_firmsize14: "Firm size: 100 or more"
	xb_permit1: "Foreigners' permit: settlement (C)" xb_permit2: "Foreigners' permit: residence (B)"
	xb_continent2: "Origin: EU25 (- EU15)" xb_continent3: "Origin: Europe ($-$ EU25)" xb_continent4: "Origin: Africa" 
	xb_continent5: "Origin: North America" xb_continent6: "Origin: South America" xb_continent7: "Origin: Asia" xb_continent8: "Origin: Australia"
	)
	order(
	lnW: bk: 
	delta: delta_vocational: delta_academic:
	alpha: alpha_vocational: alpha_academic:
	xb_married: xb_separated: xb_dep: xb_tenureT_frac:
	xb_lmat: xb_city: 
	xb_unemp1: xb_unemp2: xb_unemp3: xb_unemp4: 
	xb_subord0: xb_subord11: xb_subord12: xb_subord13: xb_subord14: 
	xb_firmsize11: xb_firmsize12: xb_firmsize13: xb_firmsize14:
	xb_permit1: xb_permit2: 
	xb_continent2: xb_continent3: xb_continent4: 
	xb_continent5: xb_continent6: xb_continent7: xb_continent8:
	)
	transform(delta: 100*@ 100 delta_vocational: 100*@ 100 delta_academic: 100*@ 100)
	drop(xb_year*: xb_canton*: xb_sector*:)
	mlabels(none) collabels(none) eqlabels("",none) 
	prefoot(
	"Year dummies" 
	"Canton dummies"
	"Sector dummies" 
	)
	stats(
		N N_clust r2_a ll aic bic delta_diff alpha_diff, 
		labels(
		"# Obs" "# Ind" "Adj. R^2" "LogL" "AIC" "BIC" 
		"F-stat for d_voc = d_ac" "F-stat for a_voc = a_ac"
		) 
		fmt(0 0 3 0 0 0 3 3) 
	)
	replace
;
#d cr





*FORMAT COURT

#d ;
esttab modelI_`sex' modelII_`sex' modelIII_`sex' modelIV_`sex' using ./tables/results_dep_`sex'_VvsA_short.rtf, 
	cells(b(star fmt(%5.3f) vacant(--)) se(par)) 
	starlevel(* .1 ** .05 *** .01) 
	varlabels(
	lnW: "lnW" bk: "b_k"
	delta: "d x 100" delta_vocational: "d_vocational x 100" delta_academic: "d_academic x 100"
	alpha: "a"	alpha_vocational: "a_vocational" alpha_academic: "a_academic"
	)
	order(
	lnW: bk: 
	delta: delta_vocational: delta_academic:
	alpha: alpha_vocational: alpha_academic:
	)
	transform(delta: 100*@ 100 delta_vocational: 100*@ 100 delta_academic: 100*@ 100)
	drop(
	xb_year*: xb_canton*: xb_sector*: xb_firm*: xb_subord*: xb_unemp*: xb_continent*: xb_permit*: xb_city: 
	xb_married: xb_separated: xb_dep: xb_lmat: xb_tenureT_frac:
	) 
	mlabels(none) collabels(none) eqlabels("",none) 
	/*
		prehead(
		"\toprule[1.5pt]"
		"& \multicolumn{1}{c}{I} & \multicolumn{1}{c}{II} & \multicolumn{1}{c}{III} & \multicolumn{1}{c}{IV} \\" 
		"\midrule [1pt]"
		)
	prefoot("\midrule[1pt]") 
	*/
	stats(
		N N_clust r2_a ll aic bic delta_diff alpha_diff, 
		labels(
		"# Obs" "# Ind" "Adj. R^2" "LogL" "AIC" "BIC" 
		"F-stat for d_voc = d_ac" "F-stat for a_voc = a_ac"
		) 
		fmt(0 0 3 0 0 0 3 3) 
		/*layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" @ "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" @ @)*/
	)
	/*postfoot("\bottomrule[1.5pt]")*/
	nogaps nolines noeqlines
	replace
;
#d cr


}

exit