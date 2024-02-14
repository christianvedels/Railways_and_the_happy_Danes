*** Bin regressions: 
eststo clear


*** Create indicator variables of bins
foreach x of num `i_min'(`i_int')`i_max'{
	di `x'
	

	if round(`x') == `x' {
		gen rail_`x' = (`rail_var' < `x')&(`rail_var' >= (`x'-`i_int') )
		local start_val = `x'-`i_int'
		label var rail_`x' "`start_val'-`x' km"
		local rail_inds `rail_inds' rail_`x' 
		local xnames `xnames' rail_`x'="`start_val'-`x' km"
	}
	
	/* HAS TO BE EDITED. USE IF INTERVALS should be .5
	if round(`x') != `x' {
		local y = `x'-0.5
		gen rail_`y'_5 = ($ind_rail < `x'*1000)&($ind_rail >= (`x'-$i_int )*1000)
		local start_val = `x'-$i_int
		label var rail_`y'_5 "`start_val'-`x' km"
		local reg_vars `reg_vars' rail_`y'_5 
		local start_val = `x'-$i_int
		local xnames `xnames' rail_`y'_5="`start_val'-`x' km"
	}
	*/
}

eststo reg1: xi: reg `dep_var' `rail_inds' i.county, nocons vce(cl county)
coefplot reg1, drop(_cons _Icounty*) vertical coeflabels(`xnames', angle(45)) yline(0) cirecast(rcap) xtitle("Trunk line indicator distance") ytitle("Coefficient and 95 confidence interval") note("With County FE" "Dep var:`dep_var'")
graph export `fe_path', replace

eststo reg2: xi: reg `dep_var' `rail_inds' i.county $expl_vars, nocons vce(cl county)
coefplot reg2, keep(`rail_inds') vertical coeflabels(`xnames', angle(45)) yline(0) cirecast(rcap) xtitle("Trunk line indicator distance") ytitle("Coefficient and 95 confidence interval") note("With County FE and explanatory variables" "Dep var:`dep_var'")
graph export `feexp_path',replace

