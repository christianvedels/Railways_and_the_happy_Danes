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
	
	* If .5, 1.5 or 2.5 
	if round(`x') != `x' {
		local y = `x'-`i_min'
		gen rail_`y'_5 = (`rail_var' < `x')&(`rail_var'>= (`x'-`i_int'))
		local start_val = `x'-`i_int'
		label var rail_`y'_5 "`start_val'-`x' km"
		local rail_inds `rail_inds' rail_`y'_5  
		local xnames `xnames' rail_`y'_5="`start_val'-`x' km"
	}
	
}

eststo reg1: xi: reg `dep_var' `rail_inds' i.county, vce(cl county)
coefplot reg1, drop(_cons _Icounty*) vertical coeflabels(`xnames', angle(45)) yline(0) cirecast(rcap) xtitle("Trunk line indicator distance") ytitle("Coefficient and 95 confidence interval") note("With County FE" "Dep var:`dep_var'")
graph export `fe_path', replace

eststo reg2: xi: reg `dep_var' `rail_inds' i.county $expl_vars, vce(cl county)
coefplot reg2, keep(`rail_inds') vertical coeflabels(`xnames', angle(45)) yline(0) cirecast(rcap) xtitle("Trunk line indicator distance") ytitle("Coefficient and 95 confidence interval") note("With County FE and explanatory variables" "Dep var:`dep_var'")
graph export `feexp_path',replace
