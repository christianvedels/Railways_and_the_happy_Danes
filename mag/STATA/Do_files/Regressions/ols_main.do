
********************************************************************************
************************** OLS regressions (main) ******************************
********************************************************************************



**** OLS with indicator and ln distance (MAIN TABLES)


eststo clear
foreach x of var `rail_var_ind' `rail_var_log' {
	/* * Simple regression
	eststo: quietly regress dlnPop `x', robust */
	
	* With county FE
	eststo: xi: regress `dep_var' `x' i.county, nocons vce(cl county)
	
	* With additional controls
	eststo: xi: regress `dep_var' `x' i.county /// 
								$expl_vars , nocons vce(cl county)
										
	/** With later rail:
	eststo: xi: quietly regress dlnPop `x' i.county /// 
								$expl_vars_pref1 $other_rail_vars, ///
								vce(cl county)*/
}

* Output
esttab using `ols_path', $estout_reg_opt indicate("County FE = _Icounty*" "Additional controls = $expl_vars") order($`rail_var_ind' `rail_var_log' /*$other_rail_vars*/) mgroups("`dep_var_name'", pattern(1 0 0 0 0 0)prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) nomti 

