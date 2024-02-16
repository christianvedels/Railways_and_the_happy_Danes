
********************************************************************************
************************** OLS regressions (main) ******************************
********************************************************************************



**** OLS with county-FE, controls and later rail


eststo clear
foreach x of var `rail_var' {
	/* * Simple regression
	eststo: quietly regress dlnPop `x', robust */
	
	* With county FE
	eststo: xi: regress `dep_var' `x' i.county, `constant' vce(cl county)
	
	* With additional controls
	eststo: xi: regress `dep_var' `x' i.county /// 
								$expl_vars , `constant' vce(cl county)
	
	* With later rail
	eststo: xi: quietly regress `dep_var' `x' i.county /// 
								$expl_vars `other_rail' ,`constant' ///
								vce(cl county)
}

* Output
esttab using `ols_path', $estout_reg_opt indicate("County FE = _Icounty*" "Additional controls = $expl_vars") order(`rail_var' `other_rail') mgroups("`dep_var_name'", pattern(1 0 0)prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) nomti 

