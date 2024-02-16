*** First stage:
eststo clear

* With County FE 
eststo iv1: xi :quietly ivreg2 `dep_var' (`rail_var' = `instrument') ///
 i.county, cluster(county)
scalar F_first = e(widstat)
eststo fs1: xi: quietly reg `rail_var' `instrument' ///
i.county, vce(cl county)
estadd scalar F_kp = F_first

* With controls 
eststo iv2: xi :quietly ivreg2 `dep_var' (`rail_var' = `instrument') ///
 i.county $expl_vars , cluster(county)
scalar F_first = e(widstat)
eststo fs2: xi: reg `rail_var' `instrument' $expl_vars ///
i.county, vce(cl county)
estadd scalar F_kp = F_first

* With later rail:
eststo iv3: xi:quietly ivreg2 `dep_var' (`rail_var' = `instrument') ///
 i.county $expl_vars `other_rail' , cluster(county)
scalar F_first = e(widstat)
eststo fs3 : xi: quietly reg `rail_var' `instrument' $expl_vars `other_rail' ///
i.county, vce(cl county)
estadd scalar F_kp = F_first


local rail_var_label: var label `rail_var'

esttab fs1 fs2 fs3 using `fs_path', $estout_reg_opt indicate("County FE = _Icounty*" "Additional controls = $expl_vars") order(`instrument') nomti stats(F_kp N, labels("Kleibergeren-Paap F-statistic" "Observations") fmt(2 0)) mgroups("`rail_var_name'", pattern(1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
