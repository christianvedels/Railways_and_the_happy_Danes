*** Descriptives by rail indicator
eststo clear
eststo des0: estpost tabstat `des_vars' if `rail_indicator' == 0, $post_opt
eststo des1: estpost tabstat `des_vars' if `rail_indicator' == 1, $post_opt
eststo desT: estpost tabstat `des_vars', $post_opt

esttab des0 des1 desT using `path', $des_opt mgroups("No trunk line access" "Trunk line access" "Total", pattern(1 1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) 

