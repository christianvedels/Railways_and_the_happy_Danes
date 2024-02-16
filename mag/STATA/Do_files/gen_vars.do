*** Generate variables

* Market town 1850
gen distance_market_town_1850_pre = distance_market_town if year == 1850
bysort gis_id: egen distance_market_town_1850 = max(distance_market_town_1850_pre)
drop distance_market_town_1850_pre


* Rail distance indicators
foreach rail_var of loc rail_vars {
	foreach dist of loc rail_ind_dists {
		gen `rail_var'_i_`dist' = `rail_var' <= `dist'
	}
}

** Log of variables
foreach var of loc log_vars {
	gen log_`var' = ln(`var')
}

** First differences (1850-1901)
foreach var of loc fd_vars {
	gen `var'_1850_pre = `var' if year == 1850
	bysort gis_id: egen `var'_1850 = max(`var'_1850_pre)
	gen `var'_1901_pre = `var' if year == 1901
	bysort gis_id: egen `var'_1901 = max(`var'_1901_pre)
	drop `var'_1850_pre `var'_1901_pre  
	
	gen log_`var'_1850 = ln(`var'_1850)
	gen log_`var'_1901 = ln(`var'_1901)
	
	gen d_`var' = `var'_1901 - `var'_1850
	gen d_log_`var' = log_`var'_1901 - log_`var'_1850
}

** Other rail 1875-1901 
gen dist_rail_1901_pre = distance_to_nearest_railway if year == 1901
bysort gis_id: egen dist_rail_1901 = max(dist_rail_1901_pre)
drop dist_rail_1901_pre


foreach dist of loc rail_ind_dists {
	gen dist_rail_1901_i_`dist' = dist_rail_1901 <= `dist'
	gen dist_rail_1875_1901_i_`dist' = 0
	replace dist_rail_1875_1901_i_`dist' = 1 if dist_rail_1901_i_`dist' == 1 & dist_rail_1875_i_`dist' == 0
}

gen connected_1901_pre = connected_rail if year == 1901
bysort gis_id: egen connected_1901 = max(connected_1901_pre)
drop connected_1901_pre

gen connected_1875_1901 = 0
replace connected_1875_1901 = 1 if connected_1875 == 0 & connected_1901 == 1 

gen log_dist_rail_1901 = ln(dist_rail_1901)



* OLD:

/*
** Rail variables
* log distance:
gen log_`rail_var' = ln(`rail_var')

foreach y of loc rail_years {
	*y rail
	gen `rail_var'_`y'_pre = `rail_var' if year == `y'
	bysort gis_id: egen `rail_var'_`y' = max(`rail_var'_`y'_pre)
	drop `rail_var'_`y'_pre

	gen log_`rail_var'_`y' = ln(`rail_var'_`y')

	* y indicator
	foreach x of local rail_ind_dists {
		gen `rail_var'_`y'_i`x' = `rail_var'_`y' <= `x' 
	}

}
*/
