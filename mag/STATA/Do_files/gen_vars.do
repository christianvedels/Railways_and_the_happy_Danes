*** Generate variables

* Market town 1850
gen distance_market_town_1850_pre = distance_market_town if year == 1850
bysort gis_id: egen distance_market_town_1850 = max(distance_market_town_1850_pre)
drop distance_market_town_1850_pre


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
