*** MAIN **** 
clear all
pause on 

cd "/Users/MagnusOrberg/Documents/GitHub/Railways_and_the_happy_Danes/mag"


*******************************************************************************
***************************** Load and edit data ******************************
*******************************************************************************
** Load Grundtvigianism data and save rail 1875:

import delimited "/Users/MagnusOrberg/Documents/GitHub/Railways_and_the_happy_Danes/Data/REGRESSION_DATA_Grundtvigianism.csv", parselocale(da_DK) clear 

keep if year == 1875 

** Convert exponented numbers to numeric:
foreach var in distance_to_nearest_railway distance_to_nearest_railway_stat v8 distance_to_nearest_railway_priv distance_to_nearest_railway_inst  {
	destring `var', replace dpcomma force
} 

gen dist_rail_1875 = distance_to_nearest_railway 
gen dist_rail_1875_trunk = distance_to_nearest_railway_stat 
gen dist_rail_1875_othstate = v8 
gen dist_rail_1875_priv = distance_to_nearest_railway_priv
gen dist_rail_1875_inst = distance_to_nearest_railway_inst

keep gis_id dist_rail_1875 dist_rail_1875_trunk dist_rail_1875_othstate dist_rail_1875_priv dist_rail_1875_inst

/* OBS! 11 duplicates */
duplicates drop 

save "STATA/Data/rail1875_from_grundtvig.dta", replace 


** Load demography data:
import delimited "/Users/MagnusOrberg/Documents/GitHub/Railways_and_the_happy_Danes/Data/REGRESSION_DATA_Demography.csv", parselocale(da_DK) clear 

** Convert exponented numbers to numeric:
foreach var in distance_to_nearest_railway distance_to_nearest_railway_inst boulder_clay_pct distoxroad age_mean child_women_ratio hisclass_avg  {
	destring `var', replace dpcomma force
} 

duplicates drop 
/* OBS! 18 dublicates */ 
sort gis_id year

* Merge on rail 1875 from above
/* OBS! all matched from demograhy however 259 not matched from grundtvig */
merge m:1 gis_id using "STATA/Data/rail1875_from_grundtvig.dta"



* Rename
rename distance_to_nearest_railway dist_rail

** Generate variables 
* Rail_var
local rail_var dist_rail

* Rail var years 
local rail_years 1860 1880 

* Rail var indicator distances
local rail_ind_dists 2 

* First Differences Variables
local fd_vars pop

* Explanatory Variables to take log of: 
local log_vars area_parish dist_coast distance_market_town distoxroad dist_hmb dist_cph

include "STATA/Do_files/gen_vars.do"
/* 
Notes to gen vars: 
OBS! gis_id A114160 is missing dist ox_road
*/

** Only needing 1 obs per parish
keep if year == 1850

*** Samples
gen non_prox_mt_sample = distance_market_town_1850 > 1


*******************************************************************************
***************************** Globals *****************************************
*******************************************************************************

** Globals for estout (tables)
include "STATA/Do_files/Globals/estout_globals.do"

** Explanatory variables
global expl_vars log_area_parish boulder_clay_pct log_dist_coast log_distance_market_town log_distoxroad log_dist_hmb log_dist_cph   


/* 
Missing variables ()
	- Rail type (State trunk, state non-trunk, private)
*/ 

/*
*******************************************************************************
***************************** Descriptives ************************************
*******************************************************************************

** Full descriptives 
local des_vars 
 
local path 
include "STATA/Do_files/Descriptives/des_full.do"


** By rail indicator 
local des_vars 
local rail_indicator

local path
include "STATA/Do_files/Descriptives/des_by_rail_indicator.do"
*/




*******************************************************************************
********************************* Regressions *********************************
*******************************************************************************

******************************* Bin-regressions *******************************
set graphics off

* Variables:
local dep_var d_log_pop
local rail_var dist_rail_1880

* Interval settings in km 
local i_min = 2
local i_int = 2 
local i_max = 12

* Graph path settings
local fe_path "STATA/Figures/population/bin_regs/bin_reg_`i_min'_`i_int'_`i_max'_fe.png"
local feexp_path "STATA/Figures/population/bin_regs/bin_reg_`i_min'_`i_int'_`i_max'_feexp.png" 


preserve
keep if non_prox_mt_sample == 1 
include "STATA/Do_files/Regressions/bin_reg.do"
restore


*************************** Standard regressions ******************************
* Variables:
local dep_var d_log_pop
local rail_var_ind  dist_rail_1880_i2
local rail_var_log log_dist_rail_1880

local dep_var_name "$\Delta$ Population 1850-1901"
local ols_path "STATA/Tables/Regressions/Population/OLS_2km" 

preserve
keep if non_prox_mt_sample == 1 
include "STATA/Do_files/Regressions/ols_main.do"
restore






