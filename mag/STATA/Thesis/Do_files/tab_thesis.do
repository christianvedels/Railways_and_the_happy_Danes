******* Main do- file 
clear all
cd "/Users/MagnusOrberg/Library/CloudStorage/GoogleDrive-magnusorberg97@gmail.com/Mit drev/Skole/Speciale/Data_editing/STATA"

use "Data/merged/merged_data_01.dta", clear

gen ind_SL_2km = (dist_SL_center <= 2000)
gen rail_ind2_c = (dist_trunk_center <= 2000)
gen pr_rail_ind2_c = (dist_priv_center <= 2000)
gen st_rail_ind2_c = (dist_opub_center <= 2000)

eststo tab1: estpost tab AMT rail_ind2_c
esttab tab1 using "/Users/MagnusOrberg/Documents/GitHub/Railways_and_the_happy_Danes/mag/STATA/Thesis/Tables/county_2km.tex" , cell(b rowpct) unstack label replace booktabs nonum


