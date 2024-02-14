**** Globals for estout tables

** Descriptives:
* estpost options
global post_opt statistics(mean p50 sd) c(stat)
global post_opt_full statistics(mean sd p25 p50 p75 min max) c(stat)

* esttab options  
global des_opt label replace cells("mean(fmt(%12.2fc )) sd(fmt(%12.2fc ))") nonum collabels("Mean"  "Std.dev.") booktabs alignment(rrrrr)
global des_opt_full label replace cells("mean(fmt(2)) sd(fmt(2))  min(fmt(2)) p25(fmt(2)) p50(fmt(2)) p75(fmt(2)) max(fmt(2))") nonum collabels("Mean" "Std.dev." "Min" "p25" "p50" "p75" "Max") booktabs alignment(rrrrrrr)

** Regressions
global estout_reg_opt replace se r2 booktabs label compress obslast noconstant b(%12.3f) nogap 
