# Functions
#
# Date updated:   2023-09-15
# Auhtor:         Christian Vedel 
# Purpose:        Functions used in the rest of the project


# pretty_pct(x, digits = 4)
# Makes pretty pct string
pretty_pct = function(x, digits = 4){
  x = signif(x, digits = digits)
  x = x*100
  x = paste(x,"%",sep ="")
  return(x)
}