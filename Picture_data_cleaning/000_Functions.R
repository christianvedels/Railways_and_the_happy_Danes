# Functions
#
# Date updated:   2024-04-07
# Auhtor:         Christian Vedel 
# Purpose:        Functions used for this part of the project

# ==== sub_scandi ====
# this substitutes scandinavian letters
sub_scandi = function(x){
  scandi_letters = c("Æ",
                     "æ",
                     "Ø",
                     "ø",
                     "Å",
                     "å")
  
  replacement = c("Ae",
                  "ae",
                  "Oe",
                  "oe",
                  "Aa",
                  "aa")
  
  for(i in 1:length(scandi_letters)){
    x = gsub(
      scandi_letters[i],
      replacement[i],
      x
    )
  }
  
  return(x)
  
}