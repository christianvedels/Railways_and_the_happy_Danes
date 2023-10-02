# Regression data
#
# Date updated:   2023-10-02
# Auhtor:         Christian Vedel 
# Purpose:        Collects data 

# ==== Libraries ====

# ==== Load data ====
railways = read_csv2("Data/Panel_of_railways_in_parishes.csv")

Assembly_houses = read_csv2("Data/Panel_of_assembly_houses.csv")
Assembly_houses_MA = read_csv2("Data/Panel_of_MA_assembly_houses.csv")

Folk_high_schools = read_csv2("Data/Panel_of_folk_high_schools.csv")
Folk_high_schools_MA = read_csv2("Data/Panel_of_MA_folk_high_schools.csv")

census = read_csv2("Data/Census_data.csv")  