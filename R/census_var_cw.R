# census_vars <- tibble::tribble(~variable_cat, ~variable_code, ~variable_label,
# 		"population", "P001001", "population",
# 		"race", "P006002", "white",
# 		"race", "P006003", "black",
# 		"race", "P006004", "aian",
# 		"race", "P006005", "asian",
# 		"race", "P006006",  "nhpi",
# 		"race", "P006007", "other",
# 		"kids", "P012003", "under5_male",
# 		"kids", "P012027", "under5_female")
#
# acs_vars <- tibble::tribble(~variable_cat, ~variable_code, ~variable_label,
# 		"kids", "B01001_003E", "under5_male",
# 		"kids", "B01001_027E", "under5_female")
#
# use_data(census_vars, acs_vars, internal = TRUE, overwrite = TRUE)
