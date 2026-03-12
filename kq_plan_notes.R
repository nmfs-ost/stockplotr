# setup
# 
# 1. create csv with cols "key_quantity", "value", "meaning", "calc dependent on other key quantity", "dependent key quantity", "calc dependent on user", "function with required input"- TEMPLATE
# 2. KQs calc'd on a fxn-by-fxn basis, and csv is updated each time unless non-NA value present
# 3. If KQ depends on other for calc, import that cell in the csv; if not NA, convert to number and calculate it. If NA, send message to console: user can add it manually OR run fxn that depends on it (see appropriate column)


unit_label = "metric tons"
geom = "line"
group = NULL
facet = NULL
era = NULL
scale_amount = 1
module = NULL
interactive = TRUE
make_rda = FALSE
figures_dir = getwd()

