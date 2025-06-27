#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
globvar <- c(
  "Label", "age", "alt_text", "area", "as_chunk", "caption", "comma", "dat", "end_year",
  "estimate", "estimate_lower", "estimate_upper", "estimate_y", "expected", "fleet",
  "growth_pattern", "label", "max_est", "max_est_yr", "module_name", "predicted",
  "figures_tables_dir", "recruitment", "sex", "spawning_biomass", "topic_label", "total_catch",
  "type", "uncertainty", "uncertainty_label", "val", "where", "year",
  ":=", "Year", "avg", "estimate_orig", "interm", "season", "tables_dir", "time", "total_fish",
  "years_per_year"
)
if (getRversion() >= "2.15.1") utils::globalVariables(globvar)
