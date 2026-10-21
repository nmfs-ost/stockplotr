#' Export all figures and tables
#'
#' Export all figures and tables to Rda files within one function.
#'
#' @inheritParams plot_spawning_biomass
#' @param recruitment_scale_amount Number. A number describing how much to scale down
#' the recruitment quantities shown on the y axis. For example,
#' recruitment_scale_amount = 100 would scale down a value from 500,000 -->
#' 5,000. This scale will be reflected in the y axis label.
#'
#' Default: 1
#'
#' @param recruitment_unit_label String. Recruitment units
#'
#' Default: "mt"
#'
#' @param figures_tables_dir Path. The location of the folder containing
#' figures and tables ("figures" and "tables").
#'
#' Default: the working directory (`getwd()`)
#'
#' @param ref_line String. Type of reference point to
#' compare biomass to. The default is `"msy"`, which looks for
#' `"biomass_msy"` in the `"label"` column of `dat`. The actual
#' searching in `dat` is case agnostic and will work with either upper- or
#' lower-case letters but you must use one of the options specified in the
#' default list to ensure that the label on the figure looks correct
#' regardless of how it is specified in `dat`. Other possibilities may include
#' "target", "MSY", and "unfished". When the reference cannot be found,
#' indicate the reference line in the form c("label" = value).
#'
#' Default: "msy"
#'
#' Options: Including, but not limited to: "target", "MSY", "unfished"
#'
#' @param biomass_scale_amount Number. A number describing how much to scale down the
#' biomass quantities shown on the y axis. See `recruitment_scale_amount`.
#'
#' Default: 1
#'
#' @param landings_unit_label String. Landings units
#'
#' Default: "mt"
#'
#' @param biomass_unit_label String. Biomass units
#'
#' Default: "mt"
#'
#' @param spawning_biomass_label String. Spawning biomass units
#'
#' Default: "mt"
#'
#' @param spawning_biomass_scale_amount Number.  A number describing how much to scale down the
#' spawning biomass quantities shown on the y axis. See `recruitment_scale_amount`.
#'
#' Default: 1
#'
#' @param ref_line_sb String. Same as `ref_line`, but this argument is
#' applied to plot_spawning_biomass.
#'
#' Default: "msy"
#'
#' Options: Including, but not limited to: "target", "MSY", "unfished"
#'
#' @param abundance_at_age_scale_amount Number.  A number describing how much to scale down the
#' abundance quantities shown via bubble size. See `recruitment_scale_amount`.
#'
#' Default: 1
#'
#' @param abundance_at_age_unit_label String. Abbreviated abundance at age units
#'
#' Default: "fish"
#'
#' @param biomass_at_age_scale_amount Number.  A number describing how much to scale down the
#' biomass quantities shown via bubble size. See `recruitment_scale_amount`.
#'
#' Default: 1
#'
#' @param biomass_at_age_unit_label String. Abbreviated biomass at age units
#'
#' Default: "mt"
#'
#' @param index_unit_label String. Index of abundance/CPUE units
#'
#' Default: ""
#'
#' @param bnc_unit_label String. Abbreviated biomass, abundance, and catch units
#'
#' Default: c("biomass" = "mt", "abundance" = "fish", "catch" = "mt")
#'
#' @param catch_unit_label String. Abbreviated catch units
#'
#' Default: "mt"
#'
#' @param catch_scale_amount Number. A number describing how much to scale down the
#' catch quantities shown via bubble size. See `recruitment_scale_amount`.
#'
#' Default: 1
#'
#' @param landings_unit_label String. Landings units
#'
#' Default: "mt"
#'
#' @param projections_unit_label Units for projections quantities
#'
#' Default: c("catch" = "mt", "spawning_biomass" = "mt",
#' "fishing_mortality" = "")
#'
#' @param projections_uncert_label Abbreviated uncertainty label for
#' each quantity
#'
#' Default: c("catch" = "stddev", "spawning_biomass" = "stddev",
#' "fishing_mortality" = "stddev")
#'
#' @param proportional Logical. TRUE/FALSE; scale size of bubble plots
#'
#' Default: `TRUE`
#'
#' @returns Rda files for each figure/table.
#'
#' @seealso [convert_output()], [create_rda()]
#'
#' @details The input is from an assessment model output file
#' translated to a standardized output (\link[stockplotr]{convert_output}).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' save_all_plots(dat,
#'   ref_line = "unfished",
#'   ref_line_sb = "target",
#'   index_unit_label = "CPUE",
#'   biomass_at_age_scale_amount = 1,
#'   biomass_at_age_unit_label = "metric tons"
#' )
#' }
save_all_plots <- function(
  # imported from plot_recruitment
  dat,
  plot_recruitment_options =list(
    module = "TIME_SERIES"
  ),
  plot_natural_mortality_options = list(
    module = "Natural_Mortality"
  ),
  table_biomass_abundance_catch_options = list(
    unit_label = c("biomass" = "mt", "abundance" = "fish", "catch" = "mt") 
  ),
  table_projections_options = list(
    unit_label = c("catch" = "mt", "spawning_biomass" = "mt", "fishing_mortality" = "")
  ),
  table_total_catch_options = list(
    module = "TIME_SERIES"
  ),
  interactive = FALSE,
  figures_tables_dir = getwd(),
  ...
) {

  all_params <- as.list(environment(), all=TRUE)
  make_rda <- TRUE
  
  cli::cli_h1("Starting export of figures and tables")

  # These arguments are used by every plot and table
  default_args <- list(
    dat = dat,
    interactive = interactive,
    make_rda = TRUE,
    figures_dir = figures_tables_dir
  )

  all_R_files <- list.files(
      file.path("R"),
  )
  plottab_function_files <- all_R_files[grepl("plot_|table_", all_R_files)]
  plottab_function_files <- plottab_function_files[!grepl("afsc", plottab_function_files)]
  plottab_functions <- sub("\\.[^\\.]*$", "", plottab_function_files)

  for(i in seq_along(plottab_functions)){
      func_name <- plottab_functions[i]
      # print(all_params)
      func_specific_args <- all_params[paste0(func_name,"_options")][[1]]

      func_pars <- default_args
      if(!is.null(func_specific_args))
        func_pars <- c(default_args, func_specific_args)

      if(grepl("table",func_name)){
        names(func_pars)[which(names(func_pars)=="figures_dir")] <- "tables_dir"
      }
      # print(func_pars)

      cli::cli_alert_info("Attempting to execute function: {func_name}")
      tryCatch(
        {
          suppressMessages(
              do.call(func_name, func_pars) # Execute plotting/table function
          )
        },
        error = function(e) {
            cli::cli_alert_danger("{func_name} failed to run.")
            cli::cli_alert("Tip: check that your arguments are correct.")
            for(a in func_specific_args){
              arg_name = names(func_specific_args)[a]
              arg_value = as.character(func_specific_args[a])
              cli::cli_li("{arg_name} = {arg_value}")
            }
            print(e)
        }
      )   
  }
}

save_all_plots(dat=example_data)
