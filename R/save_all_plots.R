#' Export all figures and tables
#'
#' Export all figures and tables to Rda files within one function.
#'
#' @inheritParams plot_spawning_biomass
#'
#' @param plot_recruitment_options List. List object of parameter values to pass 
#' directly to the `plot_recruitment` function. All default parameters, except 
#' for `dat`, of this function can be overriden with this list object. 
#' 
#' Default: list(module = "TIME_SERIES")
#'
#' @param plot_natural_mortality_options List. List object of parameter values to
#' pass directly to the `plot_natural_mortality` function. All default parameters, except 
#' for `dat`, of this function can be overriden with this list object.
#' 
#' Default: list(module = "Natural_Mortality")
#' 
#' @param table_total_catch_options List. List object of parameter values to pass
#' directly to the `table_total_catch` funciton. All default parameters, except 
#' for `dat`, of this function can be overriden with this list object.
#' 
#' Default: list(module = "TIME_SERIES")
#' 
#' ... Additional list objects of parameter values to pass directly to corresponding
#' plot or table functions. Parameter names should follow the naming convention
#' `{function_name}_options`. All default parameters, except for `dat`, of this function 
#' can be overriden with this list object.
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
#'   plot_spawning_biomass_options = list(
#'     ref_line = "unfished"
#'   ),
#'   plot_abundance_at_age_options = list(
#'     scale_amount = 1,
#'     proportional = FALSE
#'   )
#' )
#' }
save_all_plots <- function(
  dat,
  interactive = FALSE,
  figures_tables_dir = getwd(),
  # These three functions require having default parameters because of
  # no standard default modules
  plot_recruitment_options = list( module = "TIME_SERIES"),
  plot_natural_mortality_options = list(module = "Natural_Mortality"),
  table_total_catch_options = list( module = "TIME_SERIES"),
  # All other functions can be modified with appropiate *_options
  # parameter lists.
  ...
) {

  if(!is.data.frame(dat) || !all(c("label", "estimate") %in% colnames(dat))){
    stop("dat is not the appropriate format. Have you run `convert_output()`?")
  }

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


  # TODO: should change this so that the plot/table functions are pulled directly
  # from the stockplotr NAMESPACE, rather than based on the existecnce of an R
  # file, so that multiple plotting functions can live in the same file.
  all_R_files <- list.files(
      file.path(here::here(), "R"),
  )
  plottab_function_files <- all_R_files[grepl("plot_|table_", all_R_files)]
  # Remove table_afsc_tier function for now
  plottab_function_files <- plottab_function_files[!grepl("afsc", plottab_function_files)]
  
  plottab_functions <- sub("\\.[^\\.]*$", "", plottab_function_files)

  for(i in seq_along(plottab_functions)){
      func_name <- plottab_functions[i]
      func_specific_args <- all_params[paste0(func_name,"_options")][[1]]

      func_pars <- default_args
      if(!is.null(func_specific_args))
        func_pars <- c(default_args, func_specific_args)

      # switch from "figures_dir" to "tables_dir" dynamically
      if(grepl("table",func_name)){
        names(func_pars)[which(names(func_pars)=="figures_dir")] <- "tables_dir"
      }

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
            for(a in seq_along(func_specific_args)){
              arg_name = names(func_specific_args)[a]
              arg_value = as.character(func_specific_args[a])
              cli::cli_li("{arg_name} = {arg_value}")
            }
            print(e)
        }
      )   
  }
}
