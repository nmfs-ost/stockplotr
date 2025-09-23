#' Export all figures and tables
#'
#' Export all figures and tables to Rda files within one function.
#'
#' @inheritParams plot_spawning_biomass
#' @param recruitment_scale_amount A number describing how much to scale down
#' the recruitment quantities shown on the y axis. For example,
#' recruitment_scale_amount = 100 would scale down a value from 500,000 -->
#' 5,000. This scale will be reflected in the y axis label.
#' @param recruitment_unit_label Units for recruitment
#' @param figures_tables_dir The location of the folder containing
#' figures and tables ("figures" and "tables").
#' @param ref_line A string specifying the type of reference you want to
#' compare biomass to. The default is `"msy"`, which looks for
#' `"biomass_msy"` in the `"label"` column of `dat`. The actual
#' searching in `dat` is case agnostic and will work with either upper- or
#' lower-case letters but you must use one of the options specified in the
#' default list to ensure that the label on the figure looks correct
#' regardless of how it is specified in `dat`. Other possibilities may include
#' "target", "MSY", and "unfished". When the reference cannot be found, 
#' indicate the reference line in the form c("label" = value).
#' @param biomass_scale_amount A number describing how much to scale down the
#' biomass quantities shown on the y axis. See `recruitment_scale_amount`.
#' @param landings_unit_label Units for landings
#' @param biomass_unit_label Units for biomass
#' @param spawning_biomass_label Units for spawning biomass
#' @param spawning_biomass_scale_amount  A number describing how much to scale down the
#' spawning biomass quantities shown on the y axis. See `recruitment_scale_amount`.
#' @param ref_line_sb Identical definition as `ref_line`, but this argument is
#' applied to plot_spawning_biomass.
#' @param abundance_at_age_scale_amount  A number describing how much to scale down the
#' abundance quantities shown via bubble size. See `recruitment_scale_amount`.
#' @param abundance_at_age_unit_label Abbreviated units for abundance at age
#' @param biomass_at_age_scale_amount  A number describing how much to scale down the
#' biomass quantities shown via bubble size. See `recruitment_scale_amount`.
#' @param biomass_at_age_unit_label Abbreviated units for biomass at age
#' @param indices_unit_label Units for index of abundance/CPUE
#' @param biomass_unit_label Abbreviated units for biomass
#' @param catch_unit_label Abbreviated units for catch
#' @param catch_scale_amount A number describing how much to scale down the
#' catch quantities shown via bubble size. See `recruitment_scale_amount`.
<<<<<<< HEAD
=======
#' @param landings_unit_label Units for landings
>>>>>>> 6ca5b6c (update save_all_plots and work on tests)
#' @param proportional T/F to scale size of bubble plots
#'
#' @return Rda files for each figure/table.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' save_all_plots(dat,
#'   ref_line = "unfished",
#'   ref_line_sb = "target",
#'   indices_unit_label = "CPUE",
#'   biomass_at_age_scale_amount = 1,
#'   biomass_at_age_unit_label = "metric tons"
#' )
#' }
save_all_plots <- function(
    # imported from plot_recruitment
    dat,
    recruitment_unit_label = "mt", # changed from unit_label to recruitment_unit_label for specificity
    recruitment_scale_amount = 1,
    relative = FALSE,
    proportional = TRUE,
    interactive = FALSE,
    figures_tables_dir = getwd(),
    # imported from plot_biomass
    ref_line = "msy",
    biomass_scale_amount = 1,
    # imported from plot_landings
    # landings_unit_label = "mt",
    # imported from plot_recruitment_deviations- zero unique arguments
    # imported from plot_spawn_recruitment
    spawning_biomass_label = "mt",
    spawning_biomass_scale_amount = 1,
    # imported from plot_spawning_biomass
    ref_line_sb = "msy",
    # imported from plot_abundance_at_age
    abundance_at_age_scale_amount = 1,
    abundance_at_age_unit_label = "fish",
    # imported from plot_biomass_at_age
    biomass_at_age_scale_amount = 1,
    biomass_at_age_unit_label = "mt",
    # imported from plot_indices
    # indices_unit_label = "",
    # imported from table_afsc_tier- add potential unique arguments after dev
    # imported from table_bnc
    biomass_unit_label = "mt",
    catch_unit_label = "mt",
    catch_scale_amount = 1
    # imported from table_harvest_projection- add potential unique arguments after dev
    # imported from table_indices- zero unique arguments
    # imported from table_landings- zero unique arguments
    ) {
  make_rda <- TRUE

  cli::cli_h1("Starting export of figures and tables")

  # figures

  tryCatch(
    {
      cli::cli_h2("plot_recruitment")
      plot_recruitment(
        dat,
        unit_label = recruitment_unit_label,
        scale_amount = recruitment_scale_amount,
        relative = relative,
        interactive = interactive,
        make_rda = TRUE,
        figures_dir = figures_tables_dir
      ) #|>
      # suppressWarnings() |>
      # invisible()
    },
    error = function(e) {
      cli::cli_alert_danger("plot_recruitment failed to run.")
      cli::cli_alert("Tip: check that your arguments are correct.")
      cli::cli_li("recruitment_unit_label = {recruitment_unit_label}")
      cli::cli_li("recruitment_scale_amount = {recruitment_scale_amount}")
      cli::cli_li("relative = {relative}")
      print(e)
    }
  )


  tryCatch(
    {
      cli::cli_h2("plot_biomass")
      plot_biomass(
        dat,
        unit_label = biomass_unit_label,
        scale_amount = biomass_scale_amount,
        ref_line = ref_line,
        relative = relative,
        interactive = interactive,
        make_rda = TRUE,
        figures_dir = figures_tables_dir
      ) #|>
      # suppressWarnings() |>
      # invisible()
    },
    error = function(e) {
      cli::cli_alert_danger("plot_biomass failed to run.")
      cli::cli_alert("Tip: check that your arguments are correct.")
      cli::cli_li("biomass_unit_label = {biomass_unit_label}")
      cli::cli_li("biomass_scale_amount = {biomass_scale_amount}")
      cli::cli_li("ref_line = {ref_line}")
      cli::cli_li("relative = {relative}")
      print(e)
    }
  )


  # tryCatch(
  #   {
  #     cli::cli_h2("plot_landings")
  #     plot_landings(dat,
  #       unit_label = landings_unit_label,
  #       make_rda,
  #       figures_dir = figures_tables_dir
  #     ) # |>
  #     # suppressWarnings() |>
  #     # invisible()
  #   },
  #   error = function(e) {
  #     cli::cli_alert_danger("plot_landings failed to run.")
  #     cli::cli_alert("Tip: check that your arguments are correct.")
  #     cli::cli_li("landings_unit_label = {landings_unit_label}")
<<<<<<< HEAD
=======
  #     cli::cli_li("end_year = {end_year}")
>>>>>>> 6ca5b6c (update save_all_plots and work on tests)
  #     print(e)
  #   }
  # )

  tryCatch(
    {
      cli::cli_h2("plot_recruitment_deviations")
      plot_recruitment_deviations(
        dat,
        interactive = interactive,
        make_rda = TRUE,
        figures_dir = figures_tables_dir
      ) #|>
      # suppressWarnings() |>
      # invisible()
    },
    error = function(e) {
      cli::cli_alert_danger("plot_recruitment_deviations failed to run.")
      cli::cli_alert("Tip: check that your arguments are correct.")
      print(e)
    }
  )

  # plot_spawn_recruitment(dat,
  #                        spawning_biomass_label,
  #                        recruitment_label = recruitment_unit_label,
  #                        make_rda,
  #                        figures_dir = figures_tables_dir)# |> suppressWarnings() |> invisible()

  tryCatch(
    {
      cli::cli_h2("plot_spawning_biomass")
      plot_spawning_biomass(
        dat,
        unit_label = spawning_biomass_label,
        scale_amount = spawning_biomass_scale_amount,
        ref_line = ref_line_sb,
        relative = relative,
        interactive = interactive,
        make_rda = TRUE,
        figures_dir = figures_tables_dir
      ) # |>
      # suppressWarnings() |>
      # invisible()
    },
    error = function(e) {
      cli::cli_alert_danger("plot_spawning_biomass failed to run.")
      cli::cli_alert("Tip: check that your arguments are correct.")
      cli::cli_li("spawning_biomass_label = {spawning_biomass_label}")
      cli::cli_li("spawning_biomass_scale_amount = {spawning_biomass_scale_amount}")
      cli::cli_li("ref_line_sb = {ref_line_sb}")
      cli::cli_li("relative = {relative}")
      print(e)
    }
  )

  tryCatch(
    {
      cli::cli_h2("plot_abundance_at_age")
      plot_abundance_at_age(
        dat,
        unit_label = abundance_at_age_unit_label,
        scale_amount = abundance_at_age_scale_amount,
        proportional = proportional,
        make_rda = TRUE,
        figures_dir = figures_tables_dir
      ) # |>
      # suppressWarnings() |>
      # invisible()
    },
    error = function(e) {
      cli::cli_alert_danger("plot_abundance_at_age failed to run.")
      cli::cli_alert("Tip: check that your arguments are correct.")
      cli::cli_li("abundance_at_age_unit_label = {abundance_at_age_unit_label}")
      cli::cli_li("abundance_at_age_scale_amount = {abundance_at_age_scale_amount}")
      print(e)
    }
  )

  tryCatch(
    {
      plot_catch_comp(
        dat,
        unit_label = catch_unit_label,
        scale_amount = catch_scale_amount,
        proportional = proportional,
        make_rda = TRUE,
        figures_dir = figures_tables_dir
      ) # |>
      # suppressWarnings() |>
      # invisible()
    },
    error = function(e) {
      message("plot_catch_comp failed to run. Tip: check that your arguments are correct.")
      print(e)
    }
  )

  tryCatch(
    {
      cli::cli_h2("plot_biomass_at_age")
      plot_biomass_at_age(
        dat,
        unit_label = biomass_at_age_unit_label,
        scale_amount = biomass_at_age_scale_amount,
        proportional = proportional,
        make_rda = TRUE,
        figures_dir = figures_tables_dir
      ) # |>
      # suppressWarnings() |>
      # invisible()
    },
    error = function(e) {
  cli::cli_alert_danger("plot_biomass_at_age failed to run.")
  cli::cli_alert("Tip: check that your arguments are correct.")
  cli::cli_li("biomass_at_age_unit_label = {biomass_at_age_unit_label}")
  cli::cli_li("biomass_at_age_scale_amount = {biomass_at_age_scale_amount}")
  print(e)
    }
  )

  # uncomment when this is working properly
  # plot_indices(dat,
  #                    unit_label = indices_unit_label,
  #                    make_rda,
  #                    figures_dir = figures_tables_dir)# |> suppressWarnings() |> invisible()

  # tables
  # tryCatch(
  #   {
  #     cli::cli_h2("table_bnc")
  #     table_bnc(
  #       dat,
  #       biomass_unit_label,
  #       catch_unit_label,
  #       spawning_biomass_label,
  #       make_rda,
  #       tables_dir = figures_tables_dir
  #     ) # |>
  #     # suppressWarnings() |>
  #     # invisible()
  #   },
  #   error = function(e) {
  #     cli::cli_alert_danger("table_bnc failed to run.")
  #     cli::cli_alert("Tip: check that your arguments are correct.")
<<<<<<< HEAD
=======
  #     cli::cli_li("end_year = {end_year}")
>>>>>>> 6ca5b6c (update save_all_plots and work on tests)
  #     cli::cli_li("biomass_unit_label = {biomass_unit_label}")
  #     cli::cli_li("catch_unit_label = {catch_unit_label}")
  #     cli::cli_li("spawning_biomass_label = {spawning_biomass_label}")
  #     print(e)
  #   }
  # )

  # tryCatch(
  #   {
  #     cli::cli_h2("table_indices")
  #     table_indices(
  #       dat,
  #       make_rda,
  #       tables_dir = figures_tables_dir
  #     ) # |>
  #     # suppressWarnings() |>
  #     # invisible()
  #   },
  #   error = function(e) {
  #     cli::cli_alert_danger("table_indices failed to run.")
  #     cli::cli_alert("Tip: check that your arguments are correct.")
<<<<<<< HEAD
=======
  #     cli::cli_li("end_year = {end_year}")
>>>>>>> 6ca5b6c (update save_all_plots and work on tests)
  #     print(e)
  #   }
  # )

  # tryCatch(
  #   {
  #     cli::cli_h2("table_landings")
  #     table_landings(dat,
  #       unit_label = landings_unit_label,
  #       make_rda,
  #       tables_dir = figures_tables_dir
  #     ) # |>
  #     # suppressWarnings() |>
  #     # invisible()
  #   },
  #   error = function(e) {
  #     cli::cli_alert_danger("table_landings failed to run.")
  #     cli::cli_alert("Tip: check that your arguments are correct.")
  #     cli::cli_li("landings_unit_label = {landings_unit_label}")
<<<<<<< HEAD
=======
  #     cli::cli_li("end_year = {end_year}")
>>>>>>> 6ca5b6c (update save_all_plots and work on tests)
  #     print(e)
  #   }
  # )

  # uncomment when finished
  #
  # undeveloped tables - add arguments after more development
  # table_afsc_tier() #|> suppressWarnings() |> invisible()
  # table_harvest_projection() #|> suppressWarnings() |> invisible()
  cli::cli_h1("Finished export of figures and tables.")
}
