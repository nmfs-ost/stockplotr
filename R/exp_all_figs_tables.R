#' Export all figures and tables
#'
#' Export all figures and tables to Rda files within one function.
#'
#' @inheritParams plot_recruitment
#' @param recruitment_scale_amount A number describing how much to scale down
#' the recruitment quantities shown on the y axis. For example,
#' recruitment_scale_amount = 100 would scale down a value from 500,000 -->
#' 5,000. This scale will be reflected in the y axis label.
#' @param recruitment_unit_label Units for recruitment
#' @param ref_line A string specifying the type of reference you want to
#' compare biomass to. The default is `"target"`, which looks for
#' `"biomass_target"` in the `"label"` column of `dat`. The actual
#' searching in `dat` is case agnostic and will work with either upper- or
#' lower-case letters but you must use one of the options specified in the
#' default list to ensure that the label on the figure looks correct
#' regardless of how it is specified in `dat`.
#' @param ref_point A known value of the reference point along with the label
#' for the reference point as specified in the output file. Please use this
#' option if the ref_line cannot find your desired point. Indicate the
#' reference point in the form c("label" = value).
#' @param biomass_scale_amount A number describing how much to scale down the
#' biomass quantities shown on the y axis. See `recruitment_scale_amount`.
#' @param landings_unit_label Units for landings
#' @param biomass_unit_label Units for biomass
#' @param spawning_biomass_label Units for spawning biomass
#' @param spawning_biomass_scale_amount  A number describing how much to scale down the
#' spawning biomass quantities shown on the y axis. See `recruitment_scale_amount`.
#' @param ref_line_sb Identical definition as `ref_line`, but this argument is
#' applied to plot_spawning_biomass.
#' @param ref_point_sb Identical definition as `ref_point`, but this argument is
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
#' @param landings_unit_label Units for landings
#'
#' @return Rda files for each figure/table.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' exp_all_figs_tables(dat,
#'   end_year = 2022, ref_line = "unfished", ref_point = 13000,
#'   ref_point_sb = 13000, ref_line_sb = "target", indices_unit_label = "CPUE",
#'   biomass_at_age_scale_amount = 1, biomass_at_age_unit_label = "metric tons"
#' )
#' }
exp_all_figs_tables <- function(
    # imported from plot_recruitment
    dat,
    recruitment_unit_label = "mt", # changed from unit_label to recruitment_unit_label for specificity
    recruitment_scale_amount = 1,
    end_year = NULL,
    relative = FALSE,
    rda_dir = getwd(),
    # imported from plot_biomass
    ref_line = c("target", "MSY", "msy", "unfished"),
    ref_point = NULL,
    biomass_scale_amount = 1,
    # imported from plot_landings
    landings_unit_label = "mt",
    # imported from plot_recruitment_deviations- zero unique arguments
    # imported from plot_spawn_recruitment
    spawning_biomass_label = "mt",
    spawning_biomass_scale_amount = 1,
    # imported from plot_spawning_biomass
    ref_line_sb = c("target", "MSY", "msy", "unfished"),
    ref_point_sb = NULL,
    # imported from plot_abundance_at_age
    abundance_at_age_scale_amount = 1000,
    abundance_at_age_unit_label = "fish",
    # imported from plot_biomass_at_age
    biomass_at_age_scale_amount = 1,
    biomass_at_age_unit_label = "metric tons",
    # imported from plot_indices
    indices_unit_label = NULL,
    # imported from table_afsc_tier- add potential unique arguments after dev
    # imported from table_bnc
    biomass_unit_label = "mt",
    catch_unit_label = "mt"
    # imported from table_harvest_projection- add potential unique arguments after dev
    # imported from table_indices- zero unique arguments
    # imported from table_landings- zero unique arguments
    ) {
  make_rda <- TRUE

  cli::cli_alert_info("Starting export of figures and tables:")

  # figures

  tryCatch(
    {
      stockplotr::plot_recruitment(
        dat,
        unit_label = recruitment_unit_label,
        scale_amount = recruitment_scale_amount,
        end_year,
        relative,
        make_rda = TRUE,
        rda_dir
      ) #|>
      # suppressWarnings() |>
      # invisible()
    },
    error = function(e) {
      cli::cli_alert_danger("plot_recruitment failed to run. Tip: check that your arguments are correct.")
      print(e)
    }
  )


  tryCatch(
    {
      stockplotr::plot_biomass(
        dat,
        unit_label = biomass_unit_label,
        scale_amount = biomass_scale_amount,
        ref_line,
        ref_point,
        end_year,
        relative,
        make_rda,
        rda_dir
      ) #|>
      # suppressWarnings() |>
      # invisible()
    },
    error = function(e) {
      cli::cli_alert_danger("plot_biomass failed to run. Tip: check that your arguments are correct.")
      print(e)
    }
  )


  tryCatch(
    {
      stockplotr::plot_landings(dat,
        unit_label = landings_unit_label,
        end_year,
        make_rda,
        rda_dir
      ) # |>
      # suppressWarnings() |>
      # invisible()
    },
    error = function(e) {
      cli::cli_alert_danger("plot_landings failed to run. Tip: check that your arguments are correct.")
      print(e)
    }
  )

  tryCatch(
    {
      stockplotr::plot_recruitment_deviations(
        dat,
        end_year,
        make_rda,
        rda_dir
      ) #|>
      # suppressWarnings() |>
      # invisible()
    },
    error = function(e) {
      cli::cli_alert_danger("plot_recruitment_deviations failed to run. Tip: check that your arguments are correct.")
      print(e)
    }
  )

  # stockplotr::plot_spawn_recruitment(dat,
  #                        spawning_biomass_label,
  #                        recruitment_label = recruitment_unit_label,
  #                        end_year,
  #                        make_rda,
  #                        rda_dir)# |> suppressWarnings() |> invisible()

  tryCatch(
    {
      stockplotr::plot_spawning_biomass(
        dat,
        unit_label = spawning_biomass_label,
        scale_amount = spawning_biomass_scale_amount,
        ref_line = ref_line_sb,
        ref_point = ref_point_sb,
        end_year,
        relative,
        make_rda,
        rda_dir
      ) # |>
      # suppressWarnings() |>
      # invisible()
    },
    error = function(e) {
      cli::cli_alert_danger("plot_spawning_biomass failed to run. Tip: check that your arguments are correct.")
      print(e)
    }
  )

  tryCatch(
    {
      stockplotr::plot_abundance_at_age(
        dat,
        unit_label = abundance_at_age_unit_label,
        scale_amount = abundance_at_age_scale_amount,
        end_year,
        make_rda,
        rda_dir
      ) # |>
      # suppressWarnings() |>
      # invisible()
    },
    error = function(e) {
      cli::cli_alert_danger("plot_abundance_at_age failed to run. Tip: check that your arguments are correct.")
      print(e)
    }
  )

  # tryCatch(
  #   {
  #     stockplotr::plot_biomass_at_age(
  #       dat,
  #       unit_label = biomass_at_age_unit_label,
  #       scale_amount = biomass_at_age_scale_amount,
  #       end_year,
  #       make_rda,
  #       rda_dir
  #     ) # |>
  #     # suppressWarnings() |>
  #     # invisible()
  #   },
  #   error = function(e) {
  #     cli::cli_alert_danger("plot_biomass_at_age failed to run. Tip: check that your arguments are correct.")
  #     print(e)
  #   }
  # )

  # uncomment when this is working properly
  # stockplotr::plot_indices(dat,
  #                    unit_label = indices_unit_label,
  #                    make_rda,
  #                    rda_dir)# |> suppressWarnings() |> invisible()

  # tables
  tryCatch(
    {
      stockplotr::table_bnc(
        dat,
        end_year,
        biomass_unit_label,
        catch_unit_label,
        spawning_biomass_label,
        make_rda,
        rda_dir
      ) # |>
      # suppressWarnings() |>
      # invisible()
    },
    error = function(e) {
      cli::cli_alert_danger("table_bnc failed to run. Tip: check that your arguments are correct.")
      print(e)
    }
  )

  tryCatch(
    {
      stockplotr::table_indices(
        dat,
        end_year,
        make_rda,
        rda_dir
      ) # |>
      # suppressWarnings() |>
      # invisible()
    },
    error = function(e) {
      cli::cli_alert_danger("table_indices failed to run. Tip: check that your arguments are correct.")
      print(e)
    }
  )

  tryCatch(
    {
      stockplotr::table_landings(dat,
        unit_label = landings_unit_label,
        end_year,
        make_rda,
        rda_dir
      ) # |>
      # suppressWarnings() |>
      # invisible()
    },
    error = function(e) {
      cli::cli_alert_danger("table_landings failed to run. Tip: check that your arguments are correct.")
      print(e)
    }
  )

  # uncomment when finished
  #
  # undeveloped tables - add arguments after more development
  # table_afsc_tier() #|> suppressWarnings() |> invisible()
  # table_harvest_projection() #|> suppressWarnings() |> invisible()
  cli::cli_alert_success("Finished export of figures and tables.")
}
