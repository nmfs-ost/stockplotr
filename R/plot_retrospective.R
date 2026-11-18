#' Plot retrospetive peels
#' 
#' Plot a given quantity across multiple models with differing 
#' end years, so as to plot retrospective peels.
#' 
#' @inheritParams plot_spawning_biomass
#' 
#' @param end_years Integer. vector of what year, relative to the maximum
#' year present in `dat`, should be treated as the final year for each
#' model. Should be <= 0.
#' 
#' Default: 0
#' 
#' @param quantity String. Assessment quantity to plot (e.g., spawning biomass, 
#' biomass, reruitment, etc.). Should correspond to an existing `plot_*` function.
#' 
#' Default: "spawning_biomass" 
#' 
#' @param show_end_year_points Logical. Whether to add points at the terminal year
#' for each model peel.
#' 
#' Default: TRUE
#' 
#' @param mohns_rho Numeric. Externally calculated value(s) of mohn's rho. Should be
#' either a single value or of length `end_years-1`. Value of NULL disables
#' reporting Mohn's rho statsitic on plot. If vector, names of vector entries should
#' match with model names.
#' 
#' Default: NULL
#' 
#' @param group String. Single column that groups the data.
#' 
#' Default: "model"
#' 
#' @return A plot showing values of an assessment quantity across different 
#' retrospective peels.
#'
#' @details The input is from an assessment model output file
#' translated to a standardized output (\link[stockplotr]{convert_output}).
#' There are options to return a `ggplot2` object or export an .rda object
#' containing associated caption and alternative text for the figure.
#'
#' @note
#' All plotting functions automatically recognize indexing variables and will
#' use them in groupings and/or facetting, but will prioritize the defined
#' group or facet by the function arguments. @seealso [process_data()].
#'
#' @seealso [convert_output()], [plot_timeseries()], [calculate_reference_point()], [reference_line()], [filter_data()], [process_data()], [export_kqs()], [insert_kqs()], [create_rda()]
#'
#' @export
#'
#' @examples
#' plot_retrospective(
#'   dat = ss3_simple,
#'   end_years = 0:-5,
#'   quantity = "spawning_biomass",
#'   show_end_year_points=TRUE
#' )
#' plot_retrospective(
#'   dat = ss3_simple,
#'   end_years = 0:-5,
#'   quantity = "spr",
#'   show_end_year_points=TRUE,
#'   interactive=FALSE,
#'   module = "derived_quants"
#' )
#' plot_retrospective(
#'   dat = ss3_simple,
#'   end_years = 0:-5,
#'   quantity = "spr",
#'   show_end_year_points=TRUE,
#'   mohns_rho = c(0.023, 0.01, 1.25, 0.845, 0.068),
#'   interactive=FALSE,
#'   module = "derived_quants"
#' )
plot_retrospective <- function(
  dat,
  end_years=NULL,
  quantity="spawning_biomass",
  show_end_year_points=TRUE,
  mohns_rho = NULL,
  group="model",
  make_rda=FALSE,
  figures_dir=getwd(),
  ...
){

  # plot_biomass, plot_spawning_biomass, plot_fishing_mortality, plot_recruitment, plot_spr
  quantity <- match.arg(quantity, c("biomass", "spawning_biomass", "fishing_mortality", "recruitment", "spr"))

  func_name <- paste0("plot_",quantity)
  if(!(func_name %in% ls(getNamespace("stockplotr")))){
    cli::cli_abort("`quantity` not valid. Should correspond to one of the available stockplotr plotting functions.")
  }

  n_models <- length(unique(dat$model))

  if(is.null(end_years)) end_years <- rep(0, n_models)
  if(length(end_years) != n_models){
    cli::cli_alert_info("Different number of end_years and available models.")
    end_years <- c(end_years, rep(0, n_models-length(end_years)))
  }

  if(n_models == 1) 
    cli::cli_alert_info("Only 1 model detected in `dat`. Function expects multiple models corresponding to unique retrospective peels.")

  if(is.null(names(end_years))){
    cli::cli_alert("`end_years` provided without names. names automatically applied.")
    names(end_years) <- dat$model |> unique()
  }

  if(!all(is.null(mohns_rho)) && length(mohns_rho) != n_models-1){
    cli::cli_alert_warning("Invalid number of mohn's rho values provided. Should be number of peels - 1. Mohn's rho values will not be reported.")
    mohns_rho <- NULL
  }

  if(!all(is.null(mohns_rho)) && is.null(names(mohns_rho))){
    cli::cli_alert("`mohns_rho` values provided without names. names automatically applied.")
    names(mohns_rho) <- unique(dat$model)[-1]
  }

  retro_dat <- dat |>
    dplyr::filter(!is.na(year), !is.na(estimate)) |>
    dplyr::mutate(term_year=max(year)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
        max_yr = term_year+end_years[which(model==names(end_years))],
        peel = abs(end_years[which(model==names(end_years))])
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(year < max_yr-1) |>
    dplyr::select(-term_year, -max_yr)

  
  func_pars <- list(
    dat = retro_dat, 
    group="peel",
    model = "All", # turn this on to bypass interactive model selection and plot all available models
    make_rda=FALSE, # don't want to overwrite files from non-retro plots
    ...
  )

  plt <- do.call(func_name, func_pars)
  plot_data <- plt$layers[[2]]$data |> 
    dplyr::rename(
        dplyr::any_of(c(estimate="predicted_recruitment"))
    )

  if(show_end_year_points){
    # get data from the "geom_line" elements and identfiy terminal year estimate
    term_dat <- plot_data |> 
      dplyr::slice_max(order_by=year, n=1, by=model)

    plt <- plt+
      ggplot2::geom_point(
        data=term_dat, 
        ggplot2::aes(
            x=.data[["year"]], 
            y=.data[["estimate"]], 
            color={
              if (length(unique(.data[["model"]])) > 1) {
                interaction(model, group_var)
              } else {
                group_var
              }
            }
        ),
        size=3.5
      )
  }

  if(!all(is.null(mohns_rho))){

    mohns_df <- data.frame(
      label = sprintf("%.3f", mohns_rho)
    )
    mohns_df$model = names(mohns_rho)
    mohns_df$vjust = 0.5 + seq_along(mohns_rho-1)*1.5
    mohns_df$label <- paste0(mohns_df$model, " Mohns Rho:\t", mohns_df$label)

    plt <- plt + ggplot2::geom_text(
      data = mohns_df,
      ggplot2::aes(
        x=Inf,
        y=Inf,
        label=label,
        vjust=vjust
      ),
      # family = "mono",
      size=6,
      hjust=1.1
    )

  }

  plt <- plt +
    ggplot2::scale_color_discrete(labels=unique(retro_dat$model))+
    ggplot2::scale_fill_discrete(labels=unique(retro_dat$model))+
    ggplot2::guides(
      color=ggplot2::guide_legend("Peel"), 
      fill=ggplot2::guide_legend("Peel")
    )


  if(make_rda){
    
    create_rda(
      object = plt,
      topic_label = glue::glue("retrospective_{quantity}"),
      fig_or_table = "figure",
      dat = NULL,
      dir = figures_dir,
      ref_line = ifelse(!is.null(names(ref_line)), names(ref_line), ref_line),
      scale_amount = scale_amount
    )

    cli::cli_alert_danger("No captions, alt-text, or key quantities were stored at this time.")
  }
  
  plt

}

# plot_retrospective(dat)
