#' Catch (table) by fleet, gear type, sector
#'
#' @inheritParams plot_recruitment
#' @param unit_label String. Abbreviated catch units
#'
#' Default: "mt"
#' 
#' @param digits Number. Numeric value indicating the number of digits catch values in the
#' table will be rounded to.
#'
#' Default: 2
#' 
#' @param tables_dir Path. The location of the folder containing the generated table
#' rda files ("tables") that will be created if the argument `make_rda` = TRUE.
#'
#' Default: the working directory (`getwd()`)
#'
#' @returns A table ready of landed catch by fleet and year.
#'
#' @details The input is from an assessment model output file
#' translated to a standardized output (\link[stockplotr]{convert_output}).
#' There are options to return a [gt::gt()] object or export an rda object
#' containing a gt-based table, caption, and LaTeX-based table.
#'
#' @seealso [convert_output()], [filter_data()], [process_table()], [export_kqs()], [insert_kqs()], [create_rda()]
#' @export
#'
#' @examples
#' table_catch(stockplotr::example_data,
#' module = "TIME_SERIES")
#'
#' table_catch(
#'   stockplotr::example_data,
#'   unit_label = "lbs",
#'   module = "TIME_SERIES",
#'   digits = 4,
#'   scale_amount = 100
#' )
table_catch <- function(
  dat,
  unit_label = "mt",
  era = NULL,
  interactive = TRUE,
  module = NULL,
  scale_amount = 1,
  digits = 2,
  make_rda = FALSE,
  tables_dir = getwd()
) {
  # set unit label if scaled
  unit_label <- label_magnitude(
    label = "",
    unit_label = unit_label,
    scale_amount = scale_amount
  )

  # Filter data for catch
  prepared_data <- filter_data(
    dat = dat,
    label_name = "catch",
    geom = "line",
    era = era,
    module = module,
    scale_amount = scale_amount,
    interactive = interactive
  ) |>
    dplyr::filter(label %in% c("catch_retained", "catch_dead", "catch_selected", "catch")) |>
    dplyr::filter(!is.na(year)) |>
    dplyr::filter(!is.na(estimate))

  # Check if there is any data and if all labels contain "catchability", not "catch"
  if (nrow(prepared_data) == 0 | unique(stringr::str_detect(prepared_data$label, "catchability"))) {
    cli::cli_abort("No catch data found.")
  }
  
  # get uncertainty label by model
  uncert_lab <- prepared_data |>
    dplyr::filter(!is.na(uncertainty_label)) |>
    dplyr::group_by(model) |>
    dplyr::reframe(unique_uncert = unique(uncertainty_label)) # changed to reframe -- may cause errors
  uncert_lab <- stats::setNames(uncert_lab$unique_uncert, uncert_lab$model)
  # if (length(unique(uncert_lab)) == 1) uncert_lab <- unique(uncert_lab) # might need this line
  
  # This needs to be adjusted when comparing different models and diff error
  if (length(uncert_lab) > 1 & length(unique(uncert_lab)) == 1 | length(names(uncert_lab)) == 1) { # prepared_data$model
    # cli::cli_alert_warning("More than one value for uncertainty exists: {uncert_lab}")
    uncert_lab <- uncert_lab[[1]]
    # cli::cli_alert_warning("The first value ({uncert_lab}) will be chosen.")
  }
  
  if (length(uncert_lab) == 0 || is.na(uncert_lab)) uncert_lab <- "uncertainty"

  catch <- prepared_data |>
    dplyr::group_by(year, fleet, sex, area, season, type, uncertainty) |>
    dplyr::summarise(total_catch = sum(estimate)) |>
    dplyr::mutate(total_catch = round(total_catch, digits = digits)) |>
    dplyr::mutate(total_catch = format(total_catch, big.mark = ",")) |>
    dplyr::ungroup()
  
  # filter out columns if there is only one unique value
  cols_to_remove <- c()
  for (i in 1:ncol(catch)){
    if (length(unique(catch[[i]])) == 1 & names(catch[i]) != "uncertainty") {
      cols_to_remove <- c(cols_to_remove,
                        colnames(catch[i]))
    }
    if (names(catch[i]) == "uncertainty" && is.na(unique(catch[[i]]))) {
      cols_to_remove <- c(cols_to_remove,
                          colnames(catch[i]))
    }
  }
  
  catch <- catch |>
    dplyr::select(-cols_to_remove)
  
  if ("uncertainty" %in% colnames(catch)) {
    catch <- catch |>
      dplyr::mutate(total_catch = paste0(total_catch, " (", uncertainty, ")")) |>
      dplyr::rename_with(~ paste0("total_catch (", uncert_lab, ")"), .cols = total_catch)  |>   
      dplyr::select(-uncertainty)
  }
  
  names(catch) <- stringr::str_to_title(names(catch))
  
  if (uncert_lab %in% cols_to_remove){
    uncert_lab <- ""
  }
  
  if ("Fleet" %in% colnames(catch)){
    if (uncert_lab != ""){
      catch <- catch |>
        tidyr::pivot_wider(names_from = Fleet,
                           names_glue = stringr::str_glue("Fleet {{Fleet}}{unit_label} ({uncert_lab})"),
                           values_from = dplyr::starts_with("Total_catch"))
    } else {
      catch <- catch |>
        tidyr::pivot_wider(names_from = Fleet,
                           names_glue = stringr::str_glue("Fleet {{Fleet}}{unit_label}"),
                           values_from = dplyr::starts_with("Total_catch"))
    }
  } else {
    if (uncert_lab != ""){
      catch <- catch |>
        dplyr::rename_with(~ paste0("Catch", unit_label, " (", uncert_lab, ")"), .cols = dplyr::starts_with("Total_catch"))
      } else {
      catch <- catch |>
        dplyr::rename_with(~ paste0("Catch", unit_label), .cols = dplyr::starts_with("Total_catch"))}
    }
  
  # transform dfs into tables
  final <- catch |>
      gt::gt() |>
      theme_table()

  # export figure to rda if argument = T
  if (make_rda == TRUE) {
    # Obtain relevant key quantities for captions/alt text
    tot.catch.units <- unit_label

    # calculate & export key quantities
    export_kqs(tot.catch.units)

    # Add key quantities to captions/alt text
    insert_kqs(tot.catch.units)

    create_rda(
      object = final,
      # get name of function and remove "table_" from it
      topic_label = gsub("table_", "", utils::tail(as.character(sys.call()[[1]]), n = 1)),
      fig_or_table = "table",
      dat = dat,
      dir = tables_dir,
      scale_amount = 1,
      unit_label = unit_label,
      table_df = final
    )

    cli::cli_alert_warning("Multiple tables cannot be exported at this time.")
    cli::cli_alert_info("We are currently developing this feature.")
  }

  # Send table(s) to viewer
  final
}
