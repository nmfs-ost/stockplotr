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
#' table_catch(stockplotr::example_data)
#'
#' table_catch(
#'   stockplotr::example_data,
#'   unit_label = "lbs"
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
  )

  # Check if there is any data and if all labels contain "catchability", not "catch"
  if (nrow(prepared_data) == 0 | unique(stringr::str_detect(prepared_data$label, "catchability"))) {
    cli::cli_abort("No catch data found.")
  }
  
  catch <- prepared_data |>
    dplyr::filter(label %in% c("catch_retained", "catch_dead", "catch_selected", "catch")) |>
    dplyr::filter(!is.na(year)) |>
    dplyr::filter(!is.na(estimate)) |>
    dplyr::group_by(year, fleet, sex, area, season, type) |>
    dplyr::summarise(total_catch = sum(estimate)) |>
    dplyr::mutate(total_catch = round(total_catch, digits = digits)) |>
    dplyr::ungroup()
  
  # filter out columns if there is only one unique value
  cols_to_remove <- c()
  for (i in 1:ncol(catch)){
    if (length(unique(catch[[i]])) == 1) {
      cols_to_remove <- c(cols_to_remove,
                          colnames(catch[i]))
    }
  }
  
  catch <- catch |>
    dplyr::select(-cols_to_remove)
  
  names(catch) <- stringr::str_to_title(names(catch))
  
  if ("Fleet" %in% colnames(catch)){
    catch <- catch |>
      tidyr::pivot_wider(names_from = Fleet,
                         names_glue = "Fleet {Fleet} {unit_label}",
                         values_from = Total_catch)
  } else {
    catch <- catch |>
      dplyr::rename_with(~ paste0("Catch", unit_label), .cols = Total_catch)
  }
  
  # transform dfs into tables
  final <- catch |>
      gt::gt() |>
      theme_table()

  # export figure to rda if argument = T
  if (make_rda == TRUE) {
    if (length(df_list) == 1) {
      # Obtain relevant key quantities for captions/alt text
      catch.units <- unit_label

      # calculate & export key quantities
      export_kqs(catch.units)

      # Add key quantities to captions/alt text
      insert_kqs(catch.units)

      create_rda(
        object = final[[1]],
        # get name of function and remove "table_" from it
        topic_label = gsub("table_", "", utils::tail(as.character(sys.call()[[1]]), n = 1)),
        fig_or_table = "table",
        dat = dat,
        dir = tables_dir,
        scale_amount = 1,
        unit_label = unit_label,
        table_df = final
      )
    }
  } else {
    cli::cli_alert_warning("Multiple tables cannot be exported at this time.")
    cli::cli_alert_info("We are currently developing this feature.")
  }

  # Send table(s) to viewer
  if (!is.data.frame(table_data)) {
    for (t in final) {
      print(t)
    }
    # Return table list invisibly
    return(invisible(final))
  } else {
    # Return finished table (when only one table)
    return(final)
  }
}
