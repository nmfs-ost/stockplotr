#' Add NOAA formatting to a table
#'
#' @param x Object. Table from gt, flextable, or kableExtra
#'
#' @returns Add the standard formatting for stock assessment reports for any table.
#' @details Currently, the function can format table objects from:
#' flextable ({flextable} package, when installed), gt ({gt}), and
#' kable ({kableExtra}).
#' @export
#'
#' @examples
#' theme_table(cars |> gt::gt())
#' theme_table(cars |> kableExtra::kable())
theme_table <- function(x) {
  if (class(x)[1] == "flextable") {
    if (!requireNamespace("flextable", quietly = TRUE)) {
      warning("The 'flextable' package is not installed. Returning the flextable object unchanged.", call. = FALSE)
      theme_obj <- x
    } else {
      FitFlextableToPage <- function(ft, pgwidth = 6) {
        ft_out <- ft |> flextable::autofit()

        ft_out <- flextable::width(ft_out, width = dim(ft_out)$widths * pgwidth / (flextable::flextable_dim(ft_out)$widths))
        return(ft_out)
      }
      theme_obj <- x |>
        flextable::merge_h(i = 1, part = "header") |>
        flextable::align(part = "header", align = "center") |>
        # flextable::font(fontname = "arial narrow",
        #                 part = "all") |>
        flextable::bold(part = "header") |>
        flextable::add_header_lines(top = FALSE) |>
        flextable::align(align = "center", part = "body") |>
        flextable::autofit()
      # FitFlextableToPage()
    }
  } else if (class(x)[1][1] == "gt_tbl") {
    # gt object
    theme_obj <- x |>
      gt::cols_align(align = "center") |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_column_labels()
      )
  } else if (class(x)[1] == "knitr_kable") {
    theme_obj <- x
  } else {
    cli::cli_alert_danger("NOAA formatting cannot be applied to this object.")
  }

  theme_obj
}
