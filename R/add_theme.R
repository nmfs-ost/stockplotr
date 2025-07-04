#' Add NOAA formatting to figure or table
#'
#' @param x table or figures object from ggplot, base r plot, gt table, flextable, or kable extra
#'
#' @return Add the standard formatting for stock assessment reports for any
#' figure or table. Currently, the function is able to format objects from:
#' ggplot (ggplot2), base r plot, flextable (flextable), gt tables (gt), and kable tables (kableExtra).
#' @export
#'
#' @examples add_theme(ggplot2::ggplot(data = cars, ggplot2::aes(x = speed, y = dist)) +
#'   ggplot2::geom_point())
add_theme <- function(x) {
  # this is bad coding practice, but what I have for now
  if (class(x)[1] == "flextable") {
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
  } else if (class(x)[1] == "gt_tbl") {
    theme_obj <- x
    # gt object
  } else if (class(x)[1] == "kableExtra" | as.character(class(x)[2]) == "knitr_kable") {
    theme_obj <- x
  } else if (class(x)[1] == "gg" | class(x)[2] == "ggplot") { #  - removed bc wouldn't work with only 1 entry in the class for other object classes
    theme_obj <- x +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "transparent"),
        panel.background = ggplot2::element_rect(fill = "transparent"),
        panel.grid = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.5)
        # text = ggplot2::element_text(size = 12, family = "Cambria")
      ) +
      # add nmfs color palette (palette will be default)
      # I believe including both functions is fine and will
      # works regardless of the type of plot (one won't be used)
      nmfspalette::scale_color_nmfs() +
      nmfspalette::scale_fill_nmfs()
    # Determining how to treat a legend if there is one
    # check if one is present
    # check_for_legend <- function(x) {
    #   'gtable' %in% class(try(cowplot::get_legend(x), silent = TRUE))
    # }
    # if (check_for_legend(x)) {
    #   move_legend <- theme_obj +
    #     ggplot2::theme()
    # }
  } else {
    cli::cli_alert_danger("NOAA formatting cannot be applied to this object.")
  }

  theme_obj
}
