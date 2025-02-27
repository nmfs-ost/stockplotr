table_landings <- function(
    dat) {
  # read standard data file and extract target quantity
  land <- dat |>
    dplyr::filter(
      c(module_name == "t.series" & grepl("landings_observed", label)) | c(module_name == "CATCH" & grepl("ret_bio", label)),
      # t.series is associated with a conversion from BAM output and CATCH with SS3 converted output
      !is.na(fleet)
    ) |>
    dplyr::mutate(
      estimate = as.numeric(estimate),
      year = as.numeric(year),
      fleet = as.character(fleet)
    ) |>
    suppressWarnings() |>
    dplyr::filter(
      !is.na(year)
    )

  land <- land |>
    tidyr::pivot_wider(
      id_cols = year,
      names_from = fleet,
      values_from = estimate
    )
}
