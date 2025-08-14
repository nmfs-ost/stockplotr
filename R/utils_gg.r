#############################
# GGPLOT2 EXENSION UTILITIES
#############################

# From ggplot2
empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0 || is_waiver(df)
}

is_waiver <- function(x) {
  inherits(x, "waiver")
}

#--------------------------------------------------------------------

# My utils
calculate_reference_point <- function(
    dat,
    reference_name,
    label_name
) {
  # Check if the reference point exists in the data
  if (inherits(try(solve(as.numeric(dat[
    grep(
      pattern = glue::glue("^{glue::glue(\"{label_name}_{reference_name}\")}$"),
      x = dat[["label"]]
    ),
    "y"
  ])), silent = TRUE), "try-error")) {
    ref_line_val <- NULL
  } else {
    ref_line_val <- as.numeric(dat[
      grep(
        pattern = glue::glue("^{glue::glue(\"{label_name}_{reference_name}\")}$"),
        x = dat[["label"]]
      ),
      "y"
    ])
  }
  
  # Check if the reference value was found
  if (length(ref_line_val) == 0) {
    cli::cli_alert_warning(
      "The resulting reference value of `{label_name}_{reference_name}` was not found.",
      wrap = TRUE
    )
    ref_line_val <- 0
  } else if (length(ref_line_val) > 1) {
    cli::cli_alert_warning("More than one of the resulting reference value of `{label_name}_{reference_name}` was found. \n")
    options <- c()
    for (i in seq_along(unique(dat$module_name))) {
      # options <- paste0(options, " ", i, ") ", unique(plot_data$module_name)[i], "\n")
      options[i] <- paste0(" ", i, ") ", unique(dat$module_name)[i])
    }
    ref_line_val <- utils::menu(
      options,
      title = "Please select one:"
    )
    ref_line_val <- as.numeric(ref_line_val)
  }
  ref_line_val
}

#--------------------------------------------------------------------
