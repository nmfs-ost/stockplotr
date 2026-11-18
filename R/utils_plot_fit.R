# Helpers shared by plots of observed and predicted values over time
# (plot_landings(), plot_discard()): predicted values as lines with
# intervals as ribbons, observed values as points or bars

# The columns that identify a series (and its panel)
fit_keys <- function(dat) {
  intersect(c("model", "group_var", "fleet", "area", "season", "sex"), names(dat))
}

# The predicted values for the series and years with observed values
fit_match_observed <- function(predicted, observed) {
  keys <- fit_keys(observed)
  key <- function(d) do.call(paste, c(lapply(d[keys], as.character), sep = "\r"))
  first <- tapply(observed$year, key(observed), min)
  last <- tapply(observed$year, key(observed), max)
  predicted_key <- key(predicted)
  in_range <- predicted_key %in% names(first) &
    predicted$year >= first[predicted_key] & predicted$year <= last[predicted_key]
  predicted[in_range %in% TRUE, , drop = FALSE]
}

# The range of the differences between the predicted and observed values as
# a proportion of the largest observed value, as in
# r4ss::SSplotCatch(); NA when no years match
fit_difference <- function(observed, predicted) {
  keys <- c(fit_keys(observed), "year")
  both <- dplyr::inner_join(
    observed[c(keys, "estimate")],
    predicted[c(keys, "estimate")],
    by = keys,
    suffix = c("_observed", "_predicted"),
    relationship = "many-to-many"
  )
  if (nrow(both) == 0) {
    return(NA_real_)
  }
  difference <- both$estimate_predicted - both$estimate_observed
  diff(range(difference, na.rm = TRUE)) / max(abs(both$estimate_observed), na.rm = TRUE)
}

# The series (color) of each row: the group, or the model and group when
# there are several models
fit_series <- function(dat, models) {
  if (length(models) > 1) {
    factor(interaction(dat$model, dat$group_var, sep = " "))
  } else {
    factor(dat$group_var)
  }
}

# Axis breaks, a y-axis from zero, and panels shared by the fit plots
fit_finish_plot <- function(plt, dat, facet, xlab = "Year", ylab = NULL) {
  plt <- plt +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::scale_x_continuous(
      breaks = axis_breaks(dat$year),
      guide = ggplot2::guide_axis(minor.ticks = TRUE)
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::expand_limits(y = 0)
  facet <- intersect(facet, colnames(dat))
  if (length(facet) > 0) {
    plt <- plt + ggplot2::facet_wrap(
      stats::reformulate(paste("~", paste(facet, collapse = " + "))),
      labeller = function(labels) {
        # Clean column names (e.g., "growth_pattern" -> "Growth Pattern")
        names(labels) <- tools::toTitleCase(gsub("_", " ", names(labels)))
        # Combine into "Variable: Value" format
        lapply(names(labels), function(var) paste0(var, ": ", labels[[var]]))
      }
    )
  }
  plt
}

# The fit to observed data: the predicted values as lines (with intervals as
# ribbons) and the observed values as points, in the color of each series
fit_plot_lines <- function(observed, predicted, group = NULL, facet = NULL, ylab = NULL, ...) {
  models <- unique(c(observed$model, predicted$model))
  levels <- levels(fit_series(rbind(observed, predicted), models))
  observed$series <- factor(fit_series(observed, models), levels = levels)
  predicted$series <- factor(fit_series(predicted, models), levels = levels)
  one_series <- length(levels) == 1
  ribbons <- predicted[!is.na(predicted$estimate_lower), , drop = FALSE]

  if (one_series) {
    # One series: no fleet colors
    plt <- ggplot2::ggplot() +
      ggplot2::geom_ribbon(
        data = ribbons,
        ggplot2::aes(x = .data$year, ymin = .data$estimate_lower, ymax = .data$estimate_upper),
        fill = "grey70", alpha = 0.5
      ) +
      ggplot2::geom_line(
        data = predicted,
        ggplot2::aes(x = .data$year, y = .data$estimate, linetype = "Predicted"),
        ...
      ) +
      ggplot2::geom_point(
        data = observed,
        ggplot2::aes(x = .data$year, y = .data$estimate, shape = "Observed"),
        size = 2
      )
  } else {
    plt <- ggplot2::ggplot() +
      ggplot2::geom_ribbon(
        data = ribbons,
        ggplot2::aes(
          x = .data$year, ymin = .data$estimate_lower, ymax = .data$estimate_upper,
          fill = .data$series
        ),
        alpha = 0.25, show.legend = FALSE
      ) +
      ggplot2::geom_line(
        data = predicted,
        ggplot2::aes(x = .data$year, y = .data$estimate, colour = .data$series, linetype = "Predicted"),
        ...
      ) +
      ggplot2::geom_point(
        data = observed,
        ggplot2::aes(x = .data$year, y = .data$estimate, colour = .data$series, shape = "Observed"),
        size = 2
      ) +
      ggplot2::scale_colour_discrete(drop = FALSE) +
      ggplot2::scale_fill_discrete(drop = FALSE) +
      ggplot2::labs(
        colour = if (length(models) > 1) {
          paste("Model", cap_first_letter(group))
        } else {
          cap_first_letter(group)
        }
      )
  }
  plt <- plt +
    ggplot2::scale_linetype_manual(name = NULL, values = c(Predicted = "solid")) +
    ggplot2::scale_shape_manual(name = NULL, values = c(Observed = 16)) +
    ggplot2::guides(
      shape = ggplot2::guide_legend(order = 1),
      linetype = ggplot2::guide_legend(order = 2),
      colour = ggplot2::guide_legend(order = 3)
    )
  fit_finish_plot(plt, rbind(observed[names(predicted)], predicted), facet, ylab = ylab)
}

# The observed values of each series as side-by-side bars for each year
# (or stacked, with `position = "stack"`), with a panel for each model
fit_plot_bars <- function(observed, group = NULL, facet = NULL, ylab = NULL, ...) {
  observed$series <- factor(observed$group_var)
  one_series <- length(levels(observed$series)) == 1
  args <- list(...)
  if (is.null(args$position)) {
    args$position <- ggplot2::position_dodge(width = 0.9, preserve = "single")
  }
  if (is.null(args$width)) args$width <- 0.9
  mapping <- if (one_series) {
    ggplot2::aes(x = .data$year, y = .data$estimate)
  } else {
    ggplot2::aes(x = .data$year, y = .data$estimate, fill = .data$series)
  }
  plt <- ggplot2::ggplot() +
    do.call(ggplot2::geom_col, c(list(data = observed, mapping = mapping), args)) +
    ggplot2::labs(fill = cap_first_letter(group))
  if (one_series) {
    plt <- plt + ggplot2::theme(legend.position = "none")
  }
  # Bars of several models are not comparable stacked or side by side
  if (length(unique(observed$model)) > 1) {
    facet <- unique(c("model", facet))
  }
  # Bars start at zero
  suppressMessages(fit_finish_plot(plt, observed, facet, ylab = ylab) +
    ggplot2::scale_y_continuous(
      labels = scales::label_comma(),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ))
}
