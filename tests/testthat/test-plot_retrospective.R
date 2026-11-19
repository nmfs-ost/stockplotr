count_geoms <- function(plot, build, geom) {
  sapply(seq_along(plot$layers), function(i) {
    if (inherits(plot$layers[[i]]$geom, "GeomPoint")) {
      layer_data <- build$data[[i]]
      if (nrow(layer_data) == 0) {
        return(0)
      }
      length(unique(layer_data$group))
    } else {
      0
    }
  })
}

test_that("plot_retrospective with SS3 models", {
  x <- list()
  for (y in 0:-5) {
    x[[length(x) + 1]] <- r4ss::SS_output(paste0("https://raw.githubusercontent.com/iagomosqueira/2026_sol.27.4_review/refs/heads/main/stock_weights/model/ss3_nochange/retrospectives/retro", y))
  }
  names(x) <- paste0("retro-", 0:5)
  dat <- convert_output(x)

  retro <- plot_retrospective(
    dat = dat,
    end_years = 0:5,
    module = "derived_quants"
  )

  built <- ggplot2::ggplot_build(retro)

  expect_equal(sum(count_geoms(retro, built, "GeomLine")), 6)
  expect_equal(sum(count_geoms(retro, built, "GeomPoint")), 6)
})

# test_that("plot_retrospective with FIMS models", {
#   data_4_model <- FIMSFrame(FIMS::data_big)
#   parameters <- setup_default_parameters(data = data_4_model)

#   retro_fit <- run_fims_retrospective(
#     years_to_remove = 0:5,
#     data_4_model,
#     parameters,
#     n_cores = min(parallel::detectCores()-2, length(0:5))
#   )

#   fims_retro <- retro_fit$estimates |>
#       dplyr::rename(
#         year = year_i,
#         estimate = estimated,
#         model=retrospective_peel
#       ) |>
#       dplyr::mutate(
#         uncertainty_label = "se",
#       )

#   plot_retrospective(
#     dat = fims_retro,
#     end_years = 0:-5,
#     quantity="spawning_biomass",
#     unit_label="mt",
#     module="derived_quants"
#   )

# })
