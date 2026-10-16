## Code to prepare `example_comp_data`
# Simulated sex-specific age compositions for a fishery and a survey.
# Expected values come from a simple age-structured operating model. The
# observed fishery data are generated with dome-shaped selectivity while the
# expected values assume asymptotic selectivity, which creates a pattern of
# misfit at older ages that the composition diagnostics should reveal.
set.seed(20261016)

years <- 2001:2020
ages <- 1:15
n_years <- length(years)
n_ages <- length(ages)

M <- c(female = 0.20, male = 0.28)
F_year <- 0.15 + 0.15 * sin(seq(0, pi, length.out = n_years))
rec_dev <- stats::rnorm(n_years + n_ages, 0, 0.5)
# A few strong year classes to show up as diagonals in bubble plots
rec_dev[n_ages + which(years %in% c(2003, 2010, 2016)) - 1] <- c(1.3, 1.1, 1.2)

logistic <- function(a, a50, slope) 1 / (1 + exp(-slope * (a - a50)))
sel_fishery_model <- logistic(ages, 5, 1.2)
sel_fishery_true <- sel_fishery_model * exp(-0.3 * pmax(ages - 7, 0))
sel_survey <- logistic(ages, 2.5, 1.5)

# Numbers at age by sex
N <- array(0, dim = c(n_years, n_ages, 2), dimnames = list(years, ages, names(M)))
for (s in 1:2) {
  # Initial age structure
  N[1, , s] <- 0.5 * exp(rec_dev[n_ages:1] - cumsum(c(0, rep(M[s] + 0.1, n_ages - 1))))
  for (y in 2:n_years) {
    Z <- M[s] + F_year[y - 1] * sel_fishery_model
    N[y, 1, s] <- 0.5 * exp(rec_dev[n_ages + y - 1])
    N[y, 2:n_ages, s] <- N[y - 1, 1:(n_ages - 1), s] * exp(-Z[1:(n_ages - 1)])
    N[y, n_ages, s] <- N[y, n_ages, s] + N[y - 1, n_ages, s] * exp(-Z[n_ages])
  }
}

simulate_fleet <- function(fleet, yrs, sel_model, sel_true, n, theta) {
  out <- list()
  for (yr in yrs) {
    y <- which(years == yr)
    exp_model <- c(N[y, , "female"] * sel_model, N[y, , "male"] * sel_model)
    exp_true <- c(N[y, , "female"] * sel_true, N[y, , "male"] * sel_true)
    exp_model <- exp_model / sum(exp_model)
    exp_true <- exp_true / sum(exp_true)
    # Dirichlet-multinomial observation error
    p <- stats::rgamma(length(exp_true), shape = theta * exp_true)
    p <- p / sum(p)
    obs <- as.vector(stats::rmultinom(1, n, p)) / n
    out[[length(out) + 1]] <- data.frame(
      comp_type = "age",
      fleet = fleet,
      year = yr,
      sex = rep(c("female", "male"), each = n_ages),
      bin = rep(ages, 2),
      observed = obs,
      expected = exp_model,
      n = n
    )
  }
  do.call(rbind, out)
}

example_comp_data <- rbind(
  simulate_fleet("Fishery", years, sel_fishery_model, sel_fishery_true, n = 200, theta = 1000),
  simulate_fleet("Survey", 2005:2020, sel_survey, sel_survey, n = 100, theta = 300)
)
example_comp_data <- tibble::as_tibble(example_comp_data)

usethis::use_data(example_comp_data, overwrite = TRUE)
