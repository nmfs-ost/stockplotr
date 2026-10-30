#' Code to convert Rceattle output to standard framework.
#' 
#' Does not change naming conventions.
#'
#' @inheritParams convert_output
#'
#' @returns Reshaped data frame.
conout_rceattle <- function(
    file,
    out_new,
    fleet_names = NULL
) {
  # Want to extract and set values from:
  # quantities, sdrep, and estimated_params
  # take similar approach to SS3 when only some keywords were converted
  # can late take approach like BAM?
  # TODO: Do we want users to input the saved file or already loaded into the R environment?
  if (is.character(file)) {
    # Anchored and case-insensitive: saveRDS() output is conventionally named
    # either .rds or .RDS, and the real assessment files this targets use the
    # latter. The unanchored, case-sensitive form rejected them.
    if (!grepl("\\.rds$", file, ignore.case = TRUE)) {
      cli::cli_abort("File must be in an .rds format otherwise load data into your environment and run `convert_output()` using the loaded data.")
    }
    dat <- readRDS(file)
  } else {
    dat <- file
  }
  
  # The standardized output describes one stock and has no species dimension,
  # but CEATTLE is a multispecies model. A multispecies fit's `sdrep` holds
  # nspp * nyrs values per label, which fails the `n == length(styr:projyr)`
  # test used to attach years below, so every biomass / spawning biomass /
  # recruitment value would silently come back with `year = NA`. Refuse
  # instead: a converted file that quietly merges two stocks is worse than no
  # converted file.
  # `[[` rather than `$`: `$` partial-matches, so a renamed element would
  # return something else and skip the refusal.
  if (!"data_list" %in% names(dat)) {
    cli::cli_abort(c(
      "Rceattle fit has no {.field data_list} element.",
      i = "The catch, index and composition data are read from it."
    ))
  }
  nspp <- dat[["data_list"]][["nspp"]]
  if (is.null(nspp)) {
    cli::cli_abort(c(
      "Rceattle fit does not report {.field nspp}, so its species count is unknown.",
      i = "The standardized output has no species dimension and must not merge stocks."
    ))
  }
  if (nspp > 1) {
    cli::cli_abort(c(
      "Rceattle model has {nspp} species, and the standardized output has no species dimension.",
      i = "Subset the fit to a single species before converting."
    ))
  }
  
  # Extract or use fleet names
  if (is.null(fleet_names)) {
    fleet_names <- names(dat$estimated_params$index_log_q)
  }
  
  # Output fleet names in console
  cli::cli_alert_info("Identified fleet names:")
  cli::cli_alert_info("{fleet_names}")
  # Create list for morphed dfs to go into (for rbind later)
  out_list <- list()
  
  factors <- c("year", "fleet", "fleet_name", "age", "sex", "area", "seas", "season", "time", "era", "subseas", "subseason", "platoon", "platoo", "growth_pattern", "gp", "nsim", "age_a")
  errors <- c("StdDev", "sd", "se", "SE", "cv", "CV", "stddev")
  # units <- c("mt", "lbs", "eggs")
  
  ##### Loop ####
  # Select the modules by name rather than by position. An Rceattle fit's
  # element list depends on how it was fitted -- `phase = TRUE` adds
  # `.conv_phase` and `phase_params`, and `getsd = FALSE` drops `sdrep` -- so a
  # fixed offset selects different modules for different fits of the same
  # model. Under `phase = TRUE` the positional form dropped `data_list` and
  # passed `obj` to the generic vector handler below, which aborts with
  # "attempt to select less than one element in OneIndex".
  #
  # `sdrep` carries every derived time series, so without it the output holds
  # only the input data. Say so rather than returning a table whose biomass
  # and recruitment are simply absent.
  if (!"sdrep" %in% names(dat)) {
    cli::cli_warn(c(
      "Rceattle fit has no {.field sdrep}, so the converted output has no biomass, spawning biomass or recruitment.",
      i = "Refit with {.code fit_control(getsd = TRUE)} to standardize the derived time series."
    ))
  }
  for (module_name in intersect(c("sdrep", "data_list", "quantities"), names(dat))) {
    extract <- dat[module_name]
    cli::cli_alert_info("Processing {module_name}")
    if (module_name == "sdrep") {
      ##### sdrep ####
      # this does not include all elements from sdrep list
      df <- extract[[1]]
      # Extract values from sdrep element in listdrep
      values <- data.frame(
        label = names(extract[[1]]$value),
        estimate = extract[[1]]$value,
        uncertainty = extract[[1]]$sd,
        uncertainty_label = "sd"
      )
      values_count <- values |>
        dplyr::group_by(label) |>
        dplyr::count()
      values <- values |>
        dplyr::left_join(
          {
            values |>
              dplyr::group_by(label) |>
              dplyr::count()
          },
          by = "label"
        )
      # make year column
      year_col <- rep(
        dat[["data_list"]]$styr:dat[["data_list"]]$projyr,
        length(unique(
          dplyr::filter(values_count, n == length(dat[["data_list"]]$styr:dat[["data_list"]]$projyr)) |>
            dplyr::pull(label)
        ))
      )
      
      df2 <- values |>
        dplyr::filter(n == length(dat[["data_list"]]$styr:dat[["data_list"]]$projyr)) |>
        dplyr::mutate(year = year_col)
      
      df2 <- values |>
        dplyr::filter(
          n != length(dat[["data_list"]]$styr:dat[["data_list"]]$projyr)
        ) |>
        dplyr::mutate(year = NA) |>
        rbind(df2)
      
      # Extract parameter values ts
      # The standard errors of the fixed effects are the square root of the
      # diagonal of cov.fixed, in the same order as par.fixed.
      has_cov_fixed <- !is.null(extract[[1]]$cov.fixed)
      par_fixes <- data.frame(
        label = names(extract[[1]]$par.fixed),
        estimate = extract[[1]]$par.fixed,
        uncertainty = if (has_cov_fixed) sqrt(diag(extract[[1]]$cov.fixed)) else NA_real_,
        uncertainty_label = if (has_cov_fixed) "sd" else NA_character_
      )
      # Random effects, such as rec_dev under random_rec = TRUE, are in
      # par.random rather than par.fixed. Their variances are in
      # diag.cov.random.
      if (length(extract[[1]]$par.random) > 0) {
        has_cov_random <- !is.null(extract[[1]]$diag.cov.random)
        par_fixes <- rbind(
          par_fixes,
          data.frame(
            label = names(extract[[1]]$par.random),
            estimate = extract[[1]]$par.random,
            uncertainty = if (has_cov_random) sqrt(extract[[1]]$diag.cov.random) else NA_real_,
            uncertainty_label = if (has_cov_random) "sd" else NA_character_
          )
        )
      }
      par_fixes_count <- par_fixes |>
        dplyr::group_by(label) |>
        dplyr::count()
      par_fixes <- par_fixes |>
        dplyr::left_join(
          par_fixes_count,
          by = "label"
        )
      
      year_col_par_fix <- rep(
        dat[["data_list"]]$styr:dat[["data_list"]]$endyr,
        length(unique(
          dplyr::filter(par_fixes_count, n == length(dat[["data_list"]]$styr:dat[["data_list"]]$endyr)) |>
            dplyr::pull(label)
        ))
      )
      
      df3 <- par_fixes |>
        dplyr::filter(n == length(dat[["data_list"]]$styr:dat[["data_list"]]$endyr)) |>
        dplyr::mutate(year = year_col_par_fix)
      df3 <- par_fixes |>
        dplyr::filter(
          n != length(dat[["data_list"]]$styr:dat[["data_list"]]$endyr)
        ) |>
        dplyr::mutate(year = NA) |>
        rbind(df3)
      # not sure how pop_scalar is indexed
      # not sure how log_index_hat is indexes
      # Did not use r_sd for the error in rec bc used it from the other element in the list
      
      df4 <- rbind(df2, df3) |>
        dplyr::select(-n) |>
        dplyr::mutate(
          module_name = module_name
        )
      
      df4[setdiff(tolower(names(out_new)), tolower(names(df4)))] <- NA
      out_list[[names(extract)]] <- df4
    } else if (module_name == "data_list") {
      ###### data.list ####
      # Only extract specific quantity needs
      # comp_data?, index_data, catch_data, weight, Ftarget, Flimit
      data_list_list <- list()
      
      ####### Indices ####
      # Extract index_data
      df_index <- extract[[1]]$index_data |>
        # rename all columns to lower case to match standard
        dplyr::rename_with(tolower) |>
        dplyr::rename(
          fleet = fleet_name,
          indices_observed = observation,
          block = selectivity_block,
          uncertainty = log_sd
        ) |>
        dplyr::mutate(
          # label = "index_observed",
          uncertainty_label = "log_sd",
          indices_predicted = dat$quantities$index_hat
        ) |>
        # any_of(): `Q_block` is deprecated in Rceattle and no longer written
        # to `index_data`, so naming it directly fails on a current fit while
        # still needing to be dropped from an older one.
        dplyr::select(-dplyr::any_of(c("fleet_code", "q_block")))
      cli::cli_alert_info("'Selectivity_block' values located in 'block' columns.")
      
      # check if species > 1
      if (length(unique(df_index$species)) > 1) {
        cli::cli_abort("Not currently compatible for multispecies.")
      } else {
        df_index <- df_index |>
          dplyr::select(-species)
      }
      
      # expand df long to have obs and pred in same column with label
      df_index_long <- df_index |>
        tidyr::pivot_longer(
          cols = c(indices_observed, indices_predicted),
          names_to = "label",
          values_to = "estimate"
        ) |>
        dplyr::mutate(
          module_name = names(extract),
          # era = dplyr::if_else(
          #   year > dat$data_list$endyr,
          #   "fore",
          #   NA_character_
          # )
          label = stringr::str_replace_all(label, "indices", "index")
        )
      
      df_index_long[setdiff(tolower(names(out_new)), tolower(names(df_index_long)))] <- NA
      data_list_list[["index_data"]] <- df_index_long
      
      ####### Catch indexing ####
      # Extract catch_data and align with log_index_hat and catch_h
      # Modify sdrep in outlist to include index
      df_catch <- extract[[1]]$catch_data |>
        # dplyr::filter(!is.na(Catch)) |>
        dplyr::rename_with(tolower) |>
        dplyr::mutate(
          module_name = names(extract),
          # Set the era here from the year, as the index and composition
          # frames do, so the frame is complete where it is built. The mutate
          # at the end of the branch recomputes it the same way.
          era = dplyr::if_else(
            year > dat$data_list$endyr,
            "fore",
            "time"
          ),
          catch_h = dat$quantities$catch_hat,
          # TODO: follow up on this quantity
          # log_index_hat = dat$quantities$log_index_hat
          uncertainty_label = "log_sd"
        ) |>
        dplyr::rename(
          catch_predicted = catch_h,
          fleet = fleet_name,
          block = selectivity_block,
          uncertainty = log_sd
        ) |>
        tidyr::pivot_longer(
          cols = c(catch, catch_predicted),
          names_to = "label",
          values_to = "estimate"
        ) |>
        dplyr::select(-c(fleet_code, species))
      
      # Remove fleet names if do not match object
      # if (unique(df_catch$fleet) %notin% fleet_names) {
      #   df_catch <- df_catch |>
      #     dplyr::mutate(fleet = NA)
      # }
      
      if (length(unique(df_catch$month)) == 1) {
        df_catch$month <- NA
      }
      df_catch[setdiff(tolower(names(out_new)), tolower(names(df_catch)))] <- NA
      data_list_list[["catch_data"]] <- df_catch
      
      ####### Comp indexing ####
      df_comp_obs <- dat$data_list$comp_data |>
        dplyr::rename_with(tolower) |>
        dplyr::mutate(
          label = "index_observed"
        )
      
      indexing_vars_cols <- colnames(df_comp_obs)[!grepl("comp", colnames(df_comp_obs))]
      
      # `age_hat` holds one row per row of `comp_data`, in the same order, so
      # it binds column-wise. cross_join() paired every observation with the
      # whole prediction matrix instead, which is a Cartesian product: on the
      # GOAatf fixture it returned the 2,478 correct predictions 59 times
      # over, once per row of comp_data, for 146,202 in all.
      age_hat <- as.data.frame(dat$quantities$age_hat)
      names(age_hat) <- paste0("agehat_", seq_len(ncol(age_hat)))
      df_comp_pred <- dplyr::select(df_comp_obs, dplyr::all_of(indexing_vars_cols)) |>
        cbind(age_hat) |>
        tidyr::pivot_longer(
          cols = -dplyr::all_of(indexing_vars_cols),
          names_to = "age",
          values_to = "estimate"
        ) |>
        # removes the 0 so assuming there will be no recorded zero catch but Rceattle reports ages up to 117
        dplyr::filter(estimate != 0) |>
        dplyr::mutate(
          label = "composition_predicted",
          # NOTE: the below age mutate slows down code
          age = stringr::str_replace(age, "agehat_", "")
        )
      
      # Finish adjusting comp_obs
      # pivot obs data
      df_comp_obs <- df_comp_obs |>
        tidyr::pivot_longer(
          cols = -dplyr::all_of(indexing_vars_cols),
          names_to = "age",
          values_to = "estimate"
        ) |>
        dplyr::filter(!is.na(estimate)) |>
        dplyr::mutate(
          label = "composition_observed",
          # NOTE: the below age mutate slows down code
          age = stringr::str_replace(age, "comp_", "")
        )
      
      df_comp <- rbind(df_comp_obs, df_comp_pred) |>
        # Match the index and catch frames: the standard names the fleet
        # `fleet`, carries no `fleet_name` or `species`, and every row states
        # which module it came from.
        dplyr::rename(fleet = fleet_name) |>
        dplyr::mutate(module_name = names(extract)) |>
        dplyr::select(-dplyr::any_of("species")) |>
        # TODO: do I need to filter sample size by <3 for confidentiality or does this not apply?
        dplyr::select(-c(age0_length1, fleet_code, sample_size))
      
      df_comp[setdiff(tolower(names(out_new)), tolower(names(df_comp)))] <- NA
      data_list_list[["comp_data"]] <- df_comp
      
      # final df for data_list module
      # rbind() rather than bind_rows(): every frame has already been padded
      # to the standard column set above, so rbind() succeeds and keeps the
      # column contract -- it aborts on a frame carrying an extra column,
      # where bind_rows() would silently widen the output.
      new_df <- Reduce(rbind, data_list_list)
      out_list[[names(extract)]] <- new_df
    } else if (module_name == "quantities") {
      ###### quantities ####
      # Only selectivity and natural mortality at age for now. Both are
      # [fleet or species, sex, age, year] arrays. The age dimension is the
      # bin index, so the age itself is offset by minage.
      ages <- dat[["data_list"]][["minage"]] - 1 +
        seq_len(dat[["data_list"]][["nages"]])
      nsex <- dat[["data_list"]][["nsex"]]
      # Rceattle stores sex 1 as females, or both sexes when nsex = 1.
      sex_names <- if (nsex == 1) NA_character_ else c("female", "male")
      quantities_list <- list()
      
      if ("sel_at_age" %in% names(extract[[1]])) {
        sel <- extract[[1]][["sel_at_age"]][, seq_len(nsex), seq_along(ages), , drop = FALSE]
        df_sel <- expand.grid(
          fleet = dimnames(sel)[[1]],
          sex = sex_names,
          age = ages,
          year = as.numeric(dimnames(sel)[[4]]),
          stringsAsFactors = FALSE
        ) |>
          dplyr::mutate(
            estimate = as.vector(sel),
            label = "sel_at_age"
          )
        df_sel[setdiff(tolower(names(out_new)), tolower(names(df_sel)))] <- NA
        quantities_list[["sel_at_age"]] <- df_sel
      }
      
      if ("M_at_age" %in% names(extract[[1]])) {
        m_at_age <- extract[[1]][["M_at_age"]][1, seq_len(nsex), seq_along(ages), , drop = FALSE]
        df_m <- expand.grid(
          sex = sex_names,
          age = ages,
          year = as.numeric(dimnames(m_at_age)[[4]]),
          stringsAsFactors = FALSE
        ) |>
          dplyr::mutate(
            estimate = as.vector(m_at_age),
            label = "m_at_age"
          )
        df_m[setdiff(tolower(names(out_new)), tolower(names(df_m)))] <- NA
        quantities_list[["m_at_age"]] <- df_m
      }
      
      if (length(quantities_list) > 0) {
        out_list[[names(extract)]] <- Reduce(rbind, quantities_list) |>
          dplyr::mutate(
            module_name = names(extract),
            age = as.character(age)
          )
      }
    } else if (is.list(extract[[1]])) { # indicates vector and list
      ##### remaining lmnts ####
      if (any(vapply(extract[[1]], is.matrix, FUN.VALUE = logical(1)))) {
        ##############################################################
        df <- extract[[1]] |>
          expand_element(fleet_names = fleet_names) |>
          dplyr::mutate(
            module_name = module_name
          ) |>
          suppressWarnings()
        df[setdiff(tolower(names(out_new)), tolower(names(df)))] <- NA
        out_list[[names(extract)]] <- df
      } else if (any(vapply(extract[[1]], is.vector, FUN.VALUE = logical(1)))) { # all must be a vector to work - so there must be conditions for dfs with a mix
        extract_list <- list()
        # mod_name1 <- names(extract)
        for (i in seq_along(extract[[1]])) {
          # need to add condition or something in expand_element to account for data thats formatted differently but is still a list i.e. p=9
          if (is.list(extract[[1]][i][[1]])) {
            # mod_name2 <- glue::glue("{module_name}_{names(extract[[1]][i])}")
            # comment out message once finished development
            cli::cli_alert_info("Processing {names(extract[[1]][i])}")
            
            df <- extract[[1]][i][[1]] |>
              expand_element(fleet_names = fleet_names) |>
              dplyr::mutate(
                module_name = module_name # mod_name2
              ) # |>
            # suppressWarnings()
          } else {
            df <- data.frame(
              estimate = extract[[1]][[i]][[1]],
              label = names(extract[[1]][i]),
              module_name = module_name
            )
          }
          df[setdiff(tolower(names(out_new)), tolower(names(df)))] <- NA
          # remove quantities inserted from other steps
          df <- df |>
            dplyr::filter(
              label %notin% c("index_hat", "catch_h")
            )
          extract_list[[names(extract[[1]][i])]] <- df
        }
        new_df <- Reduce(rbind, extract_list)
        out_list[[names(extract)]] <- new_df
      } else {
        cli::cli_alert_warning("Not compatible.")
      }
    } else {
      cli::cli_alert_warning("Not compatible yet.")
    }
    # } else if (is.list(extract[[1]])) { # list only
    # } else if (is.matrix(extract[[1]])) { # matrix only
    # } else {
    #   cli::cli_alert_warning(paste(names(extract), " not compatible.", sep = ""))
    # } # close if statement
  } # close loop over objects listed in dat file
  # Finish out df
  # rbind() as above, so a module that has drifted from the standard column
  # set aborts here rather than widening the output.
  Reduce(rbind, out_list) |>
    # Add era as factor into BAM conout
    dplyr::mutate(
      # TODO: replace all periods with underscore if naming convention is different
      label = tolower(label),
      # set era
      era = dplyr::if_else(
        year > dat$data_list$endyr,
        "fore",
        "time"
      )
    )
}
