#' Code to convert BAM output to standard framework.
#' 
#' Does not change naming conventions.
#'
#' @inheritParams convert_output
#'
#' @returns Reshaped data frame.
conout_bam <- function(
    file,
    out_new,
    fleet_names = NULL
) {
  # Extract values from BAM output - model file after following ADMB2R
  dat <- dget(file)
  
  # Find fleet names
  if (is.null(fleet_names)) {
    # Extract names from indices
    indices <- dat$t.series |>
      dplyr::select(dplyr::contains("U.") & dplyr::contains(".ob"))
    fleets_ind <- stringr::str_extract(as.vector(colnames(indices)), "(?<=U\\.)\\w+(?=\\.ob)")
    # Extract names from landings
    landings <- dat$t.series |>
      dplyr::select(dplyr::contains("L.") & dplyr::contains(".ob") |
                      dplyr::contains("D.") & dplyr::contains(".ob"))
    fleets_land <- stringr::str_extract(as.vector(colnames(landings)), "(?<=L\\.)\\w+(?=\\.ob)")
    fleets_disc <- stringr::str_extract(as.vector(colnames(landings)), "(?<=D\\.)\\w+(?=\\.ob)")
    # Extract names from lof F dev
    parm <- dat$parm.tvec |>
      dplyr::select(dplyr::contains("log.F.dev.") |
                      dplyr::contains("log.F.dev.") & dplyr::contains(".D"))
    # fleets_parm_D <- stringr::str_extract(as.vector(colnames(parm)), "(?<=log\\.F\\.dev\\.)\\w+(?=\\.D)")
    fleets_parm <- stringr::str_extract(as.vector(colnames(parm)), "(?<=log\\.F\\.dev\\.)\\w+")
    fleets <- unique(tolower(c(fleets_ind, fleets_land, fleets_disc, fleets_parm)))
    fleet_names <- fleets[!is.na(fleets)]
    if (any(is.na(fleet_names))) {
      cli::cli_abort("No fleet names found in dataframe. Please indicate the abbreviations of fleet names using fleet_names arg.")
    }
  } # else {
  #   # check fleet names are input
  #   # if (any(is.na(fleet_names))) {
  #   fleet_names <- fleet_names
  # }
  # Output fleet names in console
  cli::cli_alert_info("Identified fleet names:")
  cli::cli_alert_info("{fleet_names}")
  # Create list for morphed dfs to go into (for rbind later)
  out_list <- list()
  
  factors <- c("year", "fleet", "fleet_name", "age", "sex", "area", "seas", "season", "time", "era", "subseas", "subseason", "platoon", "platoo", "growth_pattern", "gp", "nsim", "age_a")
  errors <- c("StdDev", "sd", "se", "SE", "cv", "CV", "stddev")
  units <- c("mt", "lbs", "eggs")
  # argument for function when model == BAM
  # fleet_names <- c("cl", "cL","cp","mrip","ct", "hb", "HB", "comm","Mbft","CVID")
  
  ##### BAM loop ----
  # Extract data from list fit to output df
  # Loop over all items to output each object/transform
  # Not transforming or including info chunk
  for (p in 2:length(dat)) {
    extract <- dat[p]
    cli::cli_alert_info(glue::glue("Processing {names(extract)}"))
    # is the object class matrix, list, or vector
    if (is.vector(extract[[1]])) {
      if (is.list(extract[[1]])) { # indicates vector and list
        if (any(vapply(extract[[1]], is.matrix, FUN.VALUE = logical(1)))) {
          extract_list <- list()
          for (i in seq_along(extract[[1]])) {
            if (is.vector(extract[[1]][[i]])) {
              df <- as.data.frame(extract[[1]][i]) |>
                tibble::rownames_to_column(var = "age") |>
                tidyr::pivot_longer(
                  cols = -age,
                  values_to = "estimate",
                  names_to = "label"
                ) |>
                dplyr::mutate(
                  label = names(extract[[1]][i]),
                  # label_init = names(extract[[1]][i]),
                  fleet = dplyr::case_when(
                    grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(label, paste(fleet_names, collapse = "|")),
                    TRUE ~ NA
                  ),
                  module_name = names(extract),
                  label = dplyr::case_when(
                    grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_replace(label, paste(".", fleet_names, sep = "", collapse = "|"), ""),
                    TRUE ~ label
                  )
                )
              df[setdiff(tolower(names(out_new)), tolower(names(df)))] <- NA
              extract_list[[names(extract[[1]][i])]] <- df
            } else {
              df <- as.data.frame(extract[[1]][[i]]) |>
                tibble::rownames_to_column(var = "year")
              df <- df |>
                dplyr::rename_with(
                  ~ ifelse(max(as.numeric(df$year)) < 50,
                           c("age_a"), c("year")
                  ),
                  year
                )
              if (grepl("lcomp", names(extract[[1]][i]))) {
                namesto <- "len_bins"
              } else if (grepl("acomp", names(extract[[1]][i]))) {
                namesto <- "age"
              } else {
                # Default
                namesto <- "age"
              }
              df2 <- df |>
                tidyr::pivot_longer(
                  cols = -intersect(colnames(df), c("year", "age_a")),
                  names_to = namesto,
                  values_to = "estimate"
                ) |>
                dplyr::mutate(
                  module_name = names(extract),
                  label = names(extract[[1]][i]),
                  fleet = dplyr::case_when(
                    grepl(paste(fleet_names, collapse = "|"), tolower(label)) ~ stringr::str_extract(tolower(label), paste(fleet_names, collapse = "|")),
                    TRUE ~ NA
                  ), # stringr::str_extract(module_name, "(?<=\\.)\\w+(?=\\.)"),
                  label = dplyr::case_when(
                    grepl(paste0(fleet_names, collapse = "|"), tolower(label)) ~ stringr::str_replace(tolower(label), paste0(".", fleet_names, collapse = "|"), ""),
                    TRUE ~ names(extract[[1]][i])
                  )
                  # label_init = names(extract[[1]][i]),
                  
                  # label = dplyr::case_when(
                  #   is.na(fleet) ~ names(extract[[1]][i]),
                  #   TRUE ~ stringr::str_replace(tolower(label), paste(".", fleet_names, sep = "", collapse = "|"), "")
                  # )
                ) # stringr::str_replace(module_name, "\\.[^.]+\\.", "."))
              df2[setdiff(tolower(names(out_new)), tolower(names(df2)))] <- NA
              extract_list[[names(extract[[1]][i])]] <- df2
            } # close if statement
          } # close for loop
          new_df <- Reduce(rbind, extract_list)
          out_list[[names(extract)]] <- new_df
        } else if (any(vapply(extract[[1]], is.vector, FUN.VALUE = logical(1)))) { # all must be a vector to work - so there must be conditions for dfs with a mix
          df <- data.frame(extract[[1]])
          if (length(intersect(colnames(df), c(factors, errors))) > 0) {
            df2 <- df |>
              tidyr::pivot_longer(
                cols = -intersect(colnames(df), c(factors, errors)),
                names_to = "label",
                values_to = "estimate"
              ) |>
              dplyr::mutate(module_name = names(extract))
          } else {
            df2 <- df |>
              tidyr::pivot_longer(
                cols = tidyselect::everything(),
                names_to = "label",
                values_to = "estimate"
              ) |>
              dplyr::mutate(module_name = names(extract))
          }
          if (any(grepl(paste(fleet_names, collapse = "|"), unique(df2$label)))) {
            df2 <- df2 |>
              dplyr::mutate(
                fleet = dplyr::case_when(
                  grepl(paste(fleet_names, collapse = "|"), tolower(label)) ~ stringr::str_extract(tolower(label), paste(fleet_names, collapse = "|")),
                  # grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(ex, paste(fleet_names,collapse="|")),
                  TRUE ~ NA
                ),
                # Number after fleet name is what? variable among df?
                age = dplyr::case_when(
                  grepl("r0$|b0$", tolower(label)) ~ NA,
                  module_name != "parms" & grepl(".Age[0-9]+.", label) ~ stringr::str_extract(label, "(?<=.Age?)[0-9]+"),
                  module_name != "parms" & grepl("[0-9]+$", label) & as.numeric(stringr::str_extract(label, "[0-9]+$")) < 30 ~ stringr::str_extract(label, "[0-9]+$"),
                  module_name != "parms" & grepl("_age[0-9]+.", label) ~ stringr::str_extract(label, "(?<=_age?)[0-9]+"),
                  TRUE ~ NA
                ),
                label = dplyr::case_when(
                  # below will only work properly if there are age varying parameters without fleet names in it
                  module_name == "parms" & !grepl(paste(".", fleet_names, sep = "", collapse = "|"), tolower(label)) ~ label,
                  module_name == "parm.cons" & !grepl(paste("_", fleet_names, sep = "", collapse = "|"), tolower(label)) ~ label,
                  grepl(paste(".", fleet_names, "d[0-9]+", sep = "", collapse = "|"), tolower(label)) ~ stringr::str_replace(tolower(label), paste(".", fleet_names, "d[0-9]+", sep = "", collapse = "|"), ".d"),
                  grepl(paste(".", fleet_names, "[0-9]+$", sep = "", collapse = "|"), tolower(label)) ~ stringr::str_replace(tolower(label), paste(fleet_names, sep = "", collapse = "|"), ""), # "[0-9]+",
                  grepl(paste(".", fleet_names, "$", sep = "", collapse = "|"), tolower(label)) ~ stringr::str_replace(tolower(label), paste(".", fleet_names, sep = "", collapse = "|"), ""),
                  grepl(paste("_", fleet_names, "_", sep = "", collapse = "|"), tolower(label)) ~ stringr::str_replace(tolower(label), paste("_", fleet_names, sep = "", collapse = "|"), ""),
                  grepl(paste(".", fleet_names, ".d$", sep = "", collapse = "|"), tolower(label)) ~ stringr::str_replace(tolower(label), paste(".", fleet_names, sep = "", collapse = "|"), ""),
                  grepl(".Age[0-9]+.[a-z]+", label) ~ stringr::str_replace(label, ".Age[0-9]+.[a-z]+", ""),
                  grepl("[0-9]+$", label) ~ stringr::str_replace(label, "[0-9]+$", ""),
                  # !is.na(fleet) | !is.na(age) ~ stringr::str_replace(label, paste(c(paste(".", fleet_names, "[0-9]+", sep = ""), ".Age[0-9]+.[a-z]+", "[0-9]+$"), collapse = "|"), ""),
                  # as.numeric(stringr::str_extract(label, "[0-9]+$")) == 0 ~ label,
                  # as.numeric(stringr::str_extract(label, "[0-9]+$")) < 30 ~ stringr::str_remove(label, "[0-9]+$"),
                  TRUE ~ label
                ),
                # additional manipulation of label to remove age in parm.cons
                label = dplyr::case_when(
                  module_name == "parm.cons" & grepl("_age[0-9]+.[a-z]+", label) ~ stringr::str_replace(label, "_age[0-9]+.[a-z]+", ""),
                  TRUE ~ label
                )
              )
          } else if (any(grepl("[0-9]$", unique(df2$label)))) {
            df2 <- df2 |>
              dplyr::mutate(
                fleet = NA,
                # Number after fleet name is what? variable among df?
                age = dplyr::case_when(
                  grepl("[0-9]+$", label) & stringr::str_extract(label, "[0-9]+$") < 30 ~ stringr::str_extract(label, "[0-9]+$"),
                  TRUE ~ NA
                ),
                # label_init = label,
                label = dplyr::case_when(
                  as.numeric(stringr::str_extract(label, "[0-9]+$")) == 0 ~ label,
                  as.numeric(stringr::str_extract(label, "[0-9]+$")) < 30 ~ stringr::str_remove(label, "[0-9]+$"),
                  TRUE ~ label
                )
              )
          } else {
            df2 <- df2 |>
              dplyr::mutate(
                fleet = ifelse("age" %in% colnames(df2), fleet, NA),
                age = ifelse("age" %in% colnames(df2), age, NA),
                label = label,
                module_name = names(extract)
              )
          }
          df2[setdiff(tolower(names(out_new)), tolower(names(df2)))] <- NA
          out_list[[names(extract)]] <- df2
        } else {
          cli::cli_alert_warning("Not compatible.")
        }
      } else { # vector only
        df <- as.data.frame(extract) |>
          tibble::rownames_to_column(var = "label") |>
          tidyr::pivot_longer(
            cols = -label,
            names_to = "module_name",
            values_to = "estimate"
          )
        if (any(grepl("age", colnames(df)))) {
          df <- df |>
            tidyr::pivot_longer(
              cols = -intersect(c(factors, errors), colnames(df)),
              names_to = "age",
              values_to = "estimate"
            ) |>
            dplyr::mutate(
              label = names(extract[[1]]),
              age = dplyr::case_when(
                grepl(".[0-9]$", age) ~ stringr::str_extract(age, "[0-9]"),
                grepl(".[0-9][0-9]$", age) ~ stringr::str_extract(age, "[0-9][0-9]"),
                TRUE ~ NA
              ),
              module_name = names(extract)
            )
        }
        if (any(grepl(paste(fleet_names, collapse = "|"), df$label))) {
          df <- df |>
            dplyr::mutate(
              fleet = dplyr::case_when(
                grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(label, paste(fleet_names, collapse = "|")),
                TRUE ~ NA
              ),
              label = dplyr::case_when(
                is.na(fleet) ~ label,
                TRUE ~ stringr::str_replace(label, paste(".", fleet_names, sep = "", collapse = "|"), "")
              )
            )
        }
        df[setdiff(tolower(names(out_new)), tolower(names(df)))] <- NA
        out_list[[names(extract)]] <- df
      }
    } else if (is.list(extract[[1]])) { # list only
      if (any(vapply(extract[[1]], is.matrix, FUN.VALUE = logical(1)))) {
        extract_list <- list()
        for (i in seq_along(extract[[1]])) {
          if (is.vector(extract[[1]][[i]])) {
            df <- as.data.frame(extract[[1]][i]) |>
              tibble::rownames_to_column(var = "age") |>
              tidyr::pivot_longer(
                cols = -age,
                values_to = "estimate",
                names_to = "label"
              ) |>
              dplyr::mutate(
                label = names(extract[[1]][i]),
                # label_init = names(extract[[1]][i]),
                fleet = dplyr::case_when(
                  grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(label, paste(fleet_names, collapse = "|")),
                  TRUE ~ NA
                ),
                module_name = names(extract),
                label = dplyr::case_when(
                  grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_replace(label, paste(".", fleet_names, sep = "", collapse = "|"), ""),
                  TRUE ~ label
                )
              )
            df[setdiff(tolower(names(out_new)), tolower(names(df)))] <- NA
            extract_list[[names(extract[[1]][i])]] <- df
          } else {
            df <- as.data.frame(extract[[1]][[i]]) |>
              tibble::rownames_to_column(var = "year")
            if (grepl("lcomp", names(extract[[1]][i]))) {
              namesto <- "len_bins"
            } else if (grepl("acomp", names(extract[[1]][i]))) {
              namesto <- "age"
            } else {
              # Default
              namesto <- "age"
            }
            df2 <- df |>
              tidyr::pivot_longer(
                cols = -year,
                names_to = namesto,
                values_to = "estimate"
              ) |>
              dplyr::mutate(
                module_name = names(extract),
                label = names(extract[[1]][i]),
                # label_init = names(extract[[1]][i]),
                fleet = dplyr::case_when(
                  grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(label, paste(fleet_names, collapse = "|")),
                  TRUE ~ NA
                ), # stringr::str_extract(module_name, "(?<=\\.)\\w+(?=\\.)"),
                label = dplyr::case_when(
                  is.na(fleet) ~ names(extract[[1]][i]),
                  TRUE ~ stringr::str_replace(label, paste(".", fleet_names, sep = "", collapse = "|"), "")
                )
              ) # stringr::str_replace(module_name, "\\.[^.]+\\.", "."))
            df2[setdiff(tolower(names(out_new)), tolower(names(df2)))] <- NA
            extract_list[[names(extract[[1]][i])]] <- df2
          } # close if statement
        } # close for loop
        new_df <- Reduce(rbind, extract_list)
        out_list[[names(extract)]] <- new_df
      } else if (any(sapply(extract[[1]], is.vector))) { # all must be a vector to work - so there must be conditions for dfs with a mix
        df <- data.frame(extract[[1]])
        if (max(as.numeric(row.names(df))) < 1000) {
          fac <- "age"
        } else if (1000 < max(as.numeric(row.names(df))) & max(as.numeric(row.names(df))) < 2100) {
          fac <- "year"
        } else {
          fac <- "nsim"
        }
        if (any(colnames(df) %in% c("age", "year"))) {
          df <- df
        } else {
          if (fac == "year") {
            df <- tibble::rowid_to_column(df, var = fac) |>
              dplyr::mutate(year = as.character(year))
          } else if (fac == "age") {
            df <- tibble::rowid_to_column(df, var = fac) |>
              dplyr::mutate(age = as.character(age))
          } else if (fac == "nsim") {
            df <- tibble::rowid_to_column(df, var = fac) |>
              dplyr::mutate(nsim = as.character(nsim))
          } else {
            cli::cli_alert_warning("Not compatible")
          }
        }
        # # Check if any col names contain error then select
        # if (any(grepl(paste0(errors, collapse = "|"), colnames(df)))) {
        #   error_cols <- grep(paste0(errors, ".", collapse = "|"), colnames(df))
        # } else {
        #   error_cols <- NULL
        # }
        if (length(intersect(colnames(df), c(factors, errors))) > 0) {
          df2 <- df |>
            tidyr::pivot_longer(
              cols = -intersect(colnames(df), c(factors, errors)),
              names_to = "label",
              values_to = "estimate"
            ) |>
            dplyr::mutate(module_name = names(extract))
        } else {
          df2 <- df |>
            tidyr::pivot_longer(
              cols = tidyselect::everything(),
              names_to = "label",
              values_to = "estimate"
            ) |>
            dplyr::mutate(module_name = names(extract))
        }
        if (any(grepl(paste(fleet_names, collapse = "|"), tolower(unique(df2$label))))) {
          if ("age" %in% colnames(df2)) {
            df2 <- df2
          } else {
            df2 <- df2 |>
              dplyr::mutate(age = NA)
          }
          df2 <- df2 |>
            dplyr::mutate(
              label = tolower(label),
              fleet = dplyr::case_when(
                grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(label, paste(fleet_names, collapse = "|")),
                # grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(ex, paste(fleet_names,collapse="|")),
                TRUE ~ NA
              ),
              # Number after fleet name is what? variable among df?
              age = dplyr::case_when(
                is.na(as.numeric(age)) & grepl("_age[0-9]+_", label) ~ as.numeric(stringr::str_extract(label, "(?<=age:?)[0-9]+")),
                is.na(as.numeric(age)) & grepl("[0-9]+$", label) ~ as.numeric(stringr::str_extract(label, "[0-9]+$")), # this is not age
                TRUE ~ as.numeric(age)
              ),
              # area = dplyr::case_when(is.na(age) & grepl("[0-9]+$", label) ~ stringr::str_extract(label, "[0-9]+$"), # this is not age
              #                         TRUE ~ NA),
              # Uncomment when units gets added in
              # units = dplyr::case_when(
              #   grepl(paste(units, collapse = "|"), label) ~ stringr::str_extract(label, paste(units, collapse = "|")),
              #   TRUE ~ NA
              # ),
              label = dplyr::case_when(
                # grepl("_age[0-9]_", label) & grepl(paste("_", fleet_names, "[0-9]+", sep = "", collapse = "|"), label) ~ stringr::str_replace(label, paste("_age[0-9]+_", fleet_names, "[0-9]+", sep = "", collapse = "|"), ""),
                # grepl("_age[0-9]_", label) & grepl(paste("_", fleet_names, sep = "", collapse = "|"), label) ~ stringr::str_replace(label, paste("_age[0-9]+_", fleet_names, sep = "", collapse = "|"), ""),
                # grepl("_age[0-9]_", label) ~ stringr::str_replace(label, "_age[0-9]+_", ""),
                # grepl(paste0(rep(fleet_names, times = length(units)), ".", units, collapse = "|"), label) ~ stringr::str_replace(label, paste0(".", rep(fleet_names, times = length(units)), ".", units, collapse = "|"), ""),
                grepl(paste("_", fleet_names, "[0-9]+$", sep = "", collapse = "|"), label) ~ stringr::str_replace(label, paste("_", fleet_names, "[0-9]+$", sep = "", collapse = "|"), ""),
                grepl(paste("_", fleet_names, sep = "", collapse = "|"), label) ~ stringr::str_replace(label, paste("_", fleet_names, sep = "", collapse = "|"), ""),
                grepl(paste(".", fleet_names, "[0-9]+$", sep = "", collapse = "|"), label) ~ stringr::str_replace(label, paste(fleet_names, sep = "", collapse = "|"), ""),
                grepl(paste(fleet_names, ".", sep = "", collapse = "|"), label) ~ stringr::str_replace(label, paste(fleet_names, ".", sep = "", collapse = "|"), ""),
                grepl(paste(".", fleet_names, "d[0-9]+$", sep = "", collapse = "|"), label) ~ stringr::str_replace(label, paste(".", fleet_names, "d[0-9]+$", sep = "", collapse = "|"), ""),
                grepl(paste(".", fleet_names, sep = "", collapse = "|"), label) ~ stringr::str_replace(label, paste(".", fleet_names, sep = "", collapse = "|"), ""),
                grepl(paste(fleet_names, "[0-9]+$", collapse = "|"), label) ~ stringr::str_replace(label, "[0-9]+$", ""),
                TRUE ~ label
              )
            )
        } else if (any(grepl("[0-9]$", unique(df2$label)))) {
          df2 <- df2 |>
            dplyr::mutate(
              fleet = NA,
              # Number after fleet name is what? variable among df?
              age = dplyr::case_when(
                grepl("[0-9]+$", label) & stringr::str_extract(label, "[0-9]+$") < 30 ~ stringr::str_extract(label, "[0-9]+$"),
                TRUE ~ NA
              ),
              # label_init = label,
              label = dplyr::case_when(
                as.numeric(stringr::str_extract(label, "[0-9]+$")) == 0 ~ label,
                # issue here that stocks with ages > 29 will not be extracted - maybe add condition if F, t or other preceeeds the number then don't extract?
                as.numeric(stringr::str_extract(label, "[0-9]+$")) < 30 ~ stringr::str_remove(label, "[0-9]+$"),
                TRUE ~ label
              )
            )
        } else {
          df2 <- df2 |>
            dplyr::mutate(
              fleet = ifelse(any(colnames(df2) %in% c("fleet")), fleet, NA),
              age = ifelse(any(colnames(df2) %in% c("age")), fleet, NA),
              label = label,
              module_name = names(extract)
            )
        }
        # Move error into column
        if (any(grepl(paste0("^", errors, "\\.", collapse = "|"), unique(df2$label)))) {
          # Pivot wider only the cv labels and align with their counter part estimate label
          error_data <- df2 |>
            dplyr::filter(stringr::str_detect(label, paste0("^", errors, ".", collapse = "|"))) |>
            dplyr::mutate(match_key = stringr::str_remove(label, paste0("^", errors, ".", collapse = "|"))) |>
            dplyr::mutate(uncertainty_label = stringr::str_extract(label, paste0(errors, collapse = "|"))) |>
            dplyr::select(year, fleet, match_key, uncertainty = estimate, uncertainty_label)
          # filter out error labels
          df2_filtered <- df2 |>
            dplyr::filter(!stringr::str_detect(label, paste0("^", errors, collapse = "|"))) |>
            dplyr::mutate(match_key = stringr::str_extract(label, "^[A-Za-z]+"))
          # Join the errors to the final df
          df2 <- df2_filtered |>
            dplyr::left_join(error_data, by = c("year", "fleet", "match_key")) |>
            dplyr::select(-match_key)
        }
        df2[setdiff(tolower(names(out_new)), tolower(names(df2)))] <- NA
        out_list[[names(extract)]] <- df2
      } else {
        cli::cli_alert_warning("Not compatible.")
      }
    } else if (is.matrix(extract[[1]])) { # matrix only
      df <- as.data.frame(extract[[1]]) |>
        tibble::rownames_to_column(var = "year")
      if (grepl("length", names(extract))) {
        namesto <- "len_bins"
      } else if (grepl("age", names(extract))) {
        namesto <- "age"
      } else {
        # Default
        namesto <- "age"
      }
      df2 <- df |>
        tidyr::pivot_longer(
          cols = -year,
          names_to = namesto,
          values_to = "estimate"
        ) |>
        dplyr::mutate(
          module_name = names(extract),
          label = names(extract),
          # label_init = names(extract[[1]][i]),
          fleet = dplyr::case_when(
            grepl(paste(fleet_names, collapse = "|"), label) ~ stringr::str_extract(label, paste(fleet_names, collapse = "|")),
            TRUE ~ NA
          ), # stringr::str_extract(module_name, "(?<=\\.)\\w+(?=\\.)"),
          label = dplyr::case_when(
            is.na(fleet) ~ names(extract),
            TRUE ~ stringr::str_replace(label, paste(".", fleet_names, sep = "", collapse = "|"), "")
          )
        ) # stringr::str_replace(module_name, "\\.[^.]+\\.", "."))
      df2[setdiff(tolower(names(out_new)), tolower(names(df2)))] <- NA
      out_list[[names(extract)]] <- df2
    } else {
      cli::cli_alert_warning(paste(names(extract), " not compatible.", sep = ""))
    } # close if statement
  } # close loop over objects listed in dat file
  
  # Place out_list into a single data frame
  # VIRG, INIT, TIME, FORE
  out_new <- Reduce(rbind, out_list) |>
    # Add era as factor into BAM conout
    dplyr::mutate(
      # TODO: replace all periods with underscore if naming convention is different
      label = tolower(label)
    )
  # if (model == "bam") {
    out_new <- dplyr::mutate(
      out_new,
      era = dplyr::case_when(
        year < dat$parms$styr ~ "init",
        year >= dat$parms$styr & year <= dat$parms$endyr ~ "time",
        year > dat$parms$endyr ~ "fore",
        TRUE ~ NA
      )
    )
  # }
  return(out_new)
}
