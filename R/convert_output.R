#' Convert Output
#'
#' Format stock assessment output file to a standardized tibble.
#'
#' @param files List of Paths. Assessment model output file paths. Currently accepts SS3
#' `r4ss::ss_output()` or Report.sso file (being deprecated), BAM .rda file, Rceattle model object, 
#' and FIMS output (the tibble returned by `FIMS::get_estimates()`, a `FIMSFit` object, or an .rds 
#' file with either). If multiple elements, all valid models will be read, converted, and 
#' appended together.
#' 
#' `names(files)` will be used as the name of the model in the output.
#' 
#' @param models Character vector Assessment model used in evaluation
#'
#' Default: NULL
#'
#' Options: "ss3", "bam", "fims", "rceattle"
#' 
#' Should match length of `files`.
#' 
#' @param fleet_names List of Character vectors. Names of fleets in the assessment model as
#'  shortened in the output file or names of fleets to replace the native fleet
#'  names in the output. For example, if fleet names in the model are 1, 2, 3
#'  then fleet_names = c("fleet1" = 1, "fleet2" = 2, "fleet3" = 3).
#'
#'  Default: NULL
#' 
#' Should match length of `files`.
#' 
#' @param save_dir Path. Full file path to save the converted output file. If
#' saved file name is not indicated, the file will be saved as "std_output.rda"
#' in the file path provided.
#'
#' Default: NULL
#' @param start_year Number. First year of the model, for FIMS output only.
#' FIMS reports years as indices (1 = first model year), which are converted
#' to years when `start_year` is given.
#'
#' Default: NULL (years are left as indices)
#'
#' @author Samantha Bredeck
#'
#' @returns A reformatted and standardized tibble of assessment model results
#'         for application in building stock assessment reports and to easily
#'         adapt results among regional assessments.
#' @details The resulting object is a long-format, transformed and machine readable
#' version of a model output file. Converted data frame is always returned.
#' It will also be saved if save_dir is not NULL.
#'
#' For FIMS, each data point (index, landings, and age and length
#' compositions) is split into an observed row (e.g., "index_observed") and a
#' predicted row (e.g., "index_predicted"), as for the other models. Observed
#' rows hold the observation error (a log-scale standard error for lognormal
#' data) in `uncertainty` and the negative log-likelihood in `likelihood`.
#' Other estimates are relabeled with `fims_var_names.csv` (e.g.,
#' "expected_recruitment" becomes "recruitment").
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' convert_output(
#'   file = here::here("model1", "Report.sso"),
#'   model = "ss3",
#'   fleet_names = c("TWL", "NONTWL"),
#'   save_dir = here::here("standard_output.rda")
#' )
#' convert_output(
#'   file = list("model1"=here::here("model1", "Report.sso", "model2"=here::here("model2", "Report.sso")),
#'   model = c("ss3", "ss3"),
#'   fleet_names = list("model1"=c("TWL", "NONTWL"), "model2"=c("Fishery1", "Survey1", "Survey2")),
#'   save_dir = here::here("standard_output.rda")
#' )
#' }
convert_output <- function(
  files,
  models = NULL,
  fleet_names = NULL,
  save_dir = NULL,
  start_year = NULL
) {

  # Is user inputs a single model file and doesn't wrap it as a list object
  if(any(grepl("Data_File|data_list", names(files))) || is.character(files) || is.data.frame(files)){
    cli::cli_alert("`files` appears to represent a single model. Converting to list object.")
    files <- list(files)
    names(files) <- "model_1"
  }

  if(!all(is.null(models)) & length(models) != length(files)){
    cli::cli_alert_warning("Number of model types entered is different from number of models.")
    models <- c(models, rep(NA, length(files)-length(models))) # can't store NULL in vector so make a string
    cli::cli_alert(glue::glue("Model types are: {paste0(\"'\",models,\"'\", collapse=\",\")}"))
  }

  if(length(fleet_names) != length(files)){
    cli::cli_alert_warning("Number of fleet_names enetered is different from number of models.")
    fleet_names <- c(fleet_names, rep(list(NULL), length(files)-length(fleet_names)))
    names(fleet_names) <- names(files)
    cli::cli_alert(glue::glue("Fleets names are: {str(fleet_names)}"))
  }

  
  processed <- purrr::imap(files, function(file, model_name){
      i <- which(names(files)==model_name)
      if (is.character(file) && grepl("Report.sso", file)) {
        lifecycle::deprecate_warn(
          when = "2.8.0", 
          what = I("convert_output(file = 'Report.sso')"), 
          details = c(
            i = "Please use the returned object from `r4ss::ss_output()`",
            i = "File will be compatible until Oct. 1, 2027"
          )
        )
      }
      
      # check if entered save_dir exists so doesn't waste user time finding this out at the end
      if (!is.null(save_dir)) {
        if (!grepl(".rda", save_dir)) {
          cli::cli_alert("save_dir does not contain a file name. Saved output will be named `std_output.rda`.")
          save_dir <- file.path(save_dir, "std_output.rda")
        }
      }

      #### out_new ####
      # Blank dataframe and set up to mold output into
      out_new <- data.frame(
        module_name = NA,
        label = NA,
        time = NA,
        era = NA,
        year = NA,
        month = NA,
        season = NA,
        subseason = NA,
        birthseas = NA,
        initial = NA,
        estimate = NA,
        uncertainty = NA,
        uncertainty_label = NA,
        likelihood = NA,
        fleet = NA,
        platoon = NA,
        area = NA,
        age = NA,
        sex = NA,
        growth_pattern = NA,
        # gradient = NA,
        # estimated = NA, # TRUE/FALSE
        # Additional factors from SS3
        bio_pattern = NA,
        settlement = NA,
        morph = NA,
        beg_mid = NA,
        type = NA,
        factor = NA,
        # sexes = NA, # remove sexes to see how it impacts the results
        part = NA,
        kind = NA,
        nsim = NA,
        # TODO: add age_a and len_bins into just bins - add the age value in age
        bin = NA,
        age_a = NA,
        len_bins = NA,
        count = NA,
        block = NA
        # model_name = model_name
      )
      out_new <- out_new[-1, ]

      # Check if path links to a valid file
      if (is.character(file)) {
        url_pattern <- "^(https?|ftp|file):\\/\\/[-A-Za-z0-9+&@#\\/%?=~_|!:,.;]*[-A-Za-z0-9+&@#\\/%=~_|]$"
        if (grepl(url_pattern, file)) {
          check <- tryCatch(
            {
              headers <- curlGetHeaders(file)
              attributes(headers)[["status"]]
            },
            error = function(e) {
              404
            }
          )
          if (check == 404) cli::cli_abort(c(message = "Invalid URL."))
        } else {
          if (!file.exists(file)) {
            cli::cli_abort(c(
              message = "`file` not found.",
              "i" = "`file` entered as {file}"
            ))
          }
        }
      }

      # Recognize model through file extension
      if (is.character(file) && tolower(stringr::str_extract(file, "\\.([^.]+)$")) == ".rds") {
        file <- readRDS(file)
      }
      if (is.character(file)) {
        if (is.null(models)) {
          model <- switch(stringr::str_extract(file, "\\.([^.]+)$"),
            ".sso" = {
              cli::cli_alert_info("Processing Stock Synthesis output file...")
              "ss3"
            },
            ".rdat" = {
              cli::cli_alert_info("Processing BAM output file...")
              "bam"
            },
            # ".rds" = {
            #   cli::cli_alert_info("Processing WHAM output file...")
            #   "wham"
            # },
            # ".RDS" = {
            #   cli::cli_alert_info("Processing FIMS output file...")
            #   "fims"
            # },
            cli::cli_abort("Unknown file type. Please indicate model.")
          )
        }
      } else if (is.null(models)){
        model <- switch(class(file)[1],
          "FIMSFit" = {
            cli::cli_alert_info("Processing FIMS output file...")
            "fims"
          },
          "fims" = {
            cli::cli_alert_info("Processing FIMS output file...")
            "fims"
          },
          "Rceattle" = {
            cli::cli_alert_info("Processing Rceattle output file...")
            "rceattle"
          },
          "list" = {
            if (any(grepl("wham_version", names(file)))) {
              cli::cli_alert_info("Processing WHAM output file...")
              "wham"
            } else {
              cli::cli_alert_info("Processing SS3 r4ss output file...")
              "ss3"
            }
          },
          cli::cli_abort("Unknown file type. Please indicate model.")
        )
      }else{
        model <- models[i]
      }

      #### SS3 ####
      # SS3 groupings: https://docs.google.com/spreadsheets/d/11-k0xOo3Bit1MbJf2ZjmymzsUNCRhgGkVfXV1jNEdbM/edit?gid=0#gid=0
      # Convert SS3 output Report.sso file
      if (tolower(model) == "ss3") {
        out_new <- conout_ss3(file = file, out_new = out_new, fleet_names = fleet_names)
      } else if (model %in% c("bam", "BAM")) {
        #### BAM ####
        out_new <- conout_bam(file = file, out_new = out_new, fleet_names = fleet_names)
        #### WHAM ----
      } else if (model == "wham") {
        # This is how Bai read the ASAP output
        # asap_output conversion written by Bai Li
        # asap_output <- dget(file.path(casedir, "output", subdir, paste("s", keep_sim_id[om_sim], sep = ""), "asap3.rdat"))
        # setwd(file.path(casedir, "output", subdir, paste("s", keep_sim_id[om_sim], sep = "")))
        # asap_std <- readRep("asap3", suffix = ".std")
        cli::cli_abort("WHAM output not currently compatible.")
        #### AMAK ####
      } else if (model == "amak") {
        cli::cli_abort("AMAK output not currently compatible.")
        #### JABBA ####
      } else if (tolower(model) == "jabba") {
        cli::cli_abort("JABBA output not currently compatible.")
        #### FIMS ####
      } else if (model == "fims") {
        out_new <- conout_fims(file = file, out_new = out_new, start_year = start_year)
        #### Rceattle ####
      } else if (model == "rceattle") {
      out_new <- conout_rceattle(file = file, out_new = out_new, fleet_names = fleet_names)
      } else {
        cli::cli_abort(c(
          message = "Output file not compatible.",
          "i" = "`model` entered as {model}.",
          "i" = "Accepted `model` options: ss3, bam, or fims."
        ))
      }

      # Call in csv for model
      con_file <- system.file("resources", glue::glue("{tolower(model)}_var_names.csv"), package = "stockplotr", mustWork = TRUE)
      # temporarily add call to local csv so I can test
      # con_file <- glue::glue("~/GitHub/stockplotr/inst/resources/{model}_var_names.csv")
      var_names_sheet <- utils::read.csv(con_file, na.strings = "")

      if (tolower(model) == "bam") {
          var_names_sheet <- var_names_sheet |>
          dplyr::mutate(label = tolower(label))
      }

      if (file.exists(con_file)) {
          # Remove 'X' column if it exists
          var_names_sheet <- var_names_sheet |>
          dplyr::select(-dplyr::any_of("X"))
          if (tolower(model) == "ss3" & !is.character(file)) {
          var_names_sheet <- var_names_sheet |> dplyr::select(-module_name)
          out_new <- dplyr::left_join(out_new, var_names_sheet, by = "label")
          } else {
          var_names_sheet <- var_names_sheet |>
              dplyr::distinct(module_name, label, .keep_all = TRUE)
          out_new <- dplyr::left_join(
              out_new,
              var_names_sheet,
              by = c("module_name", "label"),
              relationship = "many-to-one"
          )
          }
          out_new <- out_new |>
          dplyr::mutate(label = dplyr::case_when(
              !is.na(alt_label) ~ alt_label,
              # TODO: add this to ss3_var_names.xlsx
              label == "dev" & module_name == "SPAWN_RECRUIT" ~ "recruitment_deviation",
              # TODO: add this to ss3_var_names.xlsx
              label == "dev" & module_name == "SPAWN_RECRUIT_CURVE" ~ "recruitment_deviation",
              # TODO: add this to ss3_var_names.xlsx
              label == "dev" & module_name == "DISCARD_OUTPUT" ~ "discard_deviation",
              TRUE ~ label
          )) |>
          dplyr::select(-alt_label) |>
          dplyr::rename(length_bins = len_bins)
      }


      out_new <- out_new |> 
          dplyr::mutate(
              model=model_name,
              year=as.character(year),
              month=as.character(month),
              block=as.character(block),
              fleet=as.character(fleet)
            )
      return(tibble::tibble(out_new))
  })

  out_new <- dplyr::bind_rows(processed) # aggregate all of the models 


  #### Exporting ####
  # Combine DFs into one
  out_new <- out_new |>
    dplyr::mutate(
      estimate = dplyr::case_when(
        is.na(estimate) ~ NA_real_,
        TRUE ~ as.numeric(estimate)
      ),
      uncertainty = dplyr::case_when(
        is.na(uncertainty) ~ NA_real_,
        TRUE ~ as.numeric(uncertainty)
      ),
      initial = dplyr::case_when(
        is.na(initial) ~ NA_real_,
        TRUE ~ as.numeric(initial)
      ),
      year = dplyr::case_when(
        is.na(year) ~ NA_real_,
        TRUE ~ as.numeric(year)
      ),
      likelihood = dplyr::case_when(
        is.na(likelihood) ~ NA_real_,
        TRUE ~ as.numeric(likelihood)
      ),
      # change era name to keep standard
      era = dplyr::case_when(
        era == "Main" ~ "time",
        TRUE ~ tolower(era)
      )
    ) |>
    suppressWarnings()

  cli::cli_alert_success("Conversion finished!")

  # save if indicated
  if (!is.null(save_dir)) {
    # add check if save_dir does not end in .rda
    if (!grepl("\\.rda$", save_dir)) {
      cli::cli_alert_warning("save_dir does not contain .rda extension.")
      cli::cli_alert_info("Saving file as std_output.rda")
    }
    # save_name <- stringr::str_extract(save_dir, "(?<=/)[^/]+(?=\\.rda$)")
    # assign(save_name, out_new)
    save(out_new, file = save_dir)
  }

  return(out_new)
} # close function
