# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# RDA utility functions
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# TODO: update key quantities functions to work with a specified 'dir' instead of default 'getwd()'?
# TODO: update key quantities functions for plots that need to extract values from KQs csv to calculate their own, dependent KQs (none present in current plotting functions)

#' Substitute key quantities' values into template
#'
#' @param df Dataframe created by importing "key_quantity_template.csv" or
#' "key_quantities.csv", empty or partially-filled templates with key
#' quantity names and other associated information
#' @param ... Key quantity objects whose values will be added to the output
#' dataframe
#'
#' @returns Dataframe based on key quantities template that contains 
#' newly-added values of key quantities indicated via ellipsis. The dataframe
#' is input for [fill_in_kqs()].
#'
#' @examples \dontrun{
#' fill_in_kqs(
#'   df = data,
#'   F.min,
#'   F.max)
#' }
fill_in_kqs <- function(df, ...) {
  
  arg_names <- sapply(substitute(list(...))[-1], deparse)
  arg_values <- list(...)
  
  lookup_df <- tibble::tibble(
    key_quantity = arg_names,
    value_new = purrr::map_chr(arg_values, as.character)
  )
  
  # TODO: Add message when certain values aren't overwritten (already present)
  df <- df |>
    dplyr::mutate(across(everything(), as.character)) |>
    dplyr::left_join(lookup_df, by = "key_quantity") |>
    dplyr::mutate(value = dplyr::if_else(
      (is.na(value) | value == "" & !is.na(value_new)),
      value_new,
      value)) |>
    dplyr::select(-value_new)
}

#' Export updated key quantities template
#'
#' @param ... Key quantities whose values should be added to the
#' exported "key_quantities.csv"
#'
#' @returns Exports a file based on key quantities template, with values
#' added next to the names of the key quantities specified as ellipsis 
#' arguments. File is saved as "key_quantities.csv" to the working directory.
#'
#' @examples \dontrun{
#' export_kqs(
#'   F.min,
#'   F.max)
#' }
export_kqs <- function(...) {
  
  # Open new or existing key quantities csv
  if (file.exists(fs::path(getwd(), "key_quantities.csv"))) {
    cli::cli_alert_info("Key quantities text file (key_quantities.csv) exists. Newly calculated key quantities will be added to it.", wrap = TRUE)
    kqs <- utils::read.csv(file.path(getwd(), "key_quantities.csv"))
  } else {
    kqs <- utils::read.csv(
      system.file("resources", "key_quantity_template.csv", package = "stockplotr")
    )
  }
  
  # kqs (e.g., landings.end.year) are the ellipsis args
  kqs_filled <- fill_in_kqs(kqs,
                            ...)
  
  utils::write.csv(
    x = kqs_filled,
    file = fs::path(getwd(), "key_quantities.csv"),
    row.names = FALSE
  )
  
}

#' Insert key quantities into the captions and alternative text file
#'
#' @param ... Key quantities whose values should be added to the
#' exported "captions_alt_text.csv"
#'
#' @returns Exports a file ("captions_alt_text.csv") containing captions 
#' and alternative text for figures and tables, with key quantities inserted
#' into the "captions_alt_text_template.csv" template's placeholders.
#'
#' @examples \dontrun{
#' insert_kqs(
#'   F.min,
#'   F.max)
#' }
insert_kqs <- function(...) {
  if (file.exists(fs::path(getwd(), "captions_alt_text.csv"))) {
    cli::cli_alert_info("Captions/alternative text file (captions_alt_text.csv) exists. Newly calculated key quantities will be added to it.", wrap = TRUE)
    caps_alttext <- utils::read.csv(fs::path(getwd(), "captions_alt_text.csv"))
  } else {
    caps_alttext <- utils::read.csv(
      system.file("resources", "captions_alt_text_template.csv", package = "stockplotr")
    )
  }
  
  create_patterns <- function(...) {
    # Capture the names from the ellipsis
    arg_names <- sapply(rlang::enexprs(...), as.character)
    
    # Get the actual values
    vals <- as.character(list(...))
    
    # 1. Escape literal dots (e.g., "B.min" -> "B\\.min")
    # This ensures the dot is treated as a period, not a "match-anything" wildcard.
    escaped_names <- stringr::str_replace_all(arg_names, "\\.", "\\\\.")
    
    # 2. Wrap in lookarounds to enforce "whole word" logic for dots/alphanumerics
    # 2. Refined Lookarounds:
    # (?<![a-zA-Z0-9.]) -> PRECEDER: Not a letter, digit, or dot.
    # (?!([a-zA-Z0-9])) -> FOLLOWER: Not a letter or digit.
    # We REMOVED the dot from the follower check so "caa.age.max." matches.
    patterns <- paste0("(?<![a-zA-Z0-9.])", escaped_names, "(?![a-zA-Z0-9])")
    
    # Combine into a named character vector
    stats::setNames(vals, patterns)
    # # Get the actual values
    # vals <- list(...)
    # 
    # # Combine them into a named character vector
    # stats::setNames(as.character(vals), arg_names)
  }
  
  # insert new kqs into alt text/caps csv, where applicable
  patterns_replacements <- create_patterns(...) |>
  # If a value = NA, then make it "NA" to avoid errors
   tidyr::replace_na("NA")
  
  # replace values in caption column
  caps_alttext$caption <- stringr::str_replace_all(
    caps_alttext$caption,
    patterns_replacements
  )
  
  # replace values in alt text column
  caps_alttext$alt_text <- stringr::str_replace_all(
    caps_alttext$alt_text,
    patterns_replacements
  )
  
  # export df with updated captions and alt text to csv
  utils::write.csv(
    x = caps_alttext,
    file = fs::path(getwd(), "captions_alt_text.csv"),
    row.names = FALSE
  )
  
  # message explaining the extracted and inserted key quantities
  replaced_vals <- patterns_replacements |>
    as.data.frame() |>
    tibble::rownames_to_column() |>
    dplyr::rename(
      "name" = 1,
      "key_quantity" = 2
    ) |>
    # remove the added regex characters
    dplyr::mutate(
      name = stringr::str_remove_all(name, "^\\(\\?\\<!\\[a-zA-Z0-9\\.\\]\\)|\\(\\?\\!\\[a-zA-Z0-9\\]\\)$"),
      name = stringr::str_replace_all(name, "\\\\\\.", ".")
    )
  
  cli::cli_h3("The following key quantities were extracted and inserted into 'captions_alt_text.csv' and 'key_quantities.csv':")
  for (i in 1:dim(replaced_vals)[1]) {
    cli::cli_li(paste0(
      replaced_vals[i, 1],
      ": ",
      replaced_vals[i, 2]
    ))
  }
}


#' Create the rda package for a plot or table
#'
#' @param object Table or plot object
#' @param topic_label A string that names the object
#' @param fig_or_table A string identifying if the object is a "table" or "figure"
#' @param dat Data frame containing data which will fill in captions and
#' alternative text for the object
#' @param dir Directory to where the rda will be saved
#' @param year Default to current year
#' @param ref_line Reference line value such as "msy", "target" or "unfished"
#' @param ref_point Reference point value such as "msy", "target" or "unfished"
#' @param scale_amount A number describing how much to scale down the quantities
#' shown on the y axis. For example, scale_amount = 100 would scale down a value
#' from 500,000 --> 5,000. This scale will be reflected in the y axis label.
#' @param unit_label A string containing a unit label for the y-axis
#' @param table_df The data frame that the table will be made into for purposes
#' of exporting a latex formatted table.
#'
#' @returns Create an rda package for a plot or table object. Requires an
#' object from the R environment such as a ggplot or flextable object.
#' @export
#'
#' @examples \dontrun{
#' create_rda(
#'   object = my_plot,
#'   topic_label = "my_plot",
#'   fig_or_table = "figure",
#'   dat = my_data,
#'   dir = "path/to/save"
#' )
#' }
create_rda <- function(
  object, # REQUIRED: table or plot object to export
  topic_label, # REQUIRED
  fig_or_table, # REQUIRED
  dat, # REQUIRED: only one dat file to base captions and alt text from
  dir = getwd(),
  year = format(as.POSIXct(Sys.Date(), format = "%YYYY-%mm-%dd"), "%Y"),
  ref_line = "msy",
  ref_point = "msy", # this is not used anywhere
  scale_amount = 1,
  unit_label = "mt",
  table_df = NULL
) {

  # extract this plot's caption and alt text
  caps_alttext <- extract_caps_alttext(
    topic_label = topic_label,
    fig_or_table = fig_or_table,
    dir = dir
  )

  if (fig_or_table == "table") {
    latex_table <- create_latex_table(
      data = table_df,
      caption = caps_alttext[1],
      label = "landings_latex"
    )
  }

  export_rda(
    object = object,
    caps_alttext = caps_alttext,
    figures_tables_dir = dir,
    topic_label = topic_label,
    fig_or_table = fig_or_table,
    latex_table = latex_table
  )
}

#------------------------------------------------------------------------------
# NOTE: this code was taken from the former write_captions() function
# Leaving it here to pull from as we develop more figure and table functions
# which will have associated captions/alt text containing key quantities
# with the following object names and calculations

    # FIGURES-----

    ## kobe plot- don't code quantities yet
    # kobe.end.year <-
    # value of B/B(MSY) at the end year
    # B.BMSY.end.yr <- B/BMSY
    # --B = time series of biomass, last year
    # --BMSY = dplyr::filter(grepl('b_target', label) | grepl('b_msy', label) | c(grepl('fishing_mortality_msy', label) & is.na(year)))
    #   CHECK: if length > 1, then select b_target

    # value of F/F(MSY) at the end year
    # F.FMSY.end.yr <-

    # object that should be "is" or "is not" and answers the question,
    # "the stock overfishing status ... overfished"
    # overfished.status.is.isnot <-

    # object that should be "is" or "is not" and answers the question,
    # "the stock ... experiencing overfishing"
    # overfishing.status.is.isnot <-


    # TODO: uncomment and recode once we get clarity about how to extract this value properly
    # R0
    # R0 <- dat |>
    #   dplyr::filter(
    #     # pull from BAM
    #     grepl('^recruitment$', label) & module_name == "parms" |
    #        grepl('^R0', label) |
    #     # pull from SS3
    #       grepl('recruitment_virgin', label)
    #                  ) |>
    #   dplyr::pull(estimate) |>
    #   unique() |>
    # as.numeric() |>
    # round(digits = 2)

    # Bend <-

    # TODO: uncomment and recode once we get clarity about how to extract this value properly
    # Target biomass
    # Btarg <- dat |>
    #   dplyr::filter(c(grepl('biomass', label) & grepl('target', label) & estimate >1) | label == 'biomass_msy') |>
    #   dplyr::pull(estimate) |>
    #   as.numeric() |>
    #   round(digits = 2)

    # Bmsy <-

    ## vonB LAA (von Bertalanffy growth function + length at age)- don't code quantities yet
    # vonb.age.min <- # minimum vonB age
    # vonb.age.max <- # maximum vonB age

    # vonB length units (plural)
    # vonb.length.units 

    # vonb.length.min <- # minimum vonB length
    # vonb.length.max <- # minimum vonB length


    ## length-type conversion plot- don't code quantities yet
    # total length units (plural)
    # total.length.units 

    # total.length.min <- # minimum total length
    # total.length.max <- # maximum total length
    # fork length units (plural)
    # fork.length.units 

    # fork.length.min <- # minimum fork length
    # fork.length.max <- # maximum fork length


    ## weight-length conversion plot- don't code quantities yet
    # length units (plural)
    # wl.length.units 

    # wl.length.min <- # minimum length
    # wl.length.max <- # maximum length

    # weight units (plural)
    # wl.weight.units 

    # wl.weight.min <- # minimum weight
    # wl.weight.max <- # maximum weight


    ## maturity schedule (proportion mature)- don't code quantities yet
    # length units (plural)
    # prop.mat.length.units 

    # prop.mat.length.min <- # minimum length
    # prop.mat.length.max <- # maximum length


    ## fecundity at length- don't code quantities yet
    # length units (plural)
    # fecundity.length.units 

    # fecundity.length.min <- # minimum length
    # fecundity.length.max <- # maximum length

    # fecundity units (plural)
    # fecundity.units 

    # fecundity.min <- # minimum fecundity
    # fecundity.max <- # maximum fecundity

    ## CAL (catch at length)- don't code quantities yet
    # cal.length.min <- # minimum length group
    # cal.length.max <- # maximum length group
    # fleet.or.survey.name <- # fleet or survey name

    ## mod_fit_catch (model fit to catch ts)- don't code quantities yet
    # mod.fit.catch.start.year <- # start year of model fit to catch ts plot
    # mod.fit.catch.end.year <- # end year of model fit to catch ts plot

    # catch units (plural)
    # mod.fit.catch.units 

    # mod.fit.catch.min <- # minimum catch
    # mod.fit.catch.max <- # maximum catch


    ## mod_fit_abun (model fit to abundance indices plot)- don't code quantities yet
    # start year of model fit to abundance indices plot
    # mod.fit.abun.start.year <-

    # end year of model fit to abundance indices plot
    # mod.fit.abun.end.year <-

    ## mod_fit_discards- will be by fleet
    ## for ss3, obs discards not in output file
    ## -filter labels as discard_observed | discard_predicted | discard
    ## -then group_by(year, fleet, label), then summarize(estimate_y = sum(estimate))
    ## ---then, get the following:
    ## -change alt text so that we add the line's min/max, but analyst has to describe further
    # mod.fit.discards.start.year <- # start year of model fit to discards plot
    # mod.fit.discards.end.year <- # end year of F
    # mod.fit.discards.units <- # discards units (plural)
    # mod.fit.discards.min <- # minimum discards
    # mod.fit.discards.max <- # maximum discards

    ## selectivity- don't code quantities yet
    # selectivity.start.year <- # start year of selectivity plot
    # selectivity.end.year <- # end year of selectivity plot
    # selectivity.length.units <- # length units (plural)
    # selectivity.length.min <- # minimum length
    # selectivity.length.max <- # maximum length

    ## tot_b (total biomass): same as B plot above

    # ssbtarg 

    ## spr (spawning potential ratio)
    # minimum spr
    # spr.min <- dat |>
    #   dplyr::filter(c(grepl("spr", label) |
    #     label == "spr") &
    #     !is.na(year) &
    #     !is.na(estimate)) |>
    #   dplyr::slice(which.min(estimate)) |>
    #   dplyr::select(estimate) |>
    #   as.numeric() |>
    #   round(digits = 2)
    # 
    # # maximum spr
    # spr.max <- dat |>
    #   dplyr::filter(c(grepl("spr", label) |
    #     label == "spr") & !is.na(year) & !is.na(estimate)) |>
    #   dplyr::slice(which.max(estimate)) |>
    #   dplyr::select(estimate) |>
    #   as.numeric() |>
    #   round(digits = 2)

    # TODO: uncomment and recode once we get clarity about how to extract this value properly
    # spr reference point
    # spr.ref.pt <- dat |>
    # dplyr::filter(label == "spr_msy") |>
    #   dplyr::select(estimate) |>
    #   as.numeric()# |>
    #   round(digits = 2)


    ## proj_catch (projected catch)
    # projected catch units (plural)
    # proj.catch.units <- # probably mt, but wait until figure coded

    # start year of projected catch plot
    # proj.catch.start.year <- landings.end.year + 1

    # end year of projected catch plot
    # proj.catch.end.year <- dat |>
    #   dplyr::filter(label == "catch" & module_name == "DERIVED_QUANTITIES") |>
    #   dplyr::slice(which.max(year)) |>
    #   dplyr::select(year) |>
    #   as.numeric()

    # minimum projected catch
    # proj.catch.min <- dat |>
    #   # no BAM file has catch; will be NA
    #   dplyr::filter(label == "catch" & module_name == "DERIVED_QUANTITIES") |>
    #   dplyr::slice(which.min(estimate)) |>
    #   dplyr::select(estimate) |>
    #   as.numeric()

    # maximum projected catch
    # proj.catch.max <- dat |>
    #   dplyr::filter(label == "catch" & module_name == "DERIVED_QUANTITIES") |>
    #   dplyr::slice(which.max(estimate)) |>
    #   dplyr::select(estimate) |>
    #   as.numeric()

    # TABLES-----

    ## catch
    # catch.fleet <- # fleet

    ## discards
    # discards.tbl.units <- # discards units

    ## catchability
    # catchability.fleet <- # fleet


#------------------------------------------------------------------------------

#' Extract captions and alternative texts
#'
#' Extract a figure or table's caption and alternative text for usage when
#' generating a figure or table. Typically used before stockplotr::export_rda().
#'
#' @param topic_label A string that describes a figure or table's label. These
#' labels are found in the "label" column of the "captions_alt_text.csv" file
#' and are used to link the figure or table with its caption/alt text.
#' @param fig_or_table A string describing whether the plot is a figure or table.
#' @param dir The directory containing the "captions_alt_text.csv" file.
#'
#' @return A figure's caption and alternative text, in a list, or a table's caption.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' extract_caps_alttext(
#'   topic_label = "biomass",
#'   fig_or_table = "figure",
#'   dir = here::here()
#' )
#'
#' extract_caps_alttext(
#'   topic_label = "bnc",
#'   fig_or_table = "table",
#'   dir = getwd()
#' )
#' }
extract_caps_alttext <- function(topic_label = NULL,
                                 fig_or_table = NULL,
                                 dir = getwd()) {
  # import csv with captions and alt text
  captions_alttext_df <- utils::read.csv(
    fs::path(dir, "captions_alt_text.csv")
  )

  # extract plot or table's caption and alt text
  caption <- captions_alttext_df |>
    dplyr::filter(
      label == topic_label,
      type == fig_or_table
    ) |>
    dplyr::select(caption) |>
    as.character()

  if (fig_or_table == "figure") {
    alt_text <- captions_alttext_df |>
      dplyr::filter(
        label == topic_label,
        type == "figure"
      ) |>
      dplyr::select(alt_text) |>
      as.character()

    caps_alttext_list <- list(
      caption,
      alt_text
    )
  } else {
    caps_alttext_list <- list(caption)
  }

  return(caps_alttext_list)
}

#------------------------------------------------------------------------------

#' Export a figure or table to rda
#'
#' Export a figure/table, and its caption and alternative text, to an rda object.
#' Typically used after stockplotr::extract_caps_alttext().
#'
#' @param object The final figure (ggplot) or table (flextable) object.
#' @param caps_alttext The object containing a figure's caption and alternative
#' text, in a list, or a table's caption, likely generated with
#' stockplotr::extract_caps_alttext().
#' @param figures_tables_dir If the user has already created folders containing
#' figures and tables ("figures" and "tables"), figures_tables_dir represents
#' the location of these folders. Otherwise, these two folders will be created
#' automatically, then used to store the exported rda files.
#' @param topic_label A string that describes a figure or table's label. These
#' labels are found in the "label" column of the "captions_alt_text.csv" file
#' and are used to link the figure or table with its caption/alt text.
#' @param fig_or_table A string describing whether the plot is a figure or table.
#' @param latex_table The object containing a LaTeX-based table.
#'
#' @return An rda file with a figure's ggplot, caption, and alternative text, or
#' a table's flextable and caption.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' export_rda(
#'   final = final_table_object,
#'   caps_alttext = caps_alttext_object,
#'   figures_tables_dir = here::here(),
#'   topic_label = "bnc",
#'   fig_or_table = "table",
#'   latex_table = "latex_table"
#' )
#'
#' export_rda(
#'   final = final_figure_object,
#'   caps_alttext = another_caps_alttext_object,
#'   figures_tables_dir = "my_figures_tables_dir",
#'   topic_label = "landings",
#'   fig_or_table = "figure"
#' )
#' }
export_rda <- function(object = NULL,
                       caps_alttext = NULL,
                       figures_tables_dir = NULL,
                       topic_label = NULL,
                       fig_or_table = NULL,
                       latex_table = NULL) {
  # make rda for figures
  if (fig_or_table == "figure") {
    rda <- list(
      "figure" = object,
      "caption" = caps_alttext[[1]],
      "alt_text" = caps_alttext[[2]]
    )
    rda_loc <- "figures"

    # check if a figures folder already exists; if not, make one
    if (!dir.exists(fs::path(figures_tables_dir, rda_loc))) {
      dir.create(fs::path(figures_tables_dir, rda_loc))
      cli::cli_alert_success("New {rda_loc} folder created in {fs::path(figures_tables_dir)}.")
    }

    # make rda for tables
  } else if (fig_or_table == "table") {
    rda <- list(
      "table" = object,
      "caption" = caps_alttext[[1]],
      "latex_table" = latex_table
    )
    rda_loc <- "tables"
    # check if a tables folder already exists; if not, make one
    if (!dir.exists(fs::path(figures_tables_dir, rda_loc))) {
      dir.create(fs::path(figures_tables_dir, rda_loc))
      cli::cli_alert_success("New {rda_loc} folder created in {fs::path(figures_tables_dir)}.")
    }
  }
  output_file_name <- paste0(topic_label, "_", fig_or_table, ".rda")

  # check if rda is already present. If so, check it should be overwritten
  if (file.exists(fs::path(
    figures_tables_dir,
    rda_loc,
    output_file_name
  ))) {
    question1 <- readline(
      paste0(
        "The ",
        output_file_name,
        " already exists within ",
        fs::path(figures_tables_dir, rda_loc),
        ". Would you like to overwrite this file? (Y/N)"
      )
    )

    if (regexpr(question1, "y", ignore.case = TRUE) == 1) {
      # export rda
      save(rda,
        file = fs::path(
          figures_tables_dir,
          rda_loc,
          output_file_name
        )
      )
      cli::cli_alert_success("{output_file_name} was regenerated and overwrote the previous version.",
        wrap = TRUE
      )
    } else {
      cli::cli_alert_warning("{output_file_name} was not regenerated.")
    }
  } else {
    cli::cli_alert_info("{output_file_name} will be newly created.")

    # export rda
    save(rda,
      file = fs::path(
        figures_tables_dir,
        rda_loc,
        output_file_name
      )
    )

    cli::cli_alert_success("{output_file_name} was exported.")
  }
}


# values within dat$year that are excluded from multiple plotting datasets
year_exclusions <- c("S/Rcurve", "Init", "Virg")
