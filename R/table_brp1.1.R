#' Create Biological Reference Point (BRP) Table
#'
#' @param dat A standardized asar_stock object.
#' @param stock_name Character; display name for the stock.
#' @param caption Character; table subtitle/caption.
#' @param current_year Numeric; year of the current assessment.
#' @param previous_year Numeric; year of the previous assessment.
#' @param label Character; internal ID for the table.
#' @param status_f_current Character; optional explicit override for overfishing status.
#' @param status_ssb_current Character; optional explicit override for overfished status.
#'
#' @return A formatted gt table object.
#' @export
table_brp <- function(dat, 
                      stock_name    = attr(dat, "metadata")$spp_name,
                      caption       = attr(dat, "metadata")$cap_brp,
                      current_year  = attr(dat, "metadata")$report_yr,
                      previous_year = attr(dat, "metadata")$last_ass,
                      label         = "BRP_Table",
                      status_f_current = NULL,
                      status_ssb_current = NULL) {
  
  meta <- attr(dat, "metadata")
  sidecar <- attr(dat, "legacy_brp")
  
  is_latex <- !is.null(knitr::opts_knit$get("rmarkdown.pandoc.to")) && 
    knitr::opts_knit$get("rmarkdown.pandoc.to") == "latex"
  
  display_prev_yr <- if(is.null(previous_year) || is.na(previous_year)) "Previous" else as.character(previous_year)
  
  # Helper to find values in the sidecar
  get_val <- function(lab, target_era) {
    if (is.null(sidecar)) return("")
    val <- sidecar |> 
      dplyr::filter(label == lab, era == target_era) |> 
      dplyr::pull(val)
    return(if(length(val) > 0) val[1] else "")
  }
  
  # Standardize status strings
  get_prev_status <- function(val) {
    if (is.null(val) || is.na(val) || val == "" || tolower(val) == "unknown") return("Unknown")
    clean_val <- tolower(val)
    if (grepl("not", clean_val)) {
      return(if(grepl("overfishing", clean_val)) "Not Overfishing" else "Not Overfished")
    }
    if (grepl("overfishing", clean_val)) return("Overfishing")
    if (grepl("overfished", clean_val)) return("Overfished")
    return(val)
  }
  
  # Logical status check with NULL protection
  check_status <- function(type = c("f", "ssb")) {
    type <- match.arg(type)
    manual_arg <- if(type == "f") status_f_current else status_ssb_current
    if (!is.null(manual_arg)) return(manual_arg)
    
    meta_val <- if(type == "f") meta$status_f_now else meta$status_ssb_now
    if (!is.null(meta_val) && !is.na(meta_val) && meta_val != "" && tolower(meta_val) != "unknown") {
      return(get_prev_status(meta_val))
    }
    
    threshold_label <- if(type == "f") "fishing_mortality_threshold" else "biomass_threshold"
    model_label     <- if(type == "f") "fishing_mortality" else "biomass"
    
    thresh_val <- dat |> 
      dplyr::filter(module_name == "reference_points", label == threshold_label, era == "time") |> 
      dplyr::pull(estimate) |> as.numeric()
    
    curr_val <- dat |> 
      dplyr::filter(module_name == "model_results", label == model_label, era == "time") |> 
      dplyr::filter(year == max(year, na.rm = TRUE)) |> 
      dplyr::pull(estimate) |> as.numeric()
    
    if (length(curr_val) == 0 || length(thresh_val) == 0 || is.na(curr_val) || is.na(thresh_val)) return("Unknown")
    
    if (type == "f") {
      return(if(curr_val > thresh_val) "Overfishing" else "Not Overfishing")
    } else {
      return(if(curr_val < thresh_val) "Overfished" else "Not Overfished")
    }
  }
  
  # NULL-safe label cleaning
  safe_clean <- function(x, fallback) {
    input <- if(is.null(x)) fallback else x
    if (is_latex) {
      res <- to_latex_caption(input)
    } else {
      res <- clean_assessment_latex(input)
    }
    return(if (is.null(res) || length(res) == 0) fallback else res)
  }
  
  f_label    <- safe_clean(meta$f_name, "F")
  ssb_label  <- safe_clean(meta$ssb_name, "SSB")
  msy_label  <- safe_clean(meta$msy_name, "MSY")
  
  df <- data.frame(
    Metric   = c(f_label, ssb_label, msy_label, "Overfishing", "Overfished"),
    Previous = c(
      get_val("fishing_mortality_msy", "prev"),
      get_val("biomass_msy", "prev"),
      get_val("msy", "prev"),
      get_prev_status(meta$status_f_old), 
      get_prev_status(meta$status_ssb_old)
    ),
    Current  = c(
      get_val("fishing_mortality_msy", "current"),
      get_val("biomass_msy", "current"),
      get_val("msy", "current"),
      check_status("f"),
      check_status("ssb")
    )
  )
  
  df <- df |> dplyr::filter(!(Metric == "" & Current %in% c("Unknown", "", NA)))
  
  final_table <- df |>
    gt::gt(id = label) |>
    gt::fmt_markdown(columns = Metric) |>
    gt::tab_style(
      style = gt::cell_fill(color = "#CCCCCC"),
      locations = gt::cells_body(columns = Previous)
    ) |>
    gt::cols_label(
      Metric = "", 
      Previous = display_prev_yr,
      Current = as.character(current_year %||% "Current")
    ) |>
    gt::tab_header(
      title = if (is_latex) NULL else stock_name,
      subtitle = if (is_latex) gt::md(to_latex_caption(caption)) else gt::html(clean_assessment_latex(caption))
    ) |>
    gt::tab_style(
      style = gt::cell_text(color = "red", weight = "bold"),
      locations = gt::cells_body(columns = Current, rows = Current %in% c("Overfishing", "Overfished"))
    ) |>
    gt::tab_style(
      style = gt::cell_text(color = "darkgreen", weight = "bold"),
      locations = gt::cells_body(columns = Current, rows = Current %in% c("Not Overfishing", "Not Overfished"))
    ) |>
    gt::cols_align(align = "right", columns = c(Previous, Current)) |>
    gt::opt_row_striping() |>
    gt::tab_options(
      table.font.size = if (is_latex) gt::px(12) else gt::px(14),
      heading.title.font.size = if (is_latex) gt::px(14) else gt::px(16),
      heading.subtitle.font.size = if (is_latex) gt::px(11) else gt::px(13),
      # Explicitly set width to NULL for LaTeX to prevent \linewidth
      table.width = if (is_latex) NULL else gt::pct(100),
      #Can't get this stupid table to NOT use all the horizontal space
      #Gemini suggests this fix (it messes up other stuff): 
      #table.width = if (is_latex) gt::px(1) else gt::pct(100),
      # Use this instead to keep the table compact
      table.layout = "auto",
      # Ensure data rows aren't overly tall
      data_row.padding = gt::px(4),
      column_labels.font.weight = "bold"
    ) |>
    # This is one way to try to get the horizontal spacing right, but the current 
    #version of gt we have loaded does not support it.
    # gt::tab_options(
    #   table.font.size = if (is_latex) gt::px(12) else gt::px(14),
    #   heading.title.font.size = if (is_latex) gt::px(14) else gt::px(16),
    #   heading.subtitle.font.size = if (is_latex) gt::px(11) else gt::px(13),
    #   
    #   # Force standard tabular instead of tabular*
    #   table.width = NULL,
    #   table.layout = "auto",
    #   
    #   # This tells gt to stop trying to be smart with LaTeX width
    #   latex.use_tabular = TRUE,
    #   
    #   data_row.padding = gt::px(4),
    #   column_labels.font.weight = "bold"
    # ) |> 
    gt::tab_style(
      style = gt::cell_text(align = "left"),
      locations = list(gt::cells_title(groups = "title"), gt::cells_title(groups = "subtitle"))
    ) 
  
  return(final_table)
}