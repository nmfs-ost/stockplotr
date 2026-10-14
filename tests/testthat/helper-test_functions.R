#' Check for the existence and remove .rda files from test environment
#' 
#' Checks for the existence of `dir` and `filename` as well as the
#' existence of "captions_alt_text.csv" and "key_quantities.csv". 
#' Files are removed upon completion of checks. 
#' 
#' For testing only.
#' 
#' @param filename String. filename of figure/table .rda file
#' @param dir String. directory where filename exists
#' 
check_and_remove_tmp_files <- function(filename, dir=fs::path(getwd())){
    # expect that both figures dir and the abundance_at_age_figure.rda file exist
    expect_true(dir.exists(dir)) # this check needs to exist in case save dir isnt getwd()
    expect_true(dir.exists(fs::path(dir, "figures")))
    expect_true(file.exists(fs::path(dir, "figures", filename)))

    # erase temporary testing files
    captions_file <- fs::path(dir, "captions_alt_text.csv")
    keyquant_file <- fs::path(dir, "key_quantities.csv")

    if(fs::file_exists(captions_file))
        fs::file_delete(captions_file)

    if(fs::file_exists(keyquant_file))
        fs::file_delete(keyquant_file)

    unlink(fs::path(dir, "figures"), recursive = T)
}
