## code to prepare `DATASET` dataset goes here

# This script runs inside the GitHub Action environment (nmfs-ost/stockplotr)

# Ensure devtools is installed (installed in the workflow, but good practice to check)
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
library(devtools)

# --- Configuration (Relative Paths inside Repo B's workspace) ---
# This path is set by the workflow step that checks out 'asar'
REPO_A_SOURCE_PATH <- "repo_a_source"

# VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
# --- CUSTOMIZE THESE THREE VARIABLES ---

# 1. The name of the function we need to call from Repo A (nmfs-ost/asar)
FUNCTION_NAME <- "convert_output" 

# 2. The path to the raw text data in Repo B (nmfs-ost/stockplotr)
# NOTE: The raw data is typically stored in data-raw/ to keep it out of the package build.
RAW_DATA_PATH <- "data-raw/Report.sso" 

# 3. The name of the object (and file prefix) for the new example data
OUTPUT_OBJECT_NAME <- "example_data"

# ^^^^ END CUSTOMIZATION ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# The output path for the R data file in Repo B's data/ directory
OUTPUT_FILE_PATH <- paste0("data/", OUTPUT_OBJECT_NAME, ".rda")


# --- Main Logic ---

# 1. Load the functions from Repository A's source package ('asar')
message("Loading functions from nmfs-ost/asar source: ", REPO_A_SOURCE_PATH)
tryCatch({
  # This makes all functions from 'asar' available in this R session
  load_all(REPO_A_SOURCE_PATH, quiet = TRUE)
}, error = function(e) {
  stop(paste("Failed to load Repository A source (asar):", e$message))
})


# 2. Check for the function existence and retrieve it
if (!exists(FUNCTION_NAME)) {
  stop(paste0("Function '", FUNCTION_NAME, "' not found in nmfs-ost/asar source."))
}
conversion_function <- get(FUNCTION_NAME)


# 3. Read the data from Repository B (stockplotr)
message("Reading raw data from nmfs-ost/stockplotr: ", RAW_DATA_PATH)
# NOTE: Adjust the read function (read.csv, read.table, etc.) based on your raw file
raw_data <- RAW_DATA_PATH


# 4. Run the function from Repo A on the data from Repo B
message("Running conversion function ", FUNCTION_NAME, "...")
# The resulting object is the data frame for the package
converted_data_frame <- conversion_function(raw_data)


# 5. Save the resulting data frame into the data/ directory of Repo B
message("Saving generated data frame as R package data: ", OUTPUT_FILE_PATH)
if (!dir.exists("data")) { dir.create("data") }

# Assign the resulting data frame to the specified object name
# This object name will be what the user loads via `data(asar_catch_data)`
assign(OUTPUT_OBJECT_NAME, converted_data_frame)

# save() is used to create the .rda file for the package
save(list = OUTPUT_OBJECT_NAME, file = OUTPUT_FILE_PATH, compress = "gzip")

message("Data generation complete. File saved to ", OUTPUT_FILE_PATH)

# Exit successfully
q(save = "no")

