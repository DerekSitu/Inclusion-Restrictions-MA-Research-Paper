# -------------------------------------------------------------------------- #
# This master script runs all of the scripts used in the project's analysis.

# Prerequisite:
# Package manager ("pacman") package
#   This loads all other packages used in the scripts, installing if
#   necessary.

# install.packages("pacman")
# -------------------------------------------------------------------------- #


# Project Settings --------------------------------------------------------

# Run data cleaning: 01_clean_data.R
run_clean_data <- TRUE

# Run generation of pre-analysis figures: 02_pre_analysis.R
run_pre_analysis <- TRUE

# Run difference-in-differences results: 03_did_results.R
run_did_results <- TRUE

# Run regression discontinuity results: 04_rd_results.R
run_rd_results <- TRUE


# Run Scripts -------------------------------------------------------------

if (run_clean_data) {
  source("01_clean_data.R")
}

if (run_pre_analysis) {
  source("02_pre_analysis.R")
}

if (run_did_results) {
  source("03_did_results.R")
}

if (run_rd_results) {
  source("04_rd_results.R")
}



