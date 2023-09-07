##################################################
## Project: GBV Health Provider Study
## Script purpose: Running analysis for GBV HAMNASA USAID report
## Date: 09-05-2023
## Author: Susan Glenn
##################################################

# SETUP ------------------------------------------------------------------------
# WD Setup
current_wd <- getwd()

# Lint current file
if (endsWith(current_wd, "gbv-health-provider-study")) {
  gbv_project_wd <- current_wd
} else if (endsWith(current_wd, "/paper")) {
  gbv_project_wd <- str_remove(current_wd, "/paper")
} else {
  print("Got a WD that's not handled in the If-else ladder yet")
}

#style_file(paste(gbv_project_wd, "/code/mel_report_analysis.R", sep = ""))

path_to_clean_rds_scores <- paste(gbv_project_wd, "/data/clean/gbv_data_scores.RDS", sep = "")
clean_data_scores <- readRDS(path_to_clean_rds_scores)

# CREATE NEW COMPOSITE SCORES BASED ON MEL PLAN --------------------------------
