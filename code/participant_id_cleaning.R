##################################################
## Project: GBV health provider study
## Script purpose: Participant ID cleaning
## Date: 2023-08-10
## Author: Susan Glenn
##################################################

# SETUP ------------------------------------------------------------------------
# WD setup
current_wd <- getwd()

if (endsWith(current_wd, "gbv-health-provider-study")) {
  gbv_project_wd <- current_wd
} else if (endsWith(current_wd, "/paper")) {
  gbv_project_wd <- str_remove(current_wd, "/paper")
} else {
  print("Got a WD that's not handled in the If-else ladder yet")
}

source(paste(gbv_project_wd, "/code/data_import.R", sep = ""))

# Lint current file
style_file(paste(gbv_project_wd, "/code/data_cleaning.R", sep = ""))
