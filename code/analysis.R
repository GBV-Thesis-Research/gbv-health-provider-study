##################################################
## Project: GBV Health provider study
## Script purpose: Data analysis
## Date: 1-13-24
## Author: Jessica Dyer
##################################################

# SETUP ------------------------------------------------------------------------
current_wd <- getwd()

# Lint current file
if (endsWith(current_wd, "gbv-health-provider-study")) {
  gbv_project_wd <- current_wd
} else if (endsWith(current_wd, "/paper")) {
  gbv_project_wd <- str_remove(current_wd, "/paper")
} else {
  print("Got a WD that's not handled in the If-else ladder yet")
}

analysis_df_fp <- paste(gbv_project_wd, "/data/clean/analysis_data.RDS", sep = "")
df <- readRDS(analysis_df_fp)
