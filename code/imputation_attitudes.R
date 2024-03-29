##################################################
## Project: GBV Health provider study
## Script purpose: Imputation for Attitudes
## Date: 3-27-24
## Author: Susan Glenn
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

source(paste(gbv_project_wd, "/code/imputation_prep.R", sep = ""))
path_to_clean_analysis_data_long_imp <- paste(gbv_project_wd, "/data/clean/analysis_data_long_imputation.RDS", sep = "")
imp_data <- readRDS(path_to_clean_analysis_data_long_imp)

# Create attitudes dataset for each timepoint, dropping unnecessary data
att_1 <- imp_data %>%
  filter(time_point == 1) %>%
  select(-3, -5:-9,-12,-14,-20:-62, -92:-123)

att_2 <- imp_data %>%
  filter(time_point == 2) %>%
  select(-3, -5:-9,-12,-14,-20:-62, -92:-123)

att_3 <- imp_data %>%
  filter(time_point == 3) %>%
  select(-3, -5:-9,-12,-14,-20:-62, -92:-123)