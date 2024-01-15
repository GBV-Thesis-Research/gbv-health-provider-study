##################################################
## Project: GBV Thesis
## Script purpose: Importing survey data
## Date: 2023-03-06
## Author: Jessica Dyer
##################################################

#### WD SETUP ####
current_wd <- getwd()

if (endsWith(current_wd, "gbv-health-provider-study")) {
  gbv_project_wd <- current_wd
} else if (endsWith(current_wd, "/paper")) {
  gbv_project_wd <- str_remove(current_wd, "/paper")
} else {
  print("Got a WD that's not handled in the If-else ladder yet")
}

source(paste(gbv_project_wd, "/keys.R", sep = ""))
source(paste(gbv_project_wd, "/code/dependencies.R", sep = ""))

#### Lint current file ####
style_file(paste(gbv_project_wd, "/code/data_import.R", sep = ""))

#### GENERATE DATE CLOSEST FRIDAY (IF TODAY IS FRIDAY, IT WILL RETURN TODAY'S DATE) ####
last_friday <- Sys.Date() - wday(Sys.Date() + 1)

if ((Sys.Date() - last_friday) >= 7) {
  last_friday <- Sys.Date()
}

import_redcap_data <- function(token_str) {
  redcap_read(
    redcap_uri = "https://redcap.iths.org/api/",
    token = token_str,
    raw_or_label = "raw",
    export_checkbox_label = TRUE
  )$data
}

final_raw_file_name <- paste(gbv_project_wd, "/data/raw/final_gbv_data_raw.RDS", sep = "")

if (!file.exists(final_raw_file_name)) {
  index_last_item <- length(list.files(path = final_rds_data_path, pattern = ".RDS"))
  file_name <- list.files(path = final_rds_data_path, pattern = ".RDS")[index_last_item]
  raw_gbv_survey_data <- readRDS(file = paste(final_rds_data_path, file_name, sep = ""))
  saveRDS(raw_gbv_survey_data, file = final_raw_file_name)
} else {
  raw_gbv_survey_data <- readRDS(file = final_raw_file_name)
}
