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

raw_data_rds_path <- paste(gbv_project_wd, "/data/raw/gbv_data_raw_", last_friday, ".RDS", sep = "")
raw_data_csv_path <- paste(gbv_project_wd, "/data/raw/gbv_data_raw_", last_friday, ".csv", sep = "")

if (!file.exists(raw_data_csv_path)) {
  raw_gbv_survey_data <- import_redcap_data(token_str = API_KEY)
  write.csv(raw_gbv_survey_data, file = raw_data_csv_path)
  saveRDS(raw_gbv_survey_data, file = raw_data_rds_path)
} else {
  raw_gbv_survey_data <- read.csv(raw_data_csv_path)
}
