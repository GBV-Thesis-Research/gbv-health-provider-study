##################################################
## Project: GBV Thesis
## Script purpose: Importing survey data
## Date: 2023-03-06
## Author: Jessica Dyer
##################################################

#### WD SETUP ####
current_wd <- getwd()

if(endsWith(current_wd, "gbv-health-provider-study")){
  gbv_project_wd <- current_wd
} else if (endsWith(current_wd, "/paper")) {
  gbv_project_wd <- str_remove(current_wd, "/paper")   
} else {
  print("Got a WD that's not handled in the If-else ladder yet")
}

source(paste(gbv_project_wd, "/keys.R", sep = ""))

import_redcap_data <- function(token_str){
  redcap_read(redcap_uri='https://redcap.iths.org/api/',
              token=token_str,
              raw_or_label = "label",
              export_checkbox_label = TRUE
  )$data
}

data_exists = file.exists(paste(gbv_project_wd, "/data/gbv_data_raw.csv", sep = ""))
if(data_exists){
  raw_gbv_survey_data = read.csv(paste(gbv_project_wd, "/data/gbv_data_raw.csv", sep = ""))
} else {
  raw_gbv_survey_data = import_redcap_data(token_str = API_KEY)
  path_to_write = paste(gbv_project_wd, "/data/gbv_data_raw.csv", sep = "")
  write.csv(raw_gbv_survey_data, path_to_write)
}


