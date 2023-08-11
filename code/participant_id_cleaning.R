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

# DATA MANAGEMENT --------------------------------------------------------------
# Separate out key from data (key is the redcap entry form containing correct answers)
key <- raw_gbv_survey_data %>%
  filter(participant_id == "KEY") %>%
  select(everything())

data <- raw_gbv_survey_data %>%
  mutate(time_point = if_else(date %in% c("2003-07-03", "2003-07-10", "2017-07-10", "2023-07-03", "2023-07-10"), 3, time_point)) %>%
  filter(time_point != 3)

# Drop participants that have not consented to have their data used for research
# Drops from 972 to 929 (removes 43 rows)
data <- data %>%
  filter(consent == 1) %>%
  filter(!date %in% c("2021-06-18", "2021-06-14")) %>%
  filter(municipality != "Dili") %>%
  filter(participant_id != "KEY")