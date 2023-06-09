##################################################
## Project:
## Script purpose:
## Date:
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

#### Lint current file ####
style_file(paste(gbv_project_wd, "/code/data_cleaning.R", sep = ""))

source(paste(gbv_project_wd, "/code/data_import.R", sep = ""))

# Demographic field names
dem.vars <- c(
  "time_point", "participant_id", "municipality",
  "facilitator", "date", "facility", "sex", "position", "age", "position_years", "position_months",
  "previous_training", "patient_volume", "position_other"
)

# Separate out key from data (key is the redcap entry form containing correct answers)
key <- raw_gbv_survey_data %>%
  filter(participant_id == "KEY") %>%
  select(everything())

# Drop participants that have not consented to have their data used for research
data <- raw_gbv_survey_data %>%
  filter(consent == 1) %>%
  filter(!date %in% c("2021-06-18", "2021-06-14")) %>%
  filter(municipality != "Dili") %>%
  filter(participant_id != "KEY")

# Standardize municipality names
data <- data %>%
  mutate(municipality = if_else(municipality %in% c("LIQUICA", "Liquisa", "LIQUISA", "Liqujca"),
    "Liquica",
    municipality
  ))

# Filter out identical records which were accidentally imported into RedCap twice by the HAMNASA data collection team
# This isn't actually removing any duplicates and neither is Cory's code. Likely because she didn't have
# all the same columns that we're using. Should confirm how to de-duplicate.

data <- data %>%
  select(-record_id) %>%
  distinct()

# Fix dates that are empty
data <- data %>%
  mutate(date = case_when(
    date == "" & facility %in% c("hatulia", "Hatulia") & time_point == 2 ~ "2021-09-24",
    date == "" & facility == "Maubara" & time_point == 1 ~ "2021-10-11",
    date == "" & facility == "Maubara" & time_point == 2 ~ "2021-10-15",
    TRUE ~ date
  ))

