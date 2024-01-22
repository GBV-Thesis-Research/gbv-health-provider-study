##################################################
## Project:
## Script purpose:
## Date:
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

source(paste(gbv_project_wd, "/code/dependencies.R", sep = ""))
style_file(paste(gbv_project_wd, "/code/demographic_data_cleaning.R", sep = ""))

gbv_data_clean <- readRDS(file = paste(gbv_project_wd, "/data/clean/gbv_data_clean.RDS", sep = ""))

demographic_columns <- c(
  "participant_id_3", "standardized_facility", "time_point",
  "region", "sex_factored", "status", "inclusive_status", "age_groups", "position_groups",
  "position_years_clean", "position_groups", "municipality", "previous_training"
)

# Forward and back fill all the data by pid to remove NA values, pivot wide
wide_df <- gbv_data_clean %>%
  select(all_of(demographic_columns)) %>%
  group_by(participant_id_3) %>%
  fill(sex_factored, .direction = "downup") %>%
  fill(age_groups, .direction = "downup") %>%
  fill(standardized_facility, .direction = "downup") %>%
  fill(position_groups, .direction = "downup") %>%
  fill(position_years_clean, .direction = "downup") %>%
  fill(municipality, .direction = "downup") %>%
  fill(previous_training, .direction = "downup") %>%
  ungroup() %>%
  select(
    "participant_id_3", "time_point", "standardized_facility", "region", "sex_factored", "age_groups",
    "status", "position_groups", "position_years_clean", "municipality", "previous_training"
  ) %>%
  pivot_wider(id_cols = participant_id_3, names_from = time_point, values_from = c(
    standardized_facility, region, sex_factored, age_groups, status, position_groups,
    position_years_clean, municipality, previous_training
  ))

# Select the first non NA value for each set of values across timepoints
final_demographic_data <-
  wide_df %>%
  mutate(sex_factored = case_when(
    sex_factored_1 != "Other" ~ as.character(sex_factored_1),
    sex_factored_2 != "Other" ~ as.character(sex_factored_2),
    sex_factored_3 != "Other" ~ as.character(sex_factored_3),
    TRUE ~ "Other"
  )) %>%
  mutate(standardized_facility = coalesce(standardized_facility_1, standardized_facility_2, standardized_facility_3)) %>%
  mutate(age_groups = coalesce(age_groups_1, age_groups_2, age_groups_3)) %>%
  mutate(status = coalesce(status_1, status_2, status_3)) %>%
  mutate(region = coalesce(region_1, region_2, region_3)) %>%
  mutate(position_groups = droplevels(coalesce(position_groups_1, position_groups_2, position_groups_3))) %>%
  mutate(position_years_clean = as.numeric(coalesce(position_years_clean_1, position_years_clean_2, position_years_clean_3))) %>%
  mutate(municipality = coalesce(municipality_1, municipality_2, municipality_3)) %>%
  mutate(previous_training = coalesce(previous_training_1, previous_training_2, previous_training_3)) %>%
  select(
    "participant_id_3", "standardized_facility", "region", "sex_factored", "age_groups",
    "status", "position_groups", "position_years_clean", "municipality", "previous_training"
  )
demographic_data_file_path <- paste(gbv_project_wd, "/data/clean/demographic_data_clean.RDS", sep = "")
write_rds(final_demographic_data, demographic_data_file_path)
