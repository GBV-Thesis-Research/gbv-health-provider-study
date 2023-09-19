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

source(paste(gbv_project_wd, "/code/dependencies.R", sep = ""))
style_file(paste(gbv_project_wd, "/code/mel_report_analysis.R", sep = ""))

path_to_clean_rds_scores <- paste(gbv_project_wd, "/data/clean/gbv_data_scores.RDS", sep = "")
clean_data_scores <- readRDS(path_to_clean_rds_scores)

path_to_clean_three_timepoints <- paste(gbv_project_wd, "/data/clean/gbv_data_clean_three_timepoints.RDS", sep = "")
clean_data <- readRDS(path_to_clean_three_timepoints)

# Reduce data to only include matches (n = 46)
clean_data_scores <- clean_data_scores %>%
  ##  filter(status == "All three" & time_point != 2) %>%
  select(
    participant_id_3, region, time_point, outcome4_score,
    outcome5_score
  )

regional_scores <- clean_data_scores %>%
  pivot_wider(names_from = c(region), values_from = c(outcome4_score, outcome5_score)) %>%
  mutate(total_outcome4 = rowSums(select(., starts_with("outcome4")), na.rm = TRUE)) %>%
  mutate(total_outcome5 = rowSums(select(., starts_with("outcome5")), na.rm = TRUE))

difference_by_facility <-
  clean_data_scores %>%
  select(-c(participant_id_3)) %>%
  group_by(region, time_point) %>%
  summarize(
    outcome4_mean_value = mean(outcome4_score, na.rm = TRUE),
    outcome5_mean_value = mean(outcome5_score, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  pivot_wider(names_from = time_point, values_from = c(outcome4_mean_value, outcome5_mean_value))

difference_by_facility <- difference_by_facility %>%
  mutate(outcome4_difference = outcome4_mean_value_3 - outcome4_mean_value_1) %>%
  mutate(outcome5_difference = outcome5_mean_value_3 - outcome5_mean_value_1)
