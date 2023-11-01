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

path_to_clean_rds_scores <- paste(gbv_project_wd, "/data/clean/gbv_data_scores.RDS", sep = "")
clean_scores <- readRDS(path_to_clean_rds_scores)

# Reduce data to only include matches (n = 46)
clean_data_scores <- clean_data_scores %>%
  filter(status == "All three" & time_point != 2) %>%
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

difference_by_facility$outcome5_difference <- as.numeric(difference_by_facility$outcome5_difference)

# Limit to 2 decimals places
numeric_columns <- sapply(difference_by_facility, is.numeric)
difference_by_facility[numeric_columns] <- round(difference_by_facility[numeric_columns], digits = 2)

# investigating ermera score decrease
ermera_table <- clean_scores %>%
  filter(status == "All three") %>%
  filter(time_point != 2) %>%
  filter(region == "Ermera") %>%
  tbl_summary(
    include = -c(participant_id_3, inclusive_status, status, standardized_facility),
    by = c(time_point),
    type = c(
      system_support_score, practice_score, knowledge_general_score, knowledge_warning_score,
      knowledge_appropriate_score, knowledge_helpful_score, attitude_general_score, attitude_acceptability_score,
      attitude_genderroles_score, attitude_profroles_score, empathy_score, confidence_score
    ) ~ "continuous",
    label = list(
      knowledge_general_score ~ "General knowledge",
      knowledge_warning_score ~ "Warning signs",
      knowledge_appropriate_score ~ "Appropriate ways to ask about GBV",
      knowledge_helpful_score ~ "Helpful responses to support a woman subjected to GBV",
      system_support_score ~ "System support",
      attitude_general_score ~ "General attitudes towards GBV and the health provider role",
      attitude_acceptability_score ~ "Acceptability for a man to hit his partner",
      attitude_genderroles_score ~ "Attitudes towards gender roles",
      attitude_profroles_score ~ "Attitudes towards professional roles",
      empathy_score ~ "Empathy",
      confidence_score ~ "Confidence",
      practice_score ~ "Practice"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})"
    ),
    digits = all_continuous() ~ 2
  ) %>%
  add_p() %>%
  add_n() %>%
  bold_p()
ermera_table

ermera_scores <- clean_scores %>%
  filter(status == "All three") %>%
  filter(time_point != 2) %>%
  filter(region == "Ermera")

# Outcome 4 table
