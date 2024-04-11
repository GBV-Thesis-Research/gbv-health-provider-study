##################################################
## Project: GBV Health provider study
## Script purpose: Data analysis
## Date: 1-13-24
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

source(paste(gbv_project_wd, "/code/dependencies.R", sep = ""))
style_file(paste(gbv_project_wd, "/code/analysis_prep.R", sep = ""))

source(paste(gbv_project_wd, "/code/score.R", sep = ""))
merged_scores <- readRDS(paste(gbv_project_wd, "/data/clean/gbv_data_scores.RDS", sep = ""))

source(paste(gbv_project_wd, "/code/attendance_data_cleaning.R", sep = ""))
path_to_clean_attendance <- paste(gbv_project_wd, "/data/clean/attendance_data_clean.RDS", sep = "")
attendance_data <- readRDS(path_to_clean_attendance)

source(paste(gbv_project_wd, "/code/demographic_data_cleaning.R", sep = ""))
path_to_clean_demographic_data <- paste(gbv_project_wd, "/data/clean/demographic_data_clean.RDS", sep = "")
demographic_data <- readRDS(path_to_clean_demographic_data)

# CREATE NEW DATA FRAME FOR ANALYSIS  ------------------------------------------
# Pull in domain scores for matched individuals and convert to wide format
analysis_wide <- merged_scores %>%
  filter(status == "All three") %>%
  select(
    participant_id_3, time_point, knowledge_overall, attitude_overall, system_support_score,
    confidence_score, empathy_score, practice_score
  ) %>%
  pivot_wider(id_cols = participant_id_3, names_from = time_point, values_from = c(
    knowledge_overall,
    attitude_overall,
    empathy_score,
    confidence_score,
    system_support_score,
    practice_score
  ))

# Join attendance data - need to add FUAT specific attendance score once all PRs are merged to main
analysis_wide <- left_join(analysis_wide, attendance_data %>% select(participant_id_3, attendance_score_FUAT),
  by = c("participant_id_3")
)

# Join demographics
analysis_wide <- left_join(analysis_wide, demographic_data, by = c("participant_id_3"))

# Add variable calculating change from baseline to endline and midline to endline
analysis_wide <- analysis_wide %>%
  mutate(
    know_change_overall =
      knowledge_overall_3 - knowledge_overall_1,
    know_change_midend =
      knowledge_overall_3 - knowledge_overall_2,
    att_change_overall =
      attitude_overall_3 - attitude_overall_1,
    att_change_midend =
      attitude_overall_3 - attitude_overall_2,
    conf_change_overall =
      confidence_score_3 - confidence_score_1,
    conf_change_midend =
      confidence_score_3 - confidence_score_2,
    emp_change_overall =
      empathy_score_3 - empathy_score_1,
    emp_change_midend =
      empathy_score_3 - empathy_score_2,
    syssup_change_overall =
      system_support_score_3 - system_support_score_1,
    syssup_change_midend =
      system_support_score_3 - system_support_score_2
  )

# Add binary variables for improved / did not improve
analysis_wide <- analysis_wide %>%
  mutate(
    know_improve_overall =
      ifelse(know_change_overall > 0, 1, 0),
    know_improve_midend =
      ifelse(know_change_midend > 0, 1, 0),
    att_improve_overall =
      ifelse(att_change_overall > 0, 1, 0),
    att_improve_midend =
      ifelse(att_change_midend > 0, 1, 0),
    conf_improve_overall =
      ifelse(conf_change_overall > 0, 1, 0),
    conf_improve_midend =
      ifelse(conf_change_midend > 0, 1, 0),
    emp_improve_overall =
      ifelse(emp_change_overall > 0, 1, 0),
    emp_improve_midend =
      ifelse(emp_change_midend > 0, 1, 0),
    syssup_improve_overall =
      ifelse(syssup_change_overall > 0, 1, 0),
    syssup_improve_midend =
      ifelse(syssup_change_midend > 0, 1, 0),
  )

# Add variables for "high improvement"
analysis_wide <- analysis_wide %>%
  mutate(
    know_improve_high =
      ifelse(know_change_overall > median(know_change_overall), 1, 0),
    att_improve_high =
      ifelse(att_change_overall > median(att_change_overall), 1, 0),
    conf_improve_high =
      ifelse(conf_change_overall > median(conf_change_overall), 1, 0),
    emp_improve_high =
      ifelse(emp_change_overall > median(emp_change_overall), 1, 0),
    syssup_improve_high =
      ifelse(syssup_change_overall > median(syssup_change_overall), 1, 0),
  )

# Create long data frame

analysis_long <-
  merged_scores %>%
  filter(status == "All three") %>%
  select(
    participant_id_3, time_point, knowledge_overall, attitude_overall, system_support_score,
    confidence_score, empathy_score, practice_score
  )

analysis_long <-
  left_join(outcome_data_long, attendance_data)

# Write analyses data to folder
path_to_clean_analysis_data_wide <- paste(gbv_project_wd, "/data/clean/analysis_data_wide.RDS", sep = "")
saveRDS(analysis_wide, file = path_to_clean_analysis_data_wide)

path_to_clean_analysis_data_long <- paste(gbv_project_wd, "/data/clean/analysis_data_long.RDS", sep = "")
saveRDS(analysis_long, file = path_to_clean_analysis_data_long)
