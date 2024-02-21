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

analysis_long <-
  merged_scores %>%
  filter(status == "All three") %>%
  select(
    participant_id_3, time_point, knowledge_overall, attitude_overall, system_support_score,
    confidence_score, empathy_score, practice_score
  )

analysis_long <-
  left_join(analysis_long, demographic_data)

# Write analyses data to folder
path_to_clean_analysis_data_wide <- paste(gbv_project_wd, "/data/clean/analysis_data_wide.RDS", sep = "")
saveRDS(analysis_wide, file = path_to_clean_analysis_data_wide)

path_to_clean_analysis_data_long <- paste(gbv_project_wd, "/data/clean/analysis_data_long.RDS", sep = "")
saveRDS(analysis_long, file = path_to_clean_analysis_data_long)
