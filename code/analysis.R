##################################################
## Project: GBV Health provider study
## Script purpose: Data analysis
## Date: 1-13-24
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

style_file(paste(gbv_project_wd, "/code/analysis.R", sep = ""))

source(paste(gbv_project_wd, "/code/score.R", sep = ""))
path_to_clean_rds <- paste(gbv_project_wd, "/data/clean/gbv_data_clean.RDS", sep = "")
merged_scores <- readRDS(path_to_clean_rds_scores)
source(paste(gbv_project_wd, "/code/attendance_data", sep = ""))
path_to_clean_attendance <- paste(gbv_project_wd, "/data/clean/attendance_data_clean.RDS", sep = "")
attendance_data <- readRDS(path_to_clean_attendance)

# CREATE NEW DATA FRAME FOR ANALYSIS  ------------------------------------------
# Pull in domain scores for matched individuals and convert to wide format
analysis <- merged_scores %>%
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
analysis <- inner_join(analysis, attendance_data %>% select(participant_id_3, attendance_score), 
                       by = c("participant_id_3"))


result <- inner_join(df1, df2 %>% select(participant_id_3, Value2), by = "participant_id_3")


# Join demographics


# Write score data to folder
path_to_clean_rds_scores <- paste(gbv_project_wd, "/data/clean/gbv_data_scores.RDS", sep = "")
saveRDS(merged_scores, file = path_to_clean_rds_scores)
