library(haven)

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

# Load data
source(paste(gbv_project_wd, "/code/attendance_data_cleaning.R", sep = ""))
path_to_clean_attendance <- paste(gbv_project_wd, "/data/clean/attendance_data_clean.RDS", sep = "")
attendance_data <- readRDS(path_to_clean_attendance)

source(paste(gbv_project_wd, "/code/rescore_for_imputation.R", sep = ""))
path_to_scores_for_imputation <- paste(gbv_project_wd, "/data/clean/scores_for_imputation.RDS", sep = "")
imputation_scores <- readRDS(path_to_scores_for_imputation)

# Delete all but FUAT sessions 
attendance_for_mary <- attendance_data %>%
  select("participant_id_3", "attendance_score_FUAT")

# shorten attendance score variable name
attendance_for_mary <- attendance_for_mary %>%
  rename(attendance = attendance_fuat)

# Export attendance data alone for Mary
path_to_attendance_for_mary <- paste(gbv_project_wd, "/data/clean/attendance_data_for_mary.RDS", sep = "")
saveRDS(attendance_for_mary, file = path_to_attendance_for_mary)

write_dta(attendance_for_mary, "/Users/susanglenn/Repositories/gbv-health-provider-study/extra_data/attendance_for_mary.dta")

# CREATE NEW DATA FRAME FOR ANALYSIS  ------------------------------------------
# bind scores for imputation, knowledge_sys_support_scores_raw, clean_data attitudes, conf, and empath, and demographics

# Filter clean_data to only include empathy, attitudes, and confidence scores and participant id
att_conf_emp <- clean_data %>%
  select(participant_id_3, matches("empathy|attitudes|confidence"))



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

