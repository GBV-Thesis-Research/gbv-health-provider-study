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
path_to_clean_rds_scores <- paste(gbv_project_wd, "/data/clean/gbv_data_scores.RDS", sep = "")

source(paste(gbv_project_wd, "/code/dependencies.R", sep = ""))

if (!file.exists(path_to_clean_rds_scores)) {
  source(paste(gbv_project_wd, "/code/score.R", sep = ""))
}

#### Lint current file ####
style_file(paste(gbv_project_wd, "/code/table_scores.R", sep = ""))

# Load cleaned data
path_to_clean_rds_scores <- paste(gbv_project_wd, "/data/clean/gbv_data_scores.RDS", sep = "")
clean_scores <- readRDS(path_to_clean_rds_scores)

scores_summary_table <- clean_scores %>%
  select(-participant_id) %>%
  tbl_summary(
    by = time_point,
    type = c(system_support_score, practice_score) ~ "continuous",
    label = list(
      knowledge_score ~ "Knowledge",
      system_support_score ~ "System support",
      attitude_score ~ "Attitude",
      empathy_score ~ "Empathy",
      confidence_score ~ "Confidence",
      practice_score ~ "Practice"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})"
    ),
    digits = all_continuous() ~ 2
  ) %>%
  add_p()
scores_summary_table
