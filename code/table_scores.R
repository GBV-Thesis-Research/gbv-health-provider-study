##################################################
## Project:
## Script purpose:
## Date:
## Author: Jessica Dyer
##################################################

# NOTE: https://www.danieldsjoberg.com/gtsummary/articles/gallery.html#paired-test

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
  filter(status == "All three") %>%
  filter(time_point != 1) %>%
  tbl_summary(
    include = -c(participant_id_3, inclusive_status, region, status, standardized_facility),
    by = c(time_point),
    type = c(
      system_support_score, practice_score, knowledge_warning_score,
      knowledge_appropriate_score, knowledge_helpful_score
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
scores_summary_table
