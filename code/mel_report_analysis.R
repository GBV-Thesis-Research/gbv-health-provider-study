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

#style_file(paste(gbv_project_wd, "/code/mel_report_analysis.R", sep = ""))

path_to_clean_rds_scores <- paste(gbv_project_wd, "/data/clean/gbv_data_scores.RDS", sep = "")
clean_data_scores <- readRDS(path_to_clean_rds_scores)

# Reduce data to only include matches (n = 46)
clean_data_scores <- clean_data_scores %>%
  filter(status == "All three")

# CREATE NEW VARIABLES BASED ON MEL PLAN ---------------------------------------
# Create new pre-score variable for outcome 4, including the knowledge, attitude, and empathy domains
clean_data_scores <- clean_data_scores %>%
  mutate(outcome4_pre_score = ifelse(time_point == 1,
                                     ((knowledge_general_score + knowledge_warning_score +
                                         knowledge_appropriate_score + knowledge_helpful_score +
                                         attitude_general_score + attitude_acceptability_score +
                                         attitude_genderroles_score + attitude_profroles_score +
                                         empathy_score) / 900) * 100,
                                     NA))

# Create new follow-up-score variable for outcome 4, including the knowledge, attitude, and empathy domains
clean_data_scores <- clean_data_scores %>%
  mutate(outcome4_post_score = ifelse(time_point == 3,
                                     ((knowledge_general_score + knowledge_warning_score +
                                         knowledge_appropriate_score + knowledge_helpful_score +
                                         attitude_general_score + attitude_acceptability_score +
                                         attitude_genderroles_score + attitude_profroles_score +
                                         empathy_score) / 900) * 100,
                                     NA))

# Create new pre-score variable for outcome 4, including the confidence, system support, 
# and professional role domains <- have to ask Xylia  about the professional role one here 
# as its in attitude domain, not its own domain
clean_data_scores <- clean_data_scores %>%
  mutate(outcome5_pre_score = ifelse(time_point == 1,
                                     ((confidence_score + system_support_score) / 200) * 100,
                                     NA))

# Create new follow-up score variable for outcome 4, including the confidence, system support, 
# and professional role domains <- have to ask Xylia  about the professional role one here 
# as its in attitude domain, not its own domain
clean_data_scores <- clean_data_scores %>%
  mutate(outcome5_post_score = ifelse(time_point == 3,
                                     ((confidence_score + system_support_score) / 200) * 100,
                                     NA))

# CREATE NEW DATAFRAMES FOR FACILITY & REGIONAL SCORES BASED ON MEL PLAN -------
# Create new data frame for regional domain scores
regional_scores <- clean_data_scores %>%
  group_by(region) %>%
  summarize(
    outcome4_pre_score = mean(outcome4_pre_score, na.rm = TRUE),
    outcome4_post_score = mean(outcome4_post_score, na.rm = TRUE),
    outcome5_pre_score = mean(outcome5_pre_score, na.rm = TRUE),
    outcome5_post_score = mean(outcome5_post_score, na.rm = TRUE),
  )

# Create new data frame for regional domain scores
facility_scores <- clean_data_scores %>%
  group_by(standardized_facility) %>%
  summarize(
    outcome4_pre_score = mean(outcome4_pre_score, na.rm = TRUE),
    outcome4_post_score = mean(outcome4_post_score, na.rm = TRUE),
    outcome5_pre_score = mean(outcome5_pre_score, na.rm = TRUE),
    outcome5_post_score = mean(outcome5_post_score, na.rm = TRUE),
  )

# CREATE SCORE TABLES FOR REGIONAL SCORES BASED ON MEL PLAN -------------------
# Create score tables - outcome 4
scores_summary_table <- clean_scores %>%
  # filter(status == "All three") %>%
  filter(time_point != 1) %>%
  tbl_summary(
    include = -c(participant_id_3, inclusive_status, region, status),
    by = c(time_point),
    type = c(
      system_support_score, practice_score, knowledge_warning_score,
      knowledge_appropriate_score
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
