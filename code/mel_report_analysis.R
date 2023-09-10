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

# CREATE NEW DATAFRAME FOR MEAN REGIONAL SCORES --------------------------------
# Create new data frame for mean regional domain scores
regional_scores_mean <- clean_data_scores %>%
  group_by(region) %>%
  summarize(
    outcome4_pre_score = mean(outcome4_pre_score, na.rm = TRUE),
    outcome4_post_score = mean(outcome4_post_score, na.rm = TRUE),
    outcome5_pre_score = mean(outcome5_pre_score, na.rm = TRUE),
    outcome5_post_score = mean(outcome5_post_score, na.rm = TRUE),
  )

# Transpose regional_scores_mean table
regional_scores_mean <- t(regional_scores_mean) %>% 
  as.data.frame(regional_scores_mean)

# Rename columns to regions & drop region row from dataframe
colnames(regional_scores_mean) <- regional_scores_mean[1, ] 
regional_scores_mean <- regional_scores_mean[-1, ]

# Rename rows to outcomes pre/post 
row.names(regional_scores_mean) <- c("Outcome 4 pre", "Outcome 4 post", "Outcome 5 pre", "Outcome 5 post")

# CREATE NEW DATAFRAME FOR MEAN FACILITY SCORES --------------------------------
# Create new data frame for mean facility domain scores
facility_scores_mean <- clean_data_scores %>%
  group_by(standardized_facility) %>%
  summarize(
    outcome4_pre_score = mean(outcome4_pre_score, na.rm = TRUE),
    outcome4_post_score = mean(outcome4_post_score, na.rm = TRUE),
    outcome5_pre_score = mean(outcome5_pre_score, na.rm = TRUE),
    outcome5_post_score = mean(outcome5_post_score, na.rm = TRUE),
  )

# Transpose regional_scores_mean table
facility_scores_mean <- t(facility_scores_mean) %>% 
  as.data.frame(facility_scores_mean)

# Rename columns to regions & drop region row from dataframe
colnames(facility_scores_mean) <- regional_scores_mean[1, ] 
facility_scores_mean <- facility_scores_mean[-1, ]

# Rename rows to outcomes pre/post 
row.names(facility_scores_mean) <- c("Outcome 4 pre", "Outcome 4 post", "Outcome 5 pre", "Outcome 5 post")

# CREATE NEW DATAFRAMES FOR MEDIAN REGIONAL SCORES------------------------------
# Create new data frame for median regional domain scores
regional_scores_median <- clean_data_scores %>%
  group_by(region) %>%
  summarize(
    outcome4_pre_score = median(outcome4_pre_score, na.rm = TRUE),
    outcome4_post_score = median(outcome4_post_score, na.rm = TRUE),
    outcome5_pre_score = median(outcome5_pre_score, na.rm = TRUE),
    outcome5_post_score = median(outcome5_post_score, na.rm = TRUE),
  )

# Transpose regional_scores_mean table
regional_scores_median <- t(regional_scores_median) %>% 
  as.data.frame(regional_scores_median)

# Rename columns to regions & drop region row from dataframe
colnames(regional_scores_median) <- regional_scores_median[1, ] 
regional_scores_median <- regional_scores_median[-1, ]

# Rename rows to outcomes pre/post 
row.names(regional_scores_median) <- c("Outcome 4 pre", "Outcome 4 post", "Outcome 5 pre", "Outcome 5 post")

# CREATE NEW DATAFRAMES FOR MEDIAN FACILITY SCORES------------------------------

# Create new data frame for median facility domain scores
facility_scores_median <- clean_data_scores %>%
  group_by(standardized_facility) %>%
  summarize(
    outcome4_pre_score = median(outcome4_pre_score, na.rm = TRUE),
    outcome4_post_score = median(outcome4_post_score, na.rm = TRUE),
    outcome5_pre_score = median(outcome5_pre_score, na.rm = TRUE),
    outcome5_post_score = median(outcome5_post_score, na.rm = TRUE),
  )

# Transpose regional_scores_mean table
facility_scores_median <- t(facility_scores_median) %>% 
  as.data.frame(facility_scores_median)

# Rename columns to regions & drop region row from dataframe
colnames(facility_scores_median) <- facility_scores_median[1, ] 
facility_scores_median <- facility_scores_median[-1, ]

# Rename rows to outcomes pre/post 
row.names(facility_scores_median) <- c("Outcome 4 pre", "Outcome 4 post", "Outcome 5 pre", "Outcome 5 post")

# CREATE MEDIAN SCORE TABLES FOR REGIONAL SCORES BASED ON MEL PLAN ---------------
# Create score tables - outcome 4
outcome4_scores_summary_table <- regional_scores_median %>%
  tbl_summary(
    by = c(starts_with("outcome4")),
    type = c(
      region
    ) ~ "continuous",
    label = list(
      outcome4_pre_score ~ "Outcome 4: knowledge, empathy, and attitudes pre-score",
      outcome4_post_score ~ "Outcome 4: knowledge, empathy, and attitudes pre-score"
    ),
    statistic = list(
      all_continuous() ~ "{median} ({sd})"
    ),
    digits = all_continuous() ~ 2
  ) %>%
  add_p() %>%
  add_n() %>%
  bold_p()
outcome4_scores_summary_table
