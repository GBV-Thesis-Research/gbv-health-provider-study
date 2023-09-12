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
  filter(status == "All three")

# CREATE NEW VARIABLES BASED ON MEL PLAN ---------------------------------------
# Create new pre-score variable for outcome 4, including the knowledge, attitude,
# and empathy domains
clean_data_scores <- clean_data_scores %>%
  mutate(outcome4_pre_score = ifelse(time_point == 1,
    ((knowledge_general_score + knowledge_warning_score +
      knowledge_appropriate_score + knowledge_helpful_score +
      attitude_general_score + attitude_acceptability_score +
      attitude_genderroles_score + attitude_profroles_score +
      empathy_score) / 900) * 100,
    NA
  ))

# Create new follow-up-score variable for outcome 4, including the knowledge,
# attitude, and empathy domains
clean_data_scores <- clean_data_scores %>%
  mutate(outcome4_post_score = ifelse(time_point == 3,
    ((knowledge_general_score + knowledge_warning_score +
      knowledge_appropriate_score + knowledge_helpful_score +
      attitude_general_score + attitude_acceptability_score +
      attitude_genderroles_score + attitude_profroles_score +
      empathy_score) / 900) * 100,
    NA
  ))

# # Create new pre-score variable for outcome 4, including the confidence, system support,
# # and professional role domains <- have to ask Xylia  about the professional role one here
# # as its in attitude domain, not its own domain
clean_data_scores <- clean_data_scores %>%
  mutate(outcome5_pre_score = ifelse(time_point == 1,
    ((confidence_score + system_support_score) / 200) * 100,
    NA
  ))

# # Create new follow-up score variable for outcome 4, including the confidence, system support,
# # and professional role domains <- have to ask Xylia  about the professional role one here
# # as its in attitude domain, not its own domain
clean_data_scores <- clean_data_scores %>%
  mutate(outcome5_post_score = ifelse(time_point == 3,
    ((confidence_score + system_support_score) / 200) * 100,
    NA
  ))

# # CREATE NEW DATAFRAMES FOR MEDIAN REGIONAL SCORES------------------------------
# # Create new data frame for median regional domain scores
regional_scores_median <- clean_data_scores %>%
  group_by(region) %>%
  summarize(
    outcome4_pre_score = median(outcome4_pre_score, na.rm = TRUE),
    outcome5_pre_score = median(outcome5_pre_score, na.rm = TRUE),
    outcome4_post_score = median(outcome4_post_score, na.rm = TRUE),
    outcome5_post_score = median(outcome5_post_score, na.rm = TRUE),
  )

# Transpose regional_scores_mean table
regional_scores_median_pre <- t(regional_scores_median_pre) %>%
  as.data.frame(regional_scores_median_pre)

regional_scores_median_post <- t(regional_scores_median_post) %>%
  as.data.frame(regional_scores_median_post)

# Rename columns to regions & drop region row from dataframe
colnames(regional_scores_median_pre) <- regional_scores_median_pre[1, ]
regional_scores_median_pre <- regional_scores_median_pre[-1, ]

colnames(regional_scores_median_post) <- regional_scores_median_post[1, ]
regional_scores_median_post <- regional_scores_median_post[-1, ]

# Rename rows to outcomes pre/post
row.names(regional_scores_median_pre) <- c("Outcome 4 pre", "Outcome 5 pre")

row.names(regional_scores_median_post) <- c("Outcome 4 post", "Outcome 5 post")

# Make df numeric
for (col in names(regional_scores_median_pre)) {
  regional_scores_median_pre[[col]] <- as.numeric(regional_scores_median_pre[[col]])
}
str(regional_scores_median_pre)

for (col in names(regional_scores_median_post)) {
  regional_scores_median_post[[col]] <- as.numeric(regional_scores_median_post[[col]])
}
str(regional_scores_median_post)

#
result <- wilcox.test(group1, group2, paired = TRUE)

# CREATE NEW DATAFRAMES FOR MEDIAN REGIONAL SCORES FOR OUTCOME 4----------------
# Create new data frame for median regional domain scores - pre-test
# regional_scores_median_pre <- clean_data_scores %>%
#   filter(time_point == 1) %>%
#   group_by(region) %>%
#   summarize(
#     knowledge_general_score = median(knowledge_general_score),
#     knowledge_warning_score = median(knowledge_warning_score),
#     knowledge_helpful_score = median(knowledge_helpful_score),
#     knowledge_appropriate_score = median(knowledge_appropriate_score),
#     attitude_general_score = median(attitude_general_score),
#     attitude_acceptability_score = median(attitude_acceptability_score),
#     attitude_genderroles_score = median(attitude_genderroles_score),
#     empathy_score = median(empathy_score)
#   )
#
# #regional_scores_median_pre <- regional_scores_median_pre %>% mutate(timepoint = "1")
#
# # Create new data frame for median regional domain scores - post-test
# regional_scores_median_post <- clean_data_scores %>%
#   filter(time_point == 3) %>%
#   group_by(region) %>%
#   summarize(
#     knowledge_general_score = median(knowledge_general_score),
#     knowledge_warning_score = median(knowledge_warning_score),
#     knowledge_helpful_score = median(knowledge_helpful_score),
#     knowledge_appropriate_score = median(knowledge_appropriate_score),
#     attitude_general_score = median(attitude_general_score),
#     attitude_acceptability_score = median(attitude_acceptability_score),
#     attitude_genderroles_score = median(attitude_genderroles_score),
#     empathy_score = median(empathy_score)
#   )

# regional_scores_median_post <- regional_scores_median_post %>% mutate(timepoint = "3")

# Merge dataframes
regional_scores_median <- merge(regional_scores_median_pre, regional_scores_median_post, all = TRUE)

# CALCULATIONS FOR # OF CLIENTS IDENTIFIED ------------------------------------
clientnum_baseline <- clean_data %>%
  filter(status == "All three") %>%
  filter(time_point == 1) %>%
  filter(practices_18 == 1)

sum(clientnum_baseline$practices_18yes_number) / 6

clientnum_endline <- clean_data %>%
  filter(status == "All three") %>%
  filter(time_point == 3) %>%
  filter(practices_18 == 1)

sum(clientnum_endline$practices_18yes_number) / 16
