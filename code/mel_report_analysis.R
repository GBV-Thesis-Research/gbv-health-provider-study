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

# CREATE NEW VARIABLES BASED ON MEL PLAN --------------------------------
# Create new pre-score variable for outcome 4, including the knowledge, attitude, and empathy domains
clean_data_scores <- clean_data_scores %>%
  mutate(outcome4_pre_score = ifelse(time_point == 1,
                                     ((knowledge_general_score + knowledge_warning_score +
                                         knowledge_appropriate_score + knowledge_helpful_score +
                                         attitude_general_score + attitude_acceptability_score +
                                         attitude_genderroles_score + attitude_profroles_score +
                                         empathy_score) / 900) * 100,
                                     NA))

# Create new post-score variable for outcome 4, including the knowledge, attitude, and empathy domains
clean_data_scores <- clean_data_scores %>%
  mutate(outcome4_post_score = ifelse(time_point == 3,
                                     ((knowledge_general_score + knowledge_warning_score +
                                         knowledge_appropriate_score + knowledge_helpful_score +
                                         attitude_general_score + attitude_acceptability_score +
                                         attitude_genderroles_score + attitude_profroles_score +
                                         empathy_score) / 900) * 100,
                                     NA))

# Create new variable for improvement pre to post for outcome 4, including the 
# knowledge, attitude, and empathy domains (post-score - pre-score) -- how to do this with participants as rows?



# Create new variable for improvement pre to post for outcome 5, including the 
# confidence, system support, and professional role domains <- have to ask Xylia 
# about the profesisonal role one here as its in attitude domain, not its own domain