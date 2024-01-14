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

# CREATE NEW DATA FRAME FOR ANALYSIS  ------------------------------------------
# Pull in domain scores for matched individuals
analysis <- merged_scores %>%
  mutate(knowledge1 = ifelse(time_point == "1", know
))

# Convert to wide format  

# Create overall score variable for timepoints 1, 2, and 3

# Join attendance data
  
# Join demographics
  


merged_scores <- merged_scores %>%
  mutate(outcome4_score = ifelse(status == "All three",
    ((knowledge_general_score + knowledge_warning_score +
      knowledge_appropriate_score + knowledge_helpful_score +
      attitude_general_score + attitude_acceptability_score +
      attitude_genderroles_score + attitude_profroles_score +
      empathy_score) / 900) * 100,
    NA
  ))




# Write score data to folder
path_to_clean_rds_scores <- paste(gbv_project_wd, "/data/clean/gbv_data_scores.RDS", sep = "")
saveRDS(merged_scores, file = path_to_clean_rds_scores)
