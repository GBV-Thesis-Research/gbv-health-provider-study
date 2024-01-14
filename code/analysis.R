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
# Pull in domain scores for matched individuals and create variables for timepoint 1, 2, and 3 scores
analysis <- merged_scores %>%
  filter(status == "All three") %>%
  select(participant_id_3, time_point, knowledge_overall, attitude_overall, system_support_score, 
         confidence_score, empathy_score, practice_score) %>% 
  mutate(knowledge1 = ifelse(time_point==1, knowledge_overall, NA),
         knowledge2 = ifelse(time_point==2, knowledge_overall, NA),
         knowledge3 = ifelse(time_point==3, knowledge_overall, NA),
         attitude1 = ifelse(time_point==1, attitude_overall, NA),
         attitude2 = ifelse(time_point==2, attitude_overall, NA),
         attitude3 = ifelse(time_point==3, attitude_overall, NA),
         empathy1 = ifelse(time_point==1, empathy_score, NA),
         empathy2 = ifelse(time_point==2, empathy_score, NA),
         empathy3 = ifelse(time_point==3, empathy_score, NA),
         confidence1 = ifelse(time_point==1, confidence_score, NA),
         confidence2 = ifelse(time_point==2, confidence_score, NA),
         confidence3 = ifelse(time_point==3, confidence_score, NA),
         confidence1 = ifelse(time_point==1, confidence_score, NA),
         system_support_score1 = ifelse(time_point==1, system_support_score, NA),
         system_support_score2 = ifelse(time_point==2, system_support_score, NA),
         system_support_score3 = ifelse(time_point==3, system_support_score, NA),
         practice1 = ifelse(time_point==1, practice_score, NA),
         practice2 = ifelse(time_point==2, practice_score, NA),
         practice3 = ifelse(time_point==3, practice_score, NA),
  ) %>%
  select(-knowledge_overall, -attitude_overall, -empathy_score, -practice_score, 
       -confidence_score, -system_support_score)
         
# Convert to wide format  
analysis <- analysis %>%
    pivot_wider(names_from = time_point, values_from = .) %>%
    group_by(participant_id_3)

# Join attendance data
  
# Join demographics
  

# Write score data to folder
path_to_clean_rds_scores <- paste(gbv_project_wd, "/data/clean/gbv_data_scores.RDS", sep = "")
saveRDS(merged_scores, file = path_to_clean_rds_scores)
