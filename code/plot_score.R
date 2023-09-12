##################################################
## Project: GBV
## Script purpose: Plot for scores across 3 timepoints
## Date: 9-10-23
## Author: Susan Glenn
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

# CREATE MEAN SCORES FOR EACH DOMAIN-------------------------------------------
mean_pre <- clean_data_scores %>%
  filter(time_point == 1) %>%
  summarize(
    knowledge_general_score = mean(knowledge_general_score),
    knowledge_warning_score = mean(knowledge_warning_score),
    knowledge_helpful_score = mean(knowledge_helpful_score),
    knowledge_appropriate_score = mean(knowledge_appropriate_score),
    attitude_general_score = mean(attitude_general_score),
    attitude_acceptability_score = mean(attitude_acceptability_score),
    attitude_genderroles_score = mean(attitude_genderroles_score),
    empathy_score = mean(empathy_score),
    system_support_score = mean(system_support_score),
    confidence_score = mean(confidence_score),
    practice_score = mean(practice_score, na.rm=T)
  )

mean_pre <- mean_pre %>%
  gather(key = "domain", value = "mean_score")

mean_pre$timepoint <- c("Baseline")

mean_post <- clean_data_scores %>%
  filter(time_point == 2) %>%
  summarize(
    knowledge_general_score = mean(knowledge_general_score),
    knowledge_warning_score = mean(knowledge_warning_score),
    knowledge_helpful_score = mean(knowledge_helpful_score),
    knowledge_appropriate_score = mean(knowledge_appropriate_score),
    attitude_general_score = mean(attitude_general_score),
    attitude_acceptability_score = mean(attitude_acceptability_score),
    attitude_genderroles_score = mean(attitude_genderroles_score),
    empathy_score = mean(empathy_score),
    system_support_score = mean(system_support_score),
    confidence_score = mean(confidence_score),
    practice_score = mean(practice_score, na.rm=T)
  )

mean_post <- mean_post %>%
  gather(key = "domain", value = "mean_score")

mean_post$timepoint <- c("Post-intensive training")

mean_fuat <- clean_data_scores %>%
  filter(time_point == 3) %>%
  summarize(
    knowledge_general_score = mean(knowledge_general_score),
    knowledge_warning_score = mean(knowledge_warning_score),
    knowledge_helpful_score = mean(knowledge_helpful_score),
    knowledge_appropriate_score = mean(knowledge_appropriate_score),
    attitude_general_score = mean(attitude_general_score),
    attitude_acceptability_score = mean(attitude_acceptability_score),
    attitude_genderroles_score = mean(attitude_genderroles_score),
    empathy_score = mean(empathy_score),
    system_support_score = mean(system_support_score),
    confidence_score = mean(confidence_score),
    practice_score = mean(practice_score, na.rm=T)
  )

mean_fuat <- mean_fuat %>%
  gather(key = "domain", value = "mean_score")

mean_fuat$timepoint <- c("Endline")

# Bind all mean scores into one dataframe
combined_mean_scores <- rbind(mean_pre, mean_post, mean_fuat)

# CREATE PLOT FOR SCORES ACROSS TIMEPOINTS ------------------------------------
mean_bar_plot <- ggplot(combined_mean_scores, aes(domain, mean_score, fill = timepoint)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title="Mean scores by domain and timepoint", x = "Domain", y = "Mean score out of 100",
       fill = "Timepoint")

mean_bar_plot

mean_bar_plot <- mean_bar_plot +
  scale_x_discrete(labels = c("Acceptable attitudes toward GBV", "Attitude towards gender roles", 
                              "General attitudes toward GBV", "Confidence", "Empathy", "Appropriate Knowledge", 
                              "General knowledge", "Knowledge of helpful responses to GBV", "Knowledge of warning signs",
                              "Helpful practices", "Empathy"), guide = "Domain")