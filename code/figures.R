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

result <- clean_scores %>%
  group_by(time_point) %>%
  summarize(knowledge_general_score_mean = mean(knowledge_general_score, na.rm = TRUE), 
            knowledge_warning_score_mean = mean(knowledge_warning_score, na.rm = TRUE), 
            knowledge_helpful_score_mean = mean(knowledge_helpful_score, na.rm = TRUE), 
            knowledge_appropriate_score_mean = mean(knowledge_appropriate_score, na.rm = TRUE),
            attitude_general_score_mean = mean(attitude_general_score, na.rm = TRUE), 
            attitude_acceptability_score_mean = mean(attitude_acceptability_score, na.rm = TRUE), 
            attitude_genderroles_score_mean = mean(attitude_genderroles_score, na.rm = TRUE), 
            empathy_score_mean = mean(empathy_score, na.rm = TRUE), 
            system_support_score_mean = mean(system_support_score, na.rm = TRUE), 
            confidence_score_mean = mean(confidence_score, na.rm = TRUE), 
            practice_score_mean = mean(practice_score, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  pivot_longer(cols = ends_with("_mean"), names_to = "score_variable", 
               values_to = "mean_score") %>%
  mutate(time_point = factor(time_point, levels = c(1, 2, 3), 
                             labels = c("Timepoint 1", "Timepoint 2", "Timepoint 3"))) %>%
  mutate(score_variable = factor(score_variable, levels = c("knowledge_general_score_mean", 
                                                        "knowledge_warning_score_mean",
                                                        "knowledge_appropriate_score_mean",
                                                        "knowledge_helpful_score_mean",
                                                        "attitude_general_score_mean",
                                                        "attitude_acceptability_score_mean",
                                                        "attitude_genderroles_score_mean",
                                                        "empathy_score_mean",
                                                        "confidence_score_mean",
                                                        "practice_score_mean",
                                                        "system_support_score_mean"),
                              labels = c("General Knowledge", "Warning Signs", 
                                         "Appropriate inquiry", 
                                         "Helpful responses",
                                         "General attitudes", "GBV unacceptability",
                                         "Gender roles", "Provider empathy",
                                         "Provider confidence", "Provider practices",
                                         "System support")))


# CREATE PLOT FOR SCORES ACROSS TIMEPOINTS ------------------------------------
mean_bar_plot <- ggplot(result, aes(fill=time_point, y=mean_score, x=score_variable)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Scores by Domain for Each Timepoint",
       x = "Domain",
       y = "Mean Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_brewer(palette = "Paired")

folder_path <- paste(gbv_project_wd, "/figures/", sep = "")
file_name <- "mean_scores_bar_chart.png"
ggsave(filename = file.path(folder_path, file_name), plot = mean_bar_plot, device = "png")