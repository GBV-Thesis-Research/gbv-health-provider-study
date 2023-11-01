##################################################
## Project: GBV
## Script purpose: Plot for scores across 3 timepoints
## Date: 9-10-23
## Author: Susan Glenn
##################################################

#### WD SETUP ####
current_wd <- getwd()

library(stringr)
library(officedown)
library(officer) 
library(gtsummary)
library(flextable)

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

source(paste(gbv_project_wd, "/code/score.R", sep = ""))
source(paste(gbv_project_wd, "/code/table_1.R", sep = ""))
source(paste(gbv_project_wd, "/code/mel_report_analysis.R", sep = ""))


#### Lint current file ####
style_file(paste(gbv_project_wd, "/code/table_scores.R", sep = ""))

# Load cleaned data
path_to_clean_rds_scores <- paste(gbv_project_wd, "/data/clean/gbv_data_scores.RDS", sep = "")
clean_scores <- readRDS(path_to_clean_rds_scores)

result <- clean_scores %>%
  filter(status=="All three") %>%
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
                             labels = c("Baseline", "Post-intensive training", "Endline"))) %>%
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

# CREATE PLOT FOR MEAN SCORES ACROSS TIMEPOINTS ------------------------------------
mean_bar_plot <- ggplot(result, aes(fill=time_point, y=mean_score, x=score_variable)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Scores by Domain and Timepoint",
       x = "Assessment Domain",
       y = "Mean Score",
       fill = "Timepoint") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_brewer(palette = "Paired")

folder_path <- paste(gbv_project_wd, "/figures/", sep = "")
file_name <- "mean_scores_bar_chart.png"
ggsave(filename = file.path(folder_path, file_name), plot = mean_bar_plot, device = "png")

# TABLE OF MEAN SCORES FOR OUTCOME 4 AND OUTCOME 5 BY FACILITY
mean_scores_outcome_4_and_5 <- 
  clean_scores %>%
  select(-c(participant_id_3)) %>%
  filter(status == "All three") %>%
  group_by(region, time_point) %>%
  summarize(
    outcome4_mean_value = mean(outcome4_score, na.rm = TRUE),
    outcome5_mean_value = mean(outcome5_score, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  pivot_wider(names_from = time_point, values_from = c(outcome4_mean_value, outcome5_mean_value)) %>%
  mutate(
    outcome4_difference = outcome4_mean_value_3 - outcome4_mean_value_1,
    outcome5_difference = outcome5_mean_value_3 - outcome5_mean_value_1
  ) %>%
  select(region, outcome4_mean_value_1, outcome4_mean_value_3, outcome4_difference, outcome5_mean_value_1, outcome5_mean_value_3, outcome5_difference)

outcome_4_table <- 
  mean_scores_outcome_4_and_5 %>%
  select(region, starts_with("outcome4"))

outcome_5_table <- 
  mean_scores_outcome_4_and_5 %>%
  select(region, starts_with("outcome5"))

# CREATE PLOT FOR DIFFERENCE IN MEAN SCORES BY FACILITY ----------------------

diff_by_facility <-
  mean_scores_outcome_4_and_5 %>%
  select(region, outcome4_difference, outcome5_difference) %>%
  pivot_longer(cols = ends_with("difference"), names_to = "score_variable", 
               values_to = "percent_difference") 

diff_by_facility <- diff_by_facility %>%
  mutate(score_variable = recode(score_variable, "outcome4_difference" = "Outcome 4", "outcome5_difference" = "Outcome 5"))

# create plot of difference by facility
mean_diff_bar_plot <- ggplot(diff_by_facility, aes(fill=score_variable, y=percent_difference, x=region)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Score Differences by Facility",
       x = "Facility",
       y = "Score change in mean score",
       fill = "MEL Outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_brewer(palette = "Paired")

folder_path <- paste(gbv_project_wd, "/figures/", sep = "")
file_name <- "mean_diff_bar_plot.png"
ggsave(filename = file.path(folder_path, file_name), plot = mean_diff_bar_plot, device = "png")

# OUTCOME 4 TABLE
outcome4_table <- difference_by_facility %>%
  select(region, outcome4_mean_value_1, outcome4_mean_value_3, outcome4_difference) %>% 
  flextable()

outcome4_table <- set_caption(outcome4_table, "**Table 3. Outcome 4: Mean Provider Knowledge, Attitudes, and Empathy Score by Facility Catchment Area**")

outcome4_table <- set_header_labels(
  outcome4_table,
  region = "Catchment area",
  outcome4_mean_value_1 = "Baseline",
  outcome4_mean_value_3 = "Endline",
  outcome4_difference = "Difference in mean scores")

footer_values <- c("All Facilities", 
                   mean(difference_by_facility$outcome4_mean_value_1, na.rm = TRUE),
                   mean(difference_by_facility$outcome4_mean_value_3, na.rm = TRUE),
                   mean(difference_by_facility$outcome4_difference, na.rm = TRUE))

outcome4_table <- add_footer_row(outcome4_table, values = footer_values, colwidths = c(1,1,1,1))

temp_img_file <- tempfile(fileext = ".png")
outcome4_table %>%
  save_as_image(temp_img_file, width = 7, height = 3)
folder_path <- paste(gbv_project_wd, "/figures/", sep = "")
file_name <- "outcome4_table.png"
file.copy(temp_img_file, file.path(folder_path, file_name))
unlink(temp_img_file)

# OUTCOME 5 TABLE
outcome5_table <- difference_by_facility %>%
  select(region, outcome5_mean_value_1, outcome5_mean_value_3, outcome5_difference) %>%
  flextable()

outcome5_table <- set_caption(outcome5_table, "**Table 4. Outcome 5: Mean Provider Confidence Score by Facility Catchment Area**")

outcome5_table <- set_header_labels(
  outcome5_table,
  region = "Catchment area",
  outcome5_mean_value_1 = "Baseline",
  outcome5_mean_value_3 = "Endline",
  outcome5_difference = "Difference in mean scores")

footer_values <- c("All Facilities", 
                   mean(difference_by_facility$outcome5_mean_value_1, na.rm = TRUE),
                   mean(difference_by_facility$outcome5_mean_value_3, na.rm = TRUE),
                   mean(difference_by_facility$outcome5_difference, na.rm = TRUE))

outcome5_table <- add_footer_row(outcome5_table, values = footer_values, colwidths = c(1,1,1,1))

temp_img_file2 <- tempfile(fileext = ".png")
outcome5_table %>%
  save_as_image(temp_img_file2, width = 7, height = 3)
folder_path <- paste(gbv_project_wd, "/figures/", sep = "")
file_name <- "outcome5_table.png"
file.copy(temp_img_file2, file.path(folder_path, file_name))
unlink(temp_img_file2)

# OUTCOME 7 TABLE
pract19_clean_vars <- names(clean_data)[str_detect(names(clean_data), "practices_clean_19")]

outcome7pre <- clean_data %>%
  filter(status == "All three" & time_point == 1) %>%
  select(all_of(pract19_clean_vars)) %>%
  na.omit() %>%
  t() %>%
  as.data.frame() %>%
  mutate(percent_correct_pre = (rowSums(.))/6*100) %>%
  select(percent_correct_pre)
outcome7pre$question <- rownames(outcome7pre)

outcome7post <- clean_data %>%
  filter(status == "All three") %>%
  filter(time_point == 3) %>%
  select(all_of(pract19_clean_vars)) %>%
  na.omit() %>%
  t() %>%
  as.data.frame() %>%
  mutate(percent_correct_post = (rowSums(.))/16*100) %>%
  select(percent_correct_post)
outcome7post$question <- rownames(outcome7post)

outcome7_pre_post <- merge(outcome7pre, outcome7post, by = "question", all = TRUE) %>%
  mutate(difference = percent_correct_post - percent_correct_pre) %>% 
  mutate(question = factor(question, levels = c("practices_clean_19a", 
                                                "practices_clean_19b",
                                                "practices_clean_19c",
                                                "practices_clean_19d",
                                                "practices_clean_19e",
                                                "practices_clean_19f",
                                                "practices_clean_19g",
                                                "practices_clean_19h",
                                                "practices_clean_19i"),
                           labels = c("Assessed the immediate level of danger for the woman",
                                      "Discussed the options she may have",
                                      "Documented domestic violence history and physical examination findings",
                                      "Helped the woman to create a plan to increase her and her children’s safety",
                                      "Offered validating and supportive statements",
                                      "Provided basic information about domestic violence",
                                      "Provided education or resource materials about domestic violence",
                                      "Referred the woman to support services in the community",
                                      "Talked to the woman about her needs")))

numeric_columns <- sapply(outcome7_pre_post, is.numeric)
outcome7_pre_post[numeric_columns] <- round(outcome7_pre_post[numeric_columns], digits = 2)

outcome7_table <- flextable(outcome7_pre_post)

outcome7_table <- set_header_labels(
  outcome7_table,
  question = "Action Taken",
  percent_correct_pre = "Baseline",
  percent_correct_post = "Endline",
  difference = "Percentage point difference")

outcome7_table <- set_caption(outcome7_table, "**Table 5. Outcome 7: Provider Practices in Identifying and Caring for Victims of GBV**")

folder_path <- paste(gbv_project_wd, "/figures/", sep = "")
file_name <- "outcome7_table.png"
ggsave(filename = file.path(folder_path, file_name), plot = outcome7_table, device = "png")

# LEARNING LABS TABLE
clean_scores <- clean_scores %>%
  mutate(timepoint_factored = factor(time_point,
                                     levels = c(2, 3),
                                     labels = c("Post-intensive Training", "Endline")))

learning_labs_table <-
  clean_scores %>%
  filter(status == "All three") %>%  
  filter(time_point != 1) %>%
  tbl_summary(
    include = c("outcome4_score", "outcome5_score", "timepoint_factored"), 
    by = c(timepoint_factored),
    type = c(
      outcome4_score, outcome5_score
    ) ~ "continuous",
    label = list(
      outcome4_score ~ "Outcome 4 Score",
      outcome5_score ~ "Outcome 5 Score"
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})"
    ),
    digits = all_continuous() ~ 2
  ) %>%
  add_p() %>%
  bold_p()

learning_labs_table <- learning_labs_table %>%
  modify_caption("**Table 6. Outcome 4 & 5 Scores from Post-intensive Training to Endline**")

learning_labs_table